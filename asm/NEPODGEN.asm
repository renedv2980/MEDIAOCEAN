*          DATA SET NEPODGEN   AT LEVEL 160 AS OF 02/28/20                      
*PHASE T00A54B                                                                  
NEPODGEN TITLE 'T00A54 - RESEARCH WRITER GENERAL ROUTINES'                      
         PRINT NOGEN                                                            
T00A54   RSECT                                                                  
         REQUS                                                                  
         USING *,RF                                                             
         DS    16384X              RESERVE 16K                                  
         ORG   T00A54                                                           
GEN      NTR1                                                                   
         DROP  RF                                                               
         LR    RB,RF                                                            
         USING T00A54,RB,RA,R7,R6                                               
         B     *+12                                                             
         DC    CL8'**GEN***'                                                    
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         LA    R7,2048(RA)                                                      
         LA    R7,2048(R7)                                                      
         LA    R6,2048(R7)                                                      
         LA    R6,2048(R6)                                                      
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         USING GETTXTD,GETTXTCB                                                 
         USING PODBD,PODBOOK                                                    
*                                                                               
         LA    RE,RELOC                                                         
         S     RE,RELOC                                                         
         ST    RE,PGNR                                                          
*                                                                               
         MVC   DUMPDBA,=C'**DBA***'                                             
*                                                                               
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     VBRANCH(RF)                                                      
*                                                                               
VBRANCH  B     VVALUSER                                                         
         B     VVALMED                                                          
         B     VVALCLT                                                          
         B     VVALBOOK                                                         
         B     VPDSERR                                                          
         B     VVALDEMO                                                         
         B     VVALNET                                                          
         B     VVALDYTM                                                         
         B     VVPRGHUT                                                         
         B     VVALPROG                                                         
         B     VVALOPTS                                                         
         B     VVALTITS                                                         
         B     VVALFILT                                                         
         B     VCHEFILT                                                         
*                                                                               
         B     VVALLEFT                                                         
         B     VVALRGHT                                                         
         B     VVALMID                                                          
         B     VVALROWS                                                         
         B     VVALCOLS                                                         
         B     VVALPEX                                                          
*                                                                               
         B     VINTDRON                                                         
         B     VVRWDRON                                                         
         B     VGRWDRON                                                         
         B     VVCLDRON                                                         
         B     VGCLDRON                                                         
         B     VVCMDRON                                                         
         B     VGCMDRON                                                         
         B     VGUSDRON                                                         
         B     VWRPDRON                                                         
*                                                                               
         B     VINTDRIV                                                         
*                                                                               
         B     VGENHEAD                                                         
*                                                                               
         B     VGENEDCT                                                         
*                                                                               
         B     VNUMERIC                                                         
         B     VPACK                                                            
         B     VCURSERR                                                         
         B     VERRXIT                                                          
         B     VEXIT                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE USER AGENCY                                         *         
***********************************************************************         
*                                                                               
VVALUSER MVC   PDQAGY,AGENCY                                                    
         MVC   AGYSIGN,SPACES                                                   
         XC    AGYALPHA,AGYALPHA                                                
         MVI   AGYNUM,X'FF'        ASSUME NOT NUMERIC                           
*                                  AGENCY NAME & ADDRESS FROM ID REC.           
         MVC   FILENAME,=CL8'CTFILE'                                            
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         L     R1,ATWA                                                          
         MVC   CTIKID+8(2),10(R1)                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R4,AIO                                                           
         LA    R4,CTIDATA                                                       
         SR    R3,R3                                                            
*                                                                               
VUSE02   CLI   0(R4),0                                                          
         BE    VUSE14                                                           
         CLI   0(R4),X'30'                                                      
         BE    VUSE06                                                           
         CLI   0(R4),X'02'                                                      
         BE    VUSE08                                                           
         CLI   0(R4),X'06'                                                      
         BE    VUSE10                                                           
         CLI   0(R4),X'21'         SYSTEM AUTHORIZATION ELEMENT                 
         BE    VUSE12                                                           
*                                                                               
VUSE04   IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     VUSE02                                                           
*                                                                               
         USING CTDSTD,R4                                                        
VUSE06   MVC   USERNAME,CTDSTNAM                                                
         MVC   USERADDR,CTDSTADD                                                
         B     VUSE04                                                           
*                                                                               
         USING CTDSCD,R4                                                        
VUSE08   MVC   AGYSIGN,CTDSC                                                    
         MVC   WORK(3),AGYSIGN+2                                                
         NC    WORK(3),=X'F0F0F0'                                               
         CLC   WORK(3),=X'F0F0F0'                                               
         BNE   VUSE04                                                           
         MVC   AGYNUM,AGYSIGN+2                                                 
         B     VUSE04                                                           
*                                                                               
         USING CTAGYD,R4                                                        
VUSE10   MVC   AGYALPHA,CTAGYID                                                 
         B     VUSE04                                                           
*                                                                               
         USING CTSYSD,R4                                                        
VUSE12   CLI   CTSYSNUM,2          SPOT                                         
         BNE   VUSE04                                                           
         MVC   PDSSENUM,CTSYSSE    COPY SE NUMBER                               
         B     VUSE04                                                           
*                                                                               
VUSE14   XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
         GOTO1 GETFACT,DMCB,0                                                   
         L     RF,DMCB                                                          
         LA    R4,FACTWRK                                                       
         MVC   0(80,R4),0(RF)                                                   
         USING FACTSD,R4                                                        
         MVC   PDSYSTEM,FASYS                                                   
         MVC   PDOVSYS,FAOVSYS                                                  
         MVC   LNDEMCOD,=H'3'      3 CHAR DEMO CODES                            
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         CLI   PDOVSYS,2           SPOT SYSTEM?                                 
         BNE   *+14                                                             
         MVC   RCPROG(2),=C'SP'    PREFIX FOR REPORT                            
         B     VUSE16                                                           
         CLI   PDOVSYS,3           NET SYSTEM?                                  
         BNE   *+20                                                             
         MVC   RCPROG(2),=C'NE'    PREFIX FOR REPORT                            
         MVC   LNDEMCOD,=H'4'      NETWORK USES 4 CHAR DEMO CODES               
         B     VUSE16                                                           
         CLI   PDOVSYS,8           REP SYSTEM?                                  
         BNE   *+10                                                             
         MVC   RCPROG(2),=C'RE'    PREFIX FOR REPORT                            
*                                                                               
VUSE16   DS    0C                                                               
         MVI   OFFLINE,C'N'                                                     
         TM    FATFLAG,X'01'                                                    
         BNO   VUSEX                                                            
         MVI   OFFLINE,C'Y'                                                     
*                                                                               
VUSEX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE MEDIA                                               *         
***********************************************************************         
*                                                                               
VVALMED  XC    KEY,KEY             GET AGENCY RECORD                            
*                                                                               
         LA    R4,KEY                                                           
         USING AGYHDRD,R4                                                       
*                                                                               
         CLI   PDOVSYS,8           REP, FORGET ABOUT                            
         BE    VMED01                                                           
         CLI   PDOVSYS,2           SPOT, OPEN REP FILES                         
         BNE   VMED06                                                           
         CLI   OFFLINE,C'Y'        ARE WE OFFLINE?                              
         BNE   VMED06                                                           
         L     R1,=A(RFLIST)       REP FILE LIST                                
         A     R1,PGNR                                                          
         ST    R1,DMCB+8                                                        
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP ',,AIO1                           
         B     VMED06                                                           
*                                                                               
*                                                                               
* --- HAVE TO CHECK IF OFFLINE, IF SO, OPEN FILE                                
* HAVE TO SWITCH SYSTEMS FOR REP                                                
VMED01   CLI   OFFLINE,C'Y'        ARE WE OFFLINE?                              
         BNE   VMED04              NO, COULD USE SWITCH                         
         CLI   PDSSENUM,0          NEED A SE NUM TO SWITCH                      
         BNZ   VMED02                GOOD, USE SIDS                             
         MVI   PDSIDOPT,C'X'                                                    
         B     VMEDX                                                            
*                                                                               
VMED02   ICM   RF,15,PDAUTL                                                     
         MVC   4(1,RF),PDSSENUM    SPOT SE NUM                                  
                                                                                
         L     R1,=A(FLIST)                                                     
         A     R1,PGNR                                                          
         ST    R1,DMCB+8                                                        
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'SPOT',,AIO1                           
         B     VMED06                                                           
*                                                                               
VMED04   GOTO1 SWITCH,DMCB,(0,=C'SPT'),0      SWITCH TO SPOT                    
         CLI   4(R1),0                                                          
         BZ    VMED06              NEED IT TO GET AGYMED FOR SIDS               
         MVI   PDSIDOPT,C'X'       CAN'T USE SIDS                               
         CLI   4(R1),2             SWITCHED BUT NOT OPENED?                     
         BNE   VMEDX                                                            
         GOTO1 SWITCH,DMCB,(PDSYSTEM,0),0   SWITCH BACK                         
         CLI   4(R1),0                                                          
         BE    VMEDX                                                            
         DC    H'0'                                                             
*                                                                               
* --- HAVE TO SET A FLAG FOR SID AVAILABILITY                                   
VMED06   MVI   PDSIDOPT,C'N'                                                    
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         LA    R2,KEY+28                                                        
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   PDQAGY,AGENCY                                                    
         MVC   PDAPROF,AGYPROF     SAVE AGENCY PROFILE                          
         LR    R5,R4                                                            
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         B     VMED08+4                                                         
*                                                                               
VMED08   BAS   RE,NEXTEL                                                        
         BNE   BADMED                                                           
         CLI   2(R5),C'N'          NETWORK TV?                                  
         BE    VMED09                                                           
         CLI   2(R5),C'T'          SPOT TV?                                     
         BNE   VMED08                                                           
*                                  MAKE SURE CORRECT ELEM FOR THE MEDIA         
VMED09   CLI   PDOVSYS,3           FOR NET,                                     
         BNE   VMED09A                                                          
         CLI   2(R5),C'N'          NEED NETWORK TV                              
         BNE   VMED08                                                           
         B     VMED10                                                           
VMED09A  CLI   PDOVSYS,2           FOR SPOT,                                    
         BNE   VMED10                                                           
         CLI   2(R5),C'T'          NEED SPOT TV                                 
         BNE   VMED08                                                           
*                                                                               
VMED10   MVC   PDBAGYMD,3(R5)      AGENCY/MEDIA CODE                            
         CLI   PDOVSYS,8           HAVE TO SWITCH BACK TO REP                   
         BNE   VMEDX                                                            
         CLI   OFFLINE,C'Y'        RETURN TO ORIGINAL SYSTEM                    
         BNE   VMED12                                                           
         ICM   RF,15,PDAUTL                                                     
         MVC   4(1,RF),PDSYSTEM                                                 
         B     VMEDX                                                            
*                                                                               
VMED12   GOTO1 SWITCH,DMCB,(PDSYSTEM,0),0                                       
         CLI   4(R1),0                                                          
         BZ    VMEDX                                                            
         DC    H'0'                                                             
*                                                                               
VMEDX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE CLIENT                                              *         
***********************************************************************         
*                                                                               
VVALCLT  GOTO1 ANY                                                              
*                                                                               
VCLIX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE BOOK                                                *         
*                 INPUT:  A(INPUT SCREEN)                             *         
*                 OUTPUT: PODBOOK - LIST OF BOOKS                     *         
***********************************************************************         
*                                                                               
VVALBOOK DS    0H                                                               
         GOTO1 =A(VALBK),DMCB,(R2),(R9),(RC),RR=PGNR                            
         L     RE,APODBKL                                                       
         MVC   PODBD(PODBLNQ),0(RE)                                             
*                                                                               
VBOOKX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE DEMO                                                *         
***********************************************************************         
*                                                                               
VVALDEMO GOTO1 ANY                                                              
         LA    RE,BLOCK                                                         
         LA    RF,480                                                           
         XCEFL                                                                  
*                                                                               
*        GOTO1 SCANNER,PARAS,(R2),(12,BLOCK),C',=,.'                            
         GOTO1 SCANNER,PARAS,(R2),('PDNSCRD',BLOCK),C',=,.'                     
*                                   MAX NO OF DEMOS ON ONE SCREEN               
         CLI   4(R1),0                                                          
         BE    BADDEM              ERRORS                                       
*                                                                               
*--IF SOURCE IS NTI OR P NAD DEMOS ARE INVALID EX. (67.RWMN1224)                
         L     RE,APODBKL                                                       
         ZIC   RF,PODBKNUM                                                      
         LTR   RF,RF                                                            
         BZ    BADDEM                                                           
*                                                                               
VDEM02   CLC   8(3,RE),=CL3'NAD'   NAD,NHI,CNAD,CNAW                            
         BE    VDEM10                                                           
         CLC   1(3,RE),=CL3'NAW'                                                
         BE    VDEM10                                                           
         CLC   1(3,RE),=CL3'OPI'                                                
         BE    VDEM10                                                           
         CLC   1(3,RE),=CL3'IAG'                                                
         BE    VDEM10                                                           
         CLC   1(3,RE),=CL3'NHW'                                                
         BE    VDEM10                                                           
         CLC   1(3,RE),=CL3'HPW'   NHW FROM NATIONAL SAMPLE                     
         BE    VDEM10                                                           
         CLC   1(3,RE),=CL3'TCA'   TCAR                                         
         BE    VDEM10                                                           
         CLC   1(3,RE),=CL3'WB1'   TCAR                                         
         BE    VDEM10                                                           
         CLC   1(5,RE),=CL5'ACMWB' TCAR WB1 ACM                                 
         BE    VDEM10                                                           
         CLC   1(3,RE),=CL3'MPA'                                                
         BE    VDEM10                                                           
         CLC   8(3,RE),=CL3'TP'    THIS COVERS TP,T4 AND DPT                    
         BE    VDEM10                                                           
         CLC   8(3,RE),=CL3'CTP'   THIS COVERS COUNTY COVERAGE                  
         BE    VDEM10                                                           
         CLC   1(3,RE),=CL3'PAV'                                                
         BE    VDEM10                                                           
         CLC   1(3,RE),=CL3'IUN'                                                
         BE    VDEM10                                                           
         CLC   1(3,RE),=CL3'PIV'                                                
         BNE   VDEM04                                                           
         MVI   PODNADBK,X'FF'                                                   
         B     VDEM10                                                           
*                                                                               
VDEM04   CLC   1(3,RE),=CL3'CSI'                                                
         BE    VDEM10                                                           
         CLC   1(3,RE),=CL3'BBM'                                                
         BE    VDEM10                                                           
         CLC   1(3,RE),=CL3'SRC'                                                
         BE    VDEM10                                                           
         LA    RE,PODBLNQ(RE)                                                   
         CLI   0(RE),X'FF'                                                      
         BNE   VDEM02                                                           
*                                                                               
         LA    RE,BLOCK                                                         
         LA    RF,PDNSCRD          MAX NO OF DEMOS ON ONE SCREE                 
         MVI   FIELDERR,1                                                       
*                                                                               
VDEM06   CLI   0(RE),0                                                          
         BE    VDEM10                                                           
         CLI   1(RE),0             IS DEMO IN NAD FORMAT                        
         BE    VDEM08              NO BYPASS                                    
         CLI   7(RE),171           USER DEMO TVQ                                
         BE    VDEM10                                                           
         CLI   7(RE),172           USER DEMO OPI                                
         BE    VDEM10                                                           
         CLI   7(RE),175           USER DEMO IAG ORIGINAL                       
         BE    VDEM10                                                           
         CLI   7(RE),176           USER DEMO IAG REPEAT                         
         BE    VDEM10                                                           
         CLI   7(RE),177           USER DEMO IAG AUTOMATIC O/R                  
         BE    VDEM10                                                           
         TM    2(RE),X'80'         IS THERE NUMERIC PREFIX                      
         BNZ   BADDEM              YES ERROR                                    
*                                                                               
VDEM08   LA    RE,32(RE)                                                        
         AI    FIELDERR,1                                                       
         BCT   RF,VDEM06                                                        
*                                                                               
*--IF BOOK PRIOR TO SEP/88 THEN C'O'(TP PUT)                                    
*--OR A C'Q'(TP SHARE)                                                          
VDEM10   BRAS  RE,CKNADOK                                                       
         BNE   BADDEM                                                           
         MVI   FIELDERR,1                                                       
         TM    PODBKOPT,X'80'      IS THERE A BOOK PRIOR TO SEP/87              
         BZ    VDEM14              NO                                           
*                                                                               
         LA    RE,BLOCK                                                         
*        LA    RF,12                                                            
         LA    RF,PDNSCRD      MAX NO OF DEMOS ON ONE SCREEN                    
*                                                                               
VDEM12   CLI   0(RE),0                                                          
         BE    VDEM14                                                           
         CLI   12(RE),C'O'                                                      
         BE    BADDEM                                                           
         CLI   12(RE),C'Q'                                                      
         BE    BADDEM                                                           
         CLI   22(RE),C'O'                                                      
         BE    BADDEM                                                           
         CLI   22(RE),C'Q'                                                      
         BE    BADDEM                                                           
         LA    RE,32(RE)                                                        
         AI    FIELDERR,1                                                       
         BCT   RF,VDEM12                                                        
*                                                                               
VDEM14   L     R4,PDADEMTB                                                      
         USING PDDMBUFF,R4         DEMO BUFFER                                  
         LA    R5,DBLOCKA                                                       
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         CLI   PODBMED,C'C'                                                     
         BNE   *+8                                                              
         MVI   DBSELMED,C'C'                                                    
         CLI   PDOVSYS,3           FOR NET                                      
         BNE   *+8                                                              
         MVI   DBSELMED,C'N'       SET SELMED TO N                              
         MVC   DBFILE,PODBFIL                                                   
         CLC   =C'OPI',PODBEXT     EDIT OPI MODIFIERS                           
         BE    *+14                                                             
         CLC   =C'IAG',PODBEXT     EDIT OPI MODIFIERS                           
         BNE   *+10                                                             
         MVC   DBFILE,PODBEXT                                                   
*                                                                               
         ST    R5,PARAS+8                                                       
         CLI   PDOVSYS,2                                                        
         BNE   *+8                                                              
         MVI   PARAS+8,C'S'        SPOTPAK CALL                                 
         CLI   PDOVSYS,3           ONLY NET CAN USE 4 BYTE DEMOS                
         BNE   *+8                                                              
         MVI   DBDEMTYP,C'4'                                                    
*                                                                               
         LA    R3,PODDEMWK                                                      
         LHI   R5,PDNSCRD          MAX NO OF DEMOS ON A SCREEN                  
*                                                                               
         TM    SCR2IND,SCR2YES     PROCESSING 2ND SCREEN                        
         BNO   VDEM15                                                           
         LHI   R5,PDNDEMS          TOTAL NO OF DEMOS ALLOWED                    
         ZIC   R1,ACTUAL           NO OF DEMOS ON 1ST SCREEN                    
         SR    R5,R1                                                            
         LTR   R5,R5                                                            
         BM    BADDEM              TOO MANY DEMOS                               
         CHI   R5,PDNSCRD          MAX ALLOWED ON ONE SCREEN                    
         BNH   *+8                                                              
         LHI   R5,PDNSCRD                                                       
*                                                                               
         CLI   0(R3),X'FF'         START AFTER ALL THE OTHER DEMOS              
         BE    VDEM15                                                           
         AH    R3,LNDEMCOD                                                      
         B     *-12                                                             
*                                                                               
VDEM15   BRAS  RE,GETTOKEN         GET COMSCORE LICENSE                         
*                                                                               
         GOTO1 DEMOVAL,PARAS,(NFLDS,(R2)),((R5),(R3)),,0,,APDNTDMS              
         MVC   FIELDERR,0(R1)                                                   
         CLI   4(R1),0                                                          
         BE    BADDEM                                                           
*                                                                               
         TM    SCR2IND,SCR2YES     NOW PROCESSING 2ND SCREEN                    
         BO    *+20                                                             
         MVC   ACTUAL,4(R1)        PASS USER BACK NUMBER FOUND                  
         MVC   NDEMS1,ACTUAL       NO OF COLUMNS ON FIRST SCREEN                
         B     VDEM15A                                                          
*                                                                               
         ZIC   R0,ACTUAL           ADD NO OF DEMOS TO NO FROM 1ST REPRT         
         ZIC   RE,4(R1)                                                         
         AR    R0,RE                                                            
         STC   R0,ACTUAL                                                        
*                                                                               
VDEM15A  MVC   PODDMWLN,ACTUAL                                                  
         MVC   PODDMNUM,ACTUAL                                                  
         MVI   PODDMENT,1                                                       
*                                                                               
* CALL DEMOCON TO DECODE THE ENCODED MODIFIERS FOR PERSONAL LANGUAGE            
* DEMOS. AFTER THIS CALL, PODDEMWK WILL HOLD THE DECODED DEMO LIST, AND         
* PODDEWPL WILL HOLD THE ARRAY OF PERSONAL LANGUAGE ATTRIBUTES.                 
         GOTO1 DEMOCON,PARAS,(PODDMWLN,PODDEMWK),                      +        
               ('DEMOCON_16',PODDEMWK),,PODDEWPL                                
*                                                                               
         ZIC   RE,PODDMWLN                                                      
         LA    RF,PODDEMWK                                                      
*                                                                               
VDEM16   CLI   0(RF),0                                                          
         BNE   *+8                                                              
         MVI   0(RF),X'01'                                                      
         AH    RF,LNDEMCOD                                                      
         BCT   RE,VDEM16                                                        
*                                                                               
         MVI   0(RF),X'FF'         PUT END OF TABLE MARK                        
         MVC   PODDEMO(L'PODDEMWK),PODDEMWK                                     
         MVC   PODDEMPL(L'PODDEWPL),PODDEWPL PERSONAL LANGUAGE ATTRIBTS         
*                                                                               
         L     RE,APODBKL                                                       
         ZIC   RF,PODBKNUM                                                      
*                                                                               
VDEM18   CLI   0(RE),X'FF'                                                      
         BE    VDEM30                                                           
         CLC   1(3,RE),=CL3'NMI'                                                
         BE    VDEM20                                                           
         CLC   1(3,RE),=CL3'EMI'                                                
         BE    VDEM20                                                           
         LA    RE,PODBLNQ(RE)                                                   
         B     VDEM18                                                           
*                                                                               
VDEM20   LA    RF,PODDEMO                                                       
*                                                                               
VDEM22   CLI   0(RF),X'FF'                                                      
         BE    VDEM30                                                           
         CLI   1(RF),C'T'                                                       
         BE    VDEM24                                                           
         CLI   1(RF),C'R'                                                       
         BNE   BADDEM2                                                          
*                                                                               
VDEM24   AH    RF,LNDEMCOD                                                      
         B     VDEM22                                                           
*                                                                               
VDEM30   TM    OPTFLAG1,COMSRCQ    COM SOURCE REQUESTED?                        
         BZ    VDEMX                                                            
         LA    RF,PODDEMO                                                       
VDEM32   CLI   0(RF),X'FF'                                                      
         BE    VDEMX                                                            
         OC    1(2,RF),1(RF)       COMSCORE DEMO?                               
         BNZ   VDEM34                                                           
         OI    OPTFLAG1,COMDEMOQ   COMSCORE DEMO REQUESTED                      
         MVI   REQSML,C'C'         SET SOON TO COMSCORE (SPOOKQCS)              
         B     VDEMX                                                            
VDEM34   AHI   RF,4                GET NEXT DEMO                                
         B     VDEM32                                                           
*                                                                               
VDEMX    B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL VALDEM ROUTINES                                   *         
***********************************************************************         
***********************************************************************         
*        VALIDATE NETWORK                                             *         
***********************************************************************         
*                                                                               
VVALNET  DS    0H                                                               
         GOTO1 =A(VALNT),DMCB,(R2),(R9),(RC),RR=PGNR                            
         B     XIT                                                              
***********************************************************************         
*        VALIDATE DAY/TIME                                            *         
***********************************************************************         
*                                                                               
VVALDYTM GOTO1 ANY                                                              
         MVI   NADUSE,C'N'         NAD NOT USED                                 
         L     R5,APODBKL          CHECK IF NAD USED                            
         ST    R2,SAVER2           R2 GETS REUSED HERE                          
         MVI   FIELDERR,0                                                       
*                                                                               
VDAY02   CLI   0(R5),X'FF'         END OF BOOKS?                                
         BE    VDAY06                                                           
         CLC   =C'HPM-D',1(R5)     WAS HPM-D USED?                              
         BE    *+10                  .....OR                                    
         CLC   =C'NHT-D',1(R5)     WAS NHT-D USED?                              
         BE    *+10                  .....OR                                    
         CLC   =C'NAD-D',1(R5)     WAS NAD-D USED?                              
         BNE   VDAY04              NO, NEXT BOOK                                
         MVI   NADUSE,C'Y'                                                      
         B     VDAY06                                                           
*                                                                               
VDAY04   LA    R5,PODBLNQ(R5)                                                   
         B     VDAY02                                                           
*                                                                               
VDAY06   XC    PODDAYTM,PODDAYTM                                                
         LA    R4,PODDAYTM                                                      
         LA    RE,BLOCK                                                         
         LA    RF,480                                                           
         XCEFL                                                                  
         XC    HALF,HALF           USE HALF TO STORE PREV & CURR DAY            
*                                                                               
         GOTO1 SCANNER,PARAS,(20,(R2)),(10,BLOCK),C',=,/'                       
         CLI   4(R1),0                                                          
         BE    BADDAY                                                           
         LA    R5,BLOCK                                                         
*                                                                               
         CLI   0(R5),0             END OF TABLE                                 
         BE    VDAY30              YES EXIT                                     
*                                                                               
VDAY08   MVI   BYTE,2              ASSUME FIELD IS DIVIDED                      
         MVI   0(R4),X'FF'         CHECK FOR 'ALL'                              
         MVI   HALF+1,X'FF'        HOLD ONTO THE POTENTIAL CURRENT DAY          
         ZIC   RE,FIELDERR                                                      
         LA    RE,1(RE)            UPDATE CURSOR POSITION                       
         STC   RE,FIELDERR                                                      
         CLC   12(4,R5),=C'DALL'                                                
         BE    VDAY24              BUMP BLOCK                                   
         CLI   NADUSE,C'Y'                                                      
         BE    BADDAY                                                           
*                                                                               
*--CHECK FOR DAY TIME MACRO DEFINITIONS                                         
*                                                                               
         L     R3,=A(DYTMMAC)                                                   
*                                                                               
         L     RF,APODBKL          IF SOURCE MXM WAS REQUESTED,                 
VDAY09   CLI   0(RF),X'FF'         USE EXACT MINUTE DEFINITIONS OF              
         BE    VDAY10              MACROS.                                      
         CLC   1(3,RF),=C'MXM'                                                  
         BE    VDAY09A                                                          
         LA    RF,PODBLNQ(RF)      CHECK EVERY SOURCE IN THE REQUEST            
         B     VDAY09                                                           
VDAY09A  L     R3,=A(DYTMMACM)                                                  
*                                                                               
VDAY10   A     R3,PGNR                                                          
VDAY10A  CLC   0(6,R3),12(R5)                                                   
         BE    VDAY12                                                           
         LA    R3,17(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BE    VDAY16                                                           
         B     VDAY10A                                                          
*                                                                               
*---LOAD IN MACRO DAY AND TIME                                                  
VDAY12   L     RE,APODNET                                                       
VDAY12A  OC    0(3,RE),0(RE)       FOR ITN DON'T ALLOW FILTERS ON TIME          
         BZ    VDAY13                                                           
         CLI   0(RE),X'FF'                                                      
         BE    VDAY13                                                           
         CLC   =C'SIN S',0(RE)                                                  
         BE    BADTIME4                                                         
         CLC   =C'ITN S',0(RE)                                                  
         BE    BADTIME3                                                         
         LA    RE,PODNETL(RE)                                                   
         B     VDAY12A                                                          
VDAY13   MVC   0(10,R4),6(R3)                                                   
         CLC   0(5,R3),=C'PRIME'                                                
         BNE   VDAY14                                                           
*                                                                               
* OPUP JUN 18/09 :                                                              
*      I REPLACED THESE STATEMENTS WITH THE LOOP BELOW THEM.                    
*      THIS WILL ENSURE THAT WE CHECK ALL THE SOURCES IN THE REQUEST            
*      TO MAKE SURE 'PIV' IS NOT PART OF THE REQUEST.                           
*      THE ORIGINAL CODE WAS CHECKING ONLY THE FIRST SOURCE IN THE              
*      REQUEST.                                                                 
***      L     RF,APODBKL                                                       
***      CLC   1(3,RF),=C'PIV'                                                  
***      BE    VDAY14                                                           
*                                                                               
         L     RF,APODBKL          IF SOURCE PIV WAS REQUESTED,                 
VDAY13A  CLI   0(RF),X'FF'         USE THE DEFAULT DEFINITION OF PRIME          
         BE    VDAY13P             FROM DYTMMAC                                 
         CLC   1(3,RF),=C'PIV'                                                  
         BE    VDAY14                                                           
         LA    RF,PODBLNQ(RF)      CHECK EVERY SOURCE IN THE REQUEST            
         B     VDAY13A                                                          
*                                                                               
VDAY13P  MVC   0(10,R4),=X'7E07D008FC01076C08FC' NORMAL PRIME                   
*                                                                               
         L     RF,APODBKL          IF SOURCE MXM WAS REQUESTED,                 
VDAY13Q  CLI   0(RF),X'FF'         USE EXACT MINUTE DEFINITION OF PRIME         
         BE    VDAY14                                                           
         CLC   1(3,RF),=C'MXM'                                                  
         BE    VDAY13R                                                          
         LA    RF,PODBLNQ(RF)      CHECK EVERY SOURCE IN THE REQUEST            
         B     VDAY13Q                                                          
VDAY13R  MVC   0(10,R4),=X'7E07D008D301076C08D3' NORMAL PRIME EXACT MIN         
*                                                                               
VDAY14   XC    HALF,HALF           START PREV & CURR DAY AGAIN                  
         ZIC   RE,16(R3)                                                        
         AR    R4,RE                                                            
         B     VDAY28                                                           
*                                                                               
VDAY16   XC    WORK,WORK                                                        
         MVC   WORK+45(1),0(R5)    MOVE FIELD VALUE                             
         MVC   WORK+48(10),12(R5)  MOVE FIELD VALUE                             
         GOTO1 SCANNER,PARAS,WORK+40,(1,WORK),C',=,-'                           
         CLI   WORK,0                                                           
         BE    BADDAY                                                           
         CLI   WORK+1,0                                                         
         BNE   VDAY20                                                           
         L     R3,=A(DAYLIST)                                                   
         A     R3,PGNR                                                          
*                                                                               
VDAY18   MVC   0(1,R4),9(R3)                                                    
         MVC   HALF+1(1),9(R3)     HOLD ONTO THE POTENTIAL CURRENT DAY          
         SR    RE,RE                                                            
         ICM   RE,1,WORK                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   WORK+12(0),0(R3)                                                 
         BE    VDAY24                                                           
         LA    R3,10(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BNE   VDAY18                                                           
*                                                                               
* MIGHT BE REGULAR EXPRESSION                                                   
         GOTO1 DAYVAL,DMCB,(WORK,WORK+12),DMCB+20,DMCB+21                       
         MVC   0(1,R4),DMCB+20                                                  
         CLI   DMCB+20,0                                                        
         BE    VDAY22                                                           
         OI    0(R4),X'80'                                                      
         MVC   HALF+1(1),0(R4)     HOLD ONTO THE CURRENT DAY                    
         B     VDAY24                                                           
*                                                                               
*--CHECK DAY RANGES (MONDAY-FRIDAY OR MONDAY-SATURDAY)                          
VDAY20   SR    RE,RE                                                            
         ICM   RE,1,WORK                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   WORK+12(0),=CL6'MONDAY'                                          
         BNE   VDAY22                                                           
*                                                                               
         MVI   0(R4),X'7C'                                                      
         MVI   HALF+1,X'7C'        HOLD ONTO THE POTENTIAL CURRENT DAY          
         SR    RE,RE                                                            
         ICM   RE,1,WORK+1                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   WORK+22(0),=CL6'FRIDAY'                                          
         BE    VDAY24                                                           
*                                                                               
         MVI   0(R4),X'7F'                                                      
         MVI   HALF+1,X'7F'        HOLD ONTO THE POTENTIAL CURRENT DAY          
         SR    RE,RE                                                            
         ICM   RE,1,WORK+1                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   WORK+22(0),=CL6'SUNDAY'                                          
         BE    VDAY24                                                           
*                                                                               
VDAY22   L     RE,APODBKL                                                       
         CLC   =C'CCV',1(RE)       COUNTY COVERAGE - ALLOW SUN-SAT              
         BNE   VDAY23                                                           
         SR    RE,RE                                                            
         ICM   RE,1,WORK                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   WORK+12(0),=CL6'SUNDAY'                                          
         BNE   VDAY23                                                           
         SR    RE,RE                                                            
         ICM   RE,1,WORK+1                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   WORK+22(0),=CL8'SATURDAY'                                        
         BNE   VDAY23                                                           
         MVI   0(R4),X'90'                                                      
         MVI   HALF+1,X'90'                                                     
         B     VDAY24                                                           
*                                                                               
VDAY23   DS    0H                                                               
         CLI   1(R5),0             IF THIS IS A DIVIDED FIELD,                  
         BNE   BADDAY               THEN THE DAY IS INVALID                     
         CLI   HALF,0              IF NO PREVIOUS DAY,                          
         BE    BADDAY               THEN WE MUST HAVE A VALID DAY               
         MVC   0(1,R4),HALF        ELSE, WE CAN USE PREVIOUS DAY                
         MVI   BYTE,1              ASSUME DATA IN SINGLE FIELD IS TIME,         
         MVC   HALF+1(1),HALF       UPDATE THE CURRENT TO PREV DAY,             
*                                                                               
VDAY24   MVC   HALF(1),HALF+1      CURR DAY BECOMES PREV DAY                    
         LA    R2,12(R5)           LET R2 POINT TO DATA IN FIELD                
         ZIC   R3,0(R5)                                                         
         CLI   BYTE,1              IS THIS A SINGLE FIELD?                      
         BE    VDAY26               YES                                         
         LA    R2,10(R2)            NO, RESET R2 AND L(DATA)                    
         ZIC   R3,1(R5)                                                         
*                                                                               
VDAY26   MVI   1(R4),X'FF'         CHECK FOR 'ALL'                              
         CLC   0(4,R2),=C'TALL'                                                 
         BNE   VDAY27                                                           
         CLI   DAYPOPT,C'Y'        DO WE HAVE DAYPART OPTION?                   
         BE    BADTIME2            YES, TALL NOT VALID                          
         B     VDAY28                                                           
*                                                                               
VDAY27   CLI   NADUSE,C'Y'                                                      
         BE    BADTIME                                                          
         LTR   R3,R3                                                            
         BZ    BADTIME                                                          
*                                                                               
         L     RE,APODNET                                                       
VDAY27A  OC    0(3,RE),0(RE)       FOR ITN DON'T ALLOW FILTERS ON TIME          
         BZ    VDAY27B                                                          
         CLI   0(RE),X'FF'                                                      
         BE    VDAY27B                                                          
         CLC   =C'SIN S',0(RE)                                                  
         BE    BADTIME3                                                         
         CLC   =C'ITN S',0(RE)                                                  
         BE    BADTIME3                                                         
         LA    RE,PODNETL(RE)                                                   
         B     VDAY27A                                                          
*                                                                               
VDAY27B  GOTO1 TIMVAL,PARAS,((R3),0(R2)),1(R4)                                  
         CLI   0(R1),X'FF'                                                      
         BE    BADTIME                                                          
*                                                                               
         OC    3(2,R4),3(R4)       TEST FOR END TIME                            
         BNZ   *+10                                                             
         MVC   3(2,R4),1(R4)       NO-SET END TIME=START TIME                   
*                                                                               
         MVC   DUB,1(R4)           ADJUST TIME FOR 12-6AM                       
         LH    R1,DUB                                                           
*                                                                               
         CLI   PDOVSYS,2           SPOT SYSTEM?                                 
         BE    VDAY27D                                                          
         CLI   PDOVSYS,8           REP SYSTEM?                                  
         BE    VDAY27D                                                          
*                                                                               
         CH    R1,=H'600'          NET DAY START AT 6 AM                        
         BNL   *+8                                                              
         AH    R1,=H'2400'                                                      
         STH   R1,DUB                                                           
         LH    R1,DUB+2                                                         
         CH    R1,=H'600'                                                       
         BH    *+8                                                              
         AH    R1,=H'2400'                                                      
         STH   R1,DUB+2                                                         
         B     VDAY27E                                                          
*                                                                               
VDAY27D  CH    R1,=H'500'          SPOT AND REP DAY STARTS AT 5 AM              
         BNL   *+8                                                              
         AH    R1,=H'2400'                                                      
         STH   R1,DUB                                                           
         LH    R1,DUB+2                                                         
         CH    R1,=H'500'                                                       
         BH    *+8                                                              
         AH    R1,=H'2400'                                                      
         STH   R1,DUB+2                                                         
*                                                                               
VDAY27E  CLC   DUB+2(2),DUB        ENSURE END GT OR EQ START                    
         BL    BADTIME                                                          
         MVC   1(4,R4),DUB                                                      
*                                                                               
VDAY28   LA    R5,42(R5)           BUMP TO NEXT BLOCK                           
         CLI   0(R5),0                                                          
         BE    VDAY30                                                           
         LA    R4,5(R4)                                                         
         B     VDAY08                                                           
*                                                                               
VDAY30   MVI   FIELDERR,0                                                       
         LA    R4,5(R4)                                                         
         MVI   0(R4),X'99'         SET END OF TABLE MARK                        
         LA    RE,PODDAYTM                                                      
         LA    RF,PODDAYTM                                                      
*                                                                               
VDAY32   OC    1(4,RE),1(RE)                                                    
         BNZ   VDAY38                                                           
         CLI   0(RE),X'99'                                                      
         BE    VDAY40                                                           
*                                                                               
VDAY34   OC    1(4,RF),1(RF)                                                    
         BNZ   VDAY36                                                           
         LA    RF,5(RF)                                                         
         CLI   0(RF),X'99'                                                      
         BNE   VDAY34                                                           
         B     BADTIME                                                          
*                                                                               
VDAY36   MVC   1(4,RE),1(RF)                                                    
         LA    RE,5(RE)                                                         
         CR    RE,RF                                                            
         BE    VDAY32                                                           
         BNH   *+6                                                              
         DC    H'0'                                                             
         B     VDAY36                                                           
*                                                                               
VDAY38   LA    RE,5(RE)                                                         
         LA    RF,5(RF)                                                         
         CLI   0(RE),X'99'                                                      
         BNE   VDAY32                                                           
*                                                                               
VDAY40   MVI   DYTMOVR,0                                                        
         CLI   PODNADBK,X'F0'      IF SPOT BOOK AND                             
         BNE   VDAY42                                                           
         CLI   PODBMED,C'T'         TV REQUESTED,                               
         BE    VDAYX                THEN DONT CHECK OVERLAP                     
         CLI   PODBMED,C'C'         TV REQUESTED,                               
         BE    VDAYX                THEN DONT CHECK OVERLAP                     
         CLI   PODBMED,C'O'         TV OVERNIGHT REQUESTED,                     
         BE    VDAYX                THEN DONT CHECK OVERLAP                     
*                                                                               
VDAY42   BRAS  RE,CKOVLAP          CHECK DAY TIME OVERLAP                       
         BZ    VDAYX               IF OK EXIT                                   
         MVI   DYTMOVR,C'Y'                                                     
         CLI   DAYPOPT,C'Y'        ALLOW OVERLAP FOR DAYPART OPTION             
         BNE   DTOVER                                                           
*                                                                               
VDAYX    B     XIT                                                              
*        DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*        PICK UP HUTS FOR PROGRAM                                     *         
*              INPUTS              HUTTIME REQUESTED START/END        *         
*                                  PDDAY6 REQUESTED DAY NUMBER        *         
*              OUTPUT              HUT RETURNED IN HUT                *         
***********************************************************************         
*                                                                               
VVPRGHUT CLI   HUTSW,0             INITIALIZE FIRST TIME THROUGH                
         BNE   *+8                                                              
         BRAS  RE,HUTINIT                                                       
         MVI   HUTSW,1                                                          
         ZIC   R3,PDDAY                                                         
         MH    R3,=H'96'                                                        
         LA    R3,HUTVALS(R3)      PICK UP HUTS FOR DAY                         
         MVC   DBLOCKA(96),0(R3)                                                
         LA    R4,KEY                                                           
         BAS   RE,GETAHUTS                                                      
         MVC   0(96,R3),DBLOCKA    ROUTINE UPDATES VALUES                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL ROUTINES FOR VPRGHUT                              *         
***********************************************************************         
*                                                                     *         
*        COMPUTE HUTS                                                 *         
*                                                                     *         
GETAHUTS NTR1                                                                   
         LA    R2,HUTTIME          CONVERT TIME TO QUARTERS                     
         LA    R3,HUTQ                                                          
         BRAS  RE,GETQ                                                          
         MVC   HUTQ+1(1),HUTQ                                                   
         LA    R2,HUTTIME+2                                                     
         LA    R3,HUTQ+1                                                        
         OC    0(2,R2),0(R2)                                                    
         BZ    *+8                                                              
         BRAS  RE,GETQ                                                          
*                                                                               
         SR    R2,R2               ADD HUTS IN R2                               
         LA    R3,1                COUNT IN R3                                  
*                                                                               
GETA02   ZIC   R1,HUTQ                                                          
         SRL   R1,1                                                             
         SLL   R1,1                                                             
         LA    R1,DBLOCKA(R1)      LOOK UP HUT FOR THIS 1/2 HOUR                
         BAS   RE,GETDHUTS                                                      
         AH    R2,0(R1)                                                         
         AI    HUTQ,2                                                           
         CLC   HUTQ(1),HUTQ+1                                                   
         BNL   GETA04                                                           
         LA    R3,1(R3)                                                         
         B     GETA02                                                           
*                                                                               
GETA04   LR    R0,R2               AVERAGE HUTS                                 
         SRDA  R0,31                                                            
         DR    R0,R3                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         STH   R1,HUT                                                           
*                                                                               
GETAX    B     XIT                                                              
*                                                                     *         
*        REFRESH HUT VALUE FOR SPECIFC PDDAY HUTQ                     *         
*                                                                     *         
GETDHUTS NTR1                                                                   
         OC    0(2,R1),0(R1)       HAVE WE LOOKED THIS UP BEFORE                
         BNZ   GETDX                                                            
         LR    R2,R1                                                            
         LA    R5,DBLOCKA+100      SET UP BLOCK FOR GETHUT                      
         USING GETHUTD,R5                                                       
         XC    GHBLOCK,GHBLOCK                                                  
         MVI   GHPREVYR,C'N'       DON'T TRY PREV. YR. HUTS                     
         MVC   GHREPDAY,PDDAY                                                   
         MVC   GHQUARTS,HUTQ                                                    
         MVC   GHQUARTS+1(1),HUTQ                                               
         MVI   GHSCHEME,X'FE'      PRESET FROM YEAR RECORDS                     
         MVI   GHAVE,C'W'          SET UP FOR WEEKLY HUTS                       
         MVI   GH52,C'Y'           PRESET 52 WEEK OPTION                        
         CLI   HUT52,0                                                          
         BE    *+10                                                             
         MVC   GH52,HUT52                                                       
         MVC   GHBKTYPE,HUTTYPE    HUT TYPE (D, A, I OR C)                      
         MVC   GHSURVY,PDHUTTYP                                                 
*                                  DEFAULT IS ASCRIBED                          
         CLI   GHBKTYPE,0                                                       
         BNE   *+8                                                              
         MVI   GHBKTYPE,C'A'                                                    
*                                                                               
         CLI   GHBKTYPE,C'D'                                                    
         BNE   *+8                                                              
         MVI   GHBKTYPE,C'O'       (GETHUT MUST'NT CHANGE 'D' TO X'00')         
         MVC   GHBOOKS(2),PDSBOOK                                               
         MVC   GHBOOKS+2(2),PDSBOOK                                             
*                                                                               
*--CONVERT BOOK TO DEMO FILE WEEK                                               
         L     RE,=A(YRINDX)                                                    
         A     RE,PGNR                                                          
*                                                                               
GETD02   CLC   GHBOOKS(1),0(RE)    EXCEPTION YEAR                               
         BE    *+8                                                              
         CLI   0(RE),X'FF'         DEFAULT                                      
         BE    *+12                                                             
         LA    RE,3(RE)                                                         
         B     GETD02                                                           
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(RE)          CONVERT THE WEEK                             
         L     RE,=A(YRINDX)                                                    
         A     RE,PGNR                                                          
         AR    RE,R0                                                            
         ZIC   RF,GHBOOKS+1                                                     
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         MVC   GHBOOKS+1(1),0(RE)                                               
         MVC   GHBOOKS+2(2),GHBOOKS                                             
*                                                                               
         MVC   GHCOMFCS,ACOMFACS                                                
         MVC   GHAGY,AGENCY        FOR LOCKOUTS                                 
         CLI   HUTSCHEM,0                                                       
         BE    GETD04                                                           
         MVC   GHSCHEME,HUTSCHEM   AGENCY SCHEME REQUESTED                      
         MVC   GHAGYMED,PDBAGYMD                                                
*                                                                               
GETD04   GOTO1 GETHUT,DMCB,(R5)                                                 
         MVC   0(2,R2),GHHUT                                                    
         CLI   HUTSCHEM,0                                                       
         BE    GETDX                                                            
         GOTO1 HIGH                NEED TO RESTORE SEQUENCE                     
*                                                                               
GETDX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PROGRAM PERIOD                                      *         
***********************************************************************         
*                                                                               
VVALPROG DS    0H                                                               
         XC    PODPSTRT(4),PODPSTRT                                             
         CLI   5(R2),0                                                          
         BNE   VVALPR02                                                         
         BAS   RE,BUMP                                                          
         CLI   5(R2),0                                                          
         BNE   INVPROG                                                          
         B     VVALPRX                                                          
*                                                                               
VVALPR02 DS    0H                                                               
         LA    R3,WORK+10                                                       
         XC    0(L'SPLPST+1,R3),0(R3)       MAKE SURE THAT DATE IS NOT          
         MVC   0(L'SPLPST,R3),8(R2)         FOLLOWED BY JUNK                    
*VALPR02 GOTO1 DATVAL,PARAS,(0,8(R2)),WORK                                      
         GOTO1 DATVAL,PARAS,(0,0(R3)),WORK                                      
         OC    PARAS(4),PARAS                                                   
         BZ    INVPROG                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(2,PODPSTRT)                                
*                                                                               
         ZIC   RE,PODPSTRT         FIRST 7 BITS =  COMPRESSED YEAR              
         SRA   RE,1                                                             
         CHI   RE,84               YEARS < 1984 ARE INVALID                     
         BL    INVPROG             THIS ALSO TAKES CARE OF YEARS>2027           
*                                  WHICH GET TRANSLATED TO 1928,1929...         
         MVC   PROGSTRT(6),WORK    SAVE EBCDIC FORMAT                           
*                                                                               
         BAS   RE,BUMP             GET END DATE                                 
         BAS   RE,BUMP                                                          
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         MVC   PODPEND,PODPSTRT    IF NO END START AND END EQUAL                
         B     VVALPR04                                                         
*                                                                               
         LA    R3,WORK+10                                                       
         XC    0(L'SPLPEN+1,R3),0(R3)       MAKE SURE THAT DATE IS NOT          
         MVC   0(L'SPLPEN,R3),8(R2)         FOLLOWED BY JUNK                    
         GOTO1 DATVAL,PARAS,(0,0(R3)),WORK                                      
         OC    PARAS(4),PARAS                                                   
         BZ    INVPROG                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(2,PODPEND)                                 
*                                                                               
         ZIC   RE,PODPEND          FIRST 7 BITS =  COMPRESSED YEAR              
         SRA   RE,1                                                             
         CHI   RE,84               YEARS < 1984 ARE INVALID                     
         BL    INVPROG             THIS ALSO TAKES CARE OF YEARS>2027           
*                                  WHICH GET TRANSLATED TO 1928,1929...         
*                                                                               
VVALPR04 CLC   PODPSTRT,PODPEND                                                 
         BH    INVPROG                                                          
*                                                                               
VVALPRX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OPTIONS                                             *         
***********************************************************************         
*                                                                               
VVALOPTS DS    0H                                                               
         CLI   0(R1),X'FF'                                                      
         BE    VVALOP2                                                          
         GOTO1 =A(VALOPXNS),DMCB,0,(R2),(R9),(RC),RR=PGNR                       
         B     VVALOPX                                                          
*                                                                               
VVALOP2  GOTO1 =A(VALOPXNS),DMCB,(X'FF',DUB),(R2),(R9),(RC),RR=PGNR             
*                                                                               
         CLI   PDSIDOPT,C'Y'       ARE WE USING SID?                            
         BNE   VVALOPX                                                          
         CLI   PDSDPER,0                                                        
         BE    PERERR2                                                          
*                                                                               
VVALOPX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE TITLE                                               *         
***********************************************************************         
*                                                                               
VVALTITS DS    0H                                                               
         BRAS  RE,VALTITLE                                                      
*                                                                               
VTIT02   GOTO1 CENTER,DMCB,TITLE,64                                             
*                                                                               
VTITX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE FILTERS                                             *         
***********************************************************************         
*                                                                               
VVALFILT L     RE,APDPDFLT         CLEAR PROGRAM/NETWORK TABLE (PDF)            
         LA    RF,L'PDPDFLT                                                     
         XCEFL                                                                  
*                                                                               
         L     RE,APDPRGTT         CLEAR DATE/TIMES TABLE (PDF)                 
         LA    RF,L'PDPRGTT                                                     
         XCEFL                                                                  
*                                                                               
         L     RE,APDPRGTT                                                      
         STCM  RE,15,PRGTPTR       SAVE TABLE ADDRESS                           
*                                                                               
         L     RE,APDSUBPT         CLEAR SUB PROGRAM TABLE (SUBPTYPE)           
         LA    RF,L'PDSUBPT                                                     
         XCEFL                                                                  
*                                                                               
         L     RE,APVSFILT         CLEAR PVS FILTER TABLE                       
         LA    RF,PVSFILTL                                                      
         XCEF                                                                   
*                                                                               
         L     RE,APDSUBPT                                                      
         MVI   0(RE),X'FF'         SET END MARKER                               
*                                                                               
         XC    PODNTFLT(147),PODNTFLT   CLEAR AREAS                             
         MVI   PODNTFLT,X'FF'      SET END OF FILTERS MARK                      
         MVI   PODNDFLT,X'FF'                                                   
         MVI   PODNHFLT,X'FF'                                                   
         MVI   PODPVFLT,X'FF'                                                   
         XC    PDHISPNM,PDHISPNM                                                
         MVI   PDHISPLN,0                                                       
         CLI   5(R2),0                                                          
         BE    VFILX                                                            
*                                                                               
         ICM   RE,15,AVALBLCK                                                   
         LA    RF,L'VALBLCK                                                     
         XCEFL                                                                  
         GOTO1 SCANNER,DMCB,(20,(R2)),(X'8E',AVALBLCK),0 14 LINES MAX           
         ZIC   R0,4(R1)                                                         
         ICM   R3,15,AVALBLCK                                                   
         LTR   R0,R0                                                            
         BZ    BADFILT                                                          
         MVI   FIELDERR,1                                                       
         B     VFIL04                                                           
*                                                                               
VFIL02   CLI   1(R3),0                                                          
         BE    VFIL08                                                           
*                                                                               
VFIL04   L     R4,APDPDFLT         ADDRESS PDF TABLE                            
         CLC   12(3,R3),=C'PDF'    PDF FILTER IN USE?                           
         BE    VFIL40              YES                                          
         LA    R4,PODNTFLT                                                      
         CLC   12(3,R3),=C'NTI'    NTI USES THIS AREA                           
         BE    VFIL06                                                           
         CLC   12(4,R3),=C'OOH '   NTI USES THIS AREA                           
         BE    VFIL06                                                           
         LA    R4,PODNDFLT                                                      
         CLC   12(3,R3),=C'NAD'    NAD & NHT USE A SEPARATE AREA                
         BE    VFIL06                                                           
         CLC   12(3,R3),=C'NHT'                                                 
         BE    VFIL06                                                           
         CLC   12(3,R3),=C'HPM'                                                 
         BE    VFIL06                                                           
         CLC   12(3,R3),=C'MPA'                                                 
         BE    VFIL06                                                           
         CLC   12(2,R3),=C'TP'                                                  
         BE    VFIL06                                                           
         CLC   12(2,R3),=C'T4'                                                  
         BE    VFIL06                                                           
         CLC   12(3,R3),=C'PAV'                                                 
         BE    VFIL06                                                           
         CLC   12(3,R3),=C'IUN'                                                 
         BE    VFIL06                                                           
         CLC   12(3,R3),=C'DPT'                                                 
         BE    VFIL06                                                           
         CLC   12(3,R3),=C'CSI'                                                 
         BE    VFIL06                                                           
         CLC   12(3,R3),=C'BBM'                                                 
         BE    VFIL06                                                           
         CLC   12(3,R3),=C'SRC'                                                 
         BE    VFIL06                                                           
         LA    R4,PODPVFLT                                                      
         CLC   12(3,R3),=C'PIV'                                                 
         BE    VFIL06                                                           
         CLC   =C'PNAME',12(R3)                                                 
         BE    VFIL16                                                           
         CLC   12(4,R3),=C'SUBPTYPE'                                            
         BE    VFIL50                                                           
         CLC   =C'PVS',12(R3)                                                   
         BE    VFIL60                                                           
         B     VFIL18                                                           
*                                                                               
VFIL06   MVC   0(1,R3),1(R3)                                                    
         MVC   12(10,R3),22(R3)                                                 
****     LA    R0,11                                                            
* OPUP: I DON'T UNDERSTAND STATEMENT ABOVE. R0 IS A COUNTER FOR ALL             
* TOKENS PARSED. RESETING IT CAUSES PROBLEMS IF ANOTHER FILTER FOLLOWS.         
* REMOVED 5/21/07                                                               
****                                                                            
VFIL08   ZIC   RF,0(R3)                                                         
         LTR   RF,RF                                                            
         BZ    VFIL14                                                           
         LR    R1,R4                                                            
         LA    RE,12(R3)                                                        
*                                                                               
VFIL10   CLI   0(RE),C'-'          NEGATE?                                      
         BNE   VFIL12                                                           
         LTR   RF,RF                                                            
         BZ    BADFILT                                                          
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BZ    BADFILT                                                          
         LA    RE,1(RE)                                                         
         NI    0(RE),X'BF'                                                      
*                                                                               
VFIL12   MVC   0(1,R1),0(RE)                                                    
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,VFIL10                                                        
*                                                                               
         LA    R4,6(R4)            AND ADDRESS THE NEXT AREA                    
         MVI   0(R4),X'FF'                                                      
*                                                                               
VFIL14   LA    R3,42(R3)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,VFIL02                                                        
         MVI   0(R4),X'FF'                                                      
         B     VFILX                                                            
*                                                                               
VFIL16   BAS   RE,BMPSTRG          BUMP THRU TO FIND LENGTH                     
         BNE   BADPREF                                                          
         ZIC   R1,PDHISPNM+1                                                    
         LA    RF,8(R2)                                                         
         ZIC   RE,PDHISPNM                                                      
         AR    RF,RE                                                            
         SR    R1,RE                                                            
         STC   R1,PDHISPLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PDHISPNM(0),0(RF)                                                
         GOTO1 HISPNAME,DMCB,(PDHISPLN,PDHISPNM),0,SCANNER                      
         CLI   DMCB,0              MATCH?                                       
         BNZ   BADPREF             NO, ERRORS                                   
*                                                                               
         LTR   R0,R0                                                            
         BNZ   VFIL14                                                           
         MVI   0(R4),X'FF'                                                      
         B     VFILX                                                            
*                                                                               
VFIL18   CLC   12(3,R3),=C'NOR'    OPTION TO SUPPRESS (NOR) LINES               
         BNE   VFIL20               PAV ONLY                                    
         MVC   PDNOROPT,22(R3)                                                  
         CLI   22(R3),C'N'                                                      
         BE    VFIL36                                                           
         CLI   22(R3),C'Y'                                                      
         BE    VFIL36                                                           
         B     BADFILT                                                          
*                                                                               
VFIL20   CLC   12(3,R3),=C'WK '    NUMBER OF WEEKS OPTION                       
         BNE   VFIL22                                                           
         CLI   22(R3),C'0'         LEAD MUST BE NUMERI                          
         BL    BADFILT                                                          
         CLI   22(R3),C'9'                                                      
         BH    BADFILT                                                          
         MVC   PDWKOPT,22(R3)      GET THE NUMBER OF WEEKS                      
         NI    PDWKOPT,X'0F'                                                    
         CLI   23(R3),C'+'         N OR MORE                                    
         BNE   *+8                                                              
         OI    PDWKOPT,X'40'                                                    
         CLI   23(R3),C'-'         N OR LESS                                    
         BNE   *+8                                                              
         OI    PDWKOPT,X'80'                                                    
         CLI   23(R3),C'/'         ACTIVITY IN THESE WEEKS                      
         BNE   *+8                                                              
         OI    PDWKOPT,X'20'                                                    
         B     VFIL36                                                           
*                                                                               
VFIL22   CLC   12(2,R3),=C'PT'     FILTER SID PROGRAM TYPES                     
         BNE   VFIL24                                                           
         CLI   1(R3),8             MAX OF 8                                     
         BH    BADFMX8                                                          
         MVC   PDSDPTPL,22(R3)                                                  
         B     VFIL36                                                           
*                                                                               
VFIL24   CLC   12(2,R3),=C'DT'     FILTER SID DAYPARTS                          
         BNE   VFIL26                                                           
         CLI   1(R3),8             MAX OF 8                                     
         BH    BADFMX8                                                          
         MVC   PDSDDPTL,22(R3)                                                  
         B     VFIL36                                                           
*                                                                               
VFIL26   CLC   12(3,R3),=C'GAA'    OPTION SUPPRESS/INCLUDE GAA LINES            
         BNE   VFIL28               SYNDICATION ONLY                            
         MVC   PDGAAOPT,22(R3)                                                  
         CLI   22(R3),C'N'                                                      
         BE    VFIL36                                                           
         CLI   22(R3),C'Y'                                                      
         BE    VFIL36                                                           
         B     BADFILT                                                          
*                                                                               
VFIL28   CLC   12(3,R3),=C'EFF'                                                 
         BNE   BADPREF                                                          
         XC    PODPSTRT(4),PODPSTRT                                             
         LA    RF,22(R3)                                                        
         LA    RE,WORK+20                                                       
*                                                                               
VFIL30   CLI   0(RF),C'-'                                                       
         BE    VFIL32                                                           
         CLI   0(RF),C' '                                                       
         BNH   VFIL32                                                           
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         B     VFIL30                                                           
*                                                                               
VFIL32   GOTO1 DATVAL,PARAS,(0,WORK+20),WORK                                    
         OC    PARAS(4),PARAS                                                   
         BZ    BADFILT                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(2,PODPSTRT)                                
*                                                                               
         ZIC   RE,PODPSTRT         FIRST 7 BITS =  COMPRESSED YEAR              
         SRA   RE,1                                                             
         CHI   RE,84               YEARS < 1984 ARE INVALID                     
         BL    BADFILT             THIS ALSO TAKES CARE OF YEARS>2027           
*                                  WHICH GET TRANSLATED TO 1928,1929...         
         MVC   PROGSTRT(6),WORK    SAVE EBCDIC FORMAT                           
         LA    RF,22(R3)                                                        
*                                                                               
VFIL34   CLI   0(RF),C' '                                                       
         BNH   VFIL36                                                           
         CLI   0(RF),C'-'                                                       
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         B     VFIL34                                                           
*                                                                               
         LA    RF,1(RF)                                                         
         GOTO1 DATVAL,PARAS,(0,(RF)),WORK                                       
         OC    PARAS(4),PARAS                                                   
         BZ    BADFILT                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(2,PODPSTRT+2)                              
*                                                                               
         ZIC   RE,PODPSTRT+2       FIRST 7 BITS =  COMPRESSED YEAR              
         SRA   RE,1                                                             
         CHI   RE,84               YEARS < 1984 ARE INVALID                     
         BL    BADFILT             THIS ALSO TAKES CARE OF YEARS>2027           
*                                  WHICH GET TRANSLATED TO 1928,1929...         
*                                                                               
* BUMP TO NEXT FILTER                                                           
VFIL36   LA    R3,42(R3)           BUMP TO NEXT FILTER                          
         AI    FIELDERR,1                                                       
         BCT   R0,*+8              IS IT THE END                                
         B     VFILX               YES - EXIT                                   
         CLI   1(R3),0             IS IT A KEYWORD                              
         BE    BADPREF             NO-ITS AN ERROR                              
         B     VFIL02                                                           
*                                                                               
VFIL38   CLI   1(R3),0             IS THERE A SECOND FIELD?                     
         BE    VFIL42              NO, CONTINUE ON                              
         B     VFIL04              YES, START OVER                              
*                                                                               
VFIL40   L     RF,APODMKTL         SAVE A(MARKET LIST TABLE)                    
         STCM  RF,15,PODMPTR                                                    
         CLI   0(RF),X'FF'         NO, USING MRKT OPTION?                       
         BNE   OPTCON              YES, ERROR                                   
         OC    PDMGRP,PDMGRP       NO, USING MRKGRP OPTION?                     
         BNZ   OPTCON              YES, ERROR                                   
*                                                                               
         MVC   0(1,R3),1(R3)       MOVE LENGTH AND DATA OF FIRST ITEM           
         MVC   12(10,R3),22(R3)                                                 
*                                                                               
VFIL42   ZIC   RF,0(R3)            GET LENGTH OF ENTRY                          
         LTR   RF,RF                                                            
         BZ    VFIL48                                                           
         LR    R1,R4               SAVE TABLE ADDRESS                           
         LA    RE,12(R3)           ADDRESS THE DATA                             
         CLI   5(RE),C'/'          / DIVIDES PROGRAM/STATION                    
         BE    VFIL44              IT'S THERE, OK                               
         CHI   RF,5                NOT THERE, IS THERE A STATION?               
         BH    BADFILT             YES, ERROR                                   
*                                                                               
VFIL44   CLI   0(RE),C'/'          SKIP THE DIVIDER                             
         BE    VFIL46                                                           
         MVC   0(1,R1),0(RE)       MOVE 1 BYTE                                  
         LA    R1,1(R1)            SHIFT THE TABLE                              
*                                                                               
VFIL46   LA    RE,1(RE)            GET NEXT BYTE                                
         BCT   RF,VFIL44           CONTINUE                                     
*                                                                               
         OC    0(9,R4),SPACES      FILL IN WITH BLANKS                          
         LA    R4,9(R4)            MAX ENTRY IS 9 BYTES                         
         MVI   0(R4),X'FF'         MARK END OF TABLE                            
*                                                                               
VFIL48   LA    R3,42(R3)           GET NEXT ENTRY IN BLOCK                      
         AI    FIELDERR,1                                                       
         BCT   R0,VFIL38                                                        
         MVI   0(R4),X'FF'                                                      
         B     VFILX                                                            
*                                                                               
* SUBTYPE FILTER                                                                
*                                                                               
VFIL50   L     R4,APDSUBPT         GET SUBPTYPE TABLE                           
         MVC   0(1,R3),1(R3)       SHIFT DATA FIRST TIME IN                     
         MVC   2(1,R3),3(R3)                                                    
         MVC   4(4,R3),8(R3)                                                    
         MVC   12(10,R3),22(R3)                                                 
*                                                                               
VFIL52   MVC   0(4,R4),12(R3)      2ND HALF IS VALID NUMERIC                    
         LA    R4,4(R4)            GET NEXT SPOT IN LIST                        
         MVI   0(R4),X'FF'         MARK IN CASE LAST                            
*                                                                               
         L     RF,APDSUBPT                                                      
         LA    RF,L'PDSUBPT(RF)    A(END OF SUB PROGRAM TABLE)                  
         CR    R4,RF                                                            
         BNL   BADFILT2            ERROR IF PAST IT                             
*                                                                               
         LA    R3,42(R3)           BUMP TO NEXT FILTER                          
         AI    FIELDERR,1                                                       
         BCT   R0,*+8              IS IT THE END                                
         B     VFILX               YES - EXIT                                   
         CLI   1(R3),0             IS THERE ANOTHER FIELD?                      
         BE    VFIL52              YES, STILL IN SUBPTYPE                       
         B     VFIL02              NO, A KEYWORD                                
*                                                                               
* PVS FILTER                                                                    
*                                                                               
VFIL60   L     R4,APVSFILT         GET PVS TABLE                                
         OC    0(2,R4),0(R4)       ALREADY HAD A PVS FILTER?                    
         BNZ   BADFILT             YES. CAN'T HAVE MORE THAN ONE PVS=           
         MVC   0(1,R3),1(R3)       SHIFT DATA FIRST TIME IN                     
         MVC   12(10,R3),22(R3)                                                 
*                                                                               
         MVI   BYTE,1              FIRST TIME FLAG                              
         SR    RF,RF               PVS EXPRESSION COUNTER                       
VFIL62   BRAS  RE,VALPVS           VALIDATE ONE PVS EXPRESSION                  
         BNE   BADFILT             AND PUT IT AT DMCB(2)                        
         LA    RF,1(RF)                                                         
         CHI   RF,MAXPVSQ                                                       
         BH    BADFILT2                                                         
         MVI   BYTE,0              CLEAR FIRST TIME FLAG                        
         MVC   0(2,R4),DMCB                                                     
         LA    R4,2(R4)            MOVE VIEWING SOURCE TO TABLE                 
*                                                                               
         LA    R3,42(R3)           BUMP TO NEXT FILTER                          
         AI    FIELDERR,1                                                       
         BCT   R0,*+8              IS IT THE END                                
         B     VFILX               YES - EXIT                                   
         CLI   1(R3),0             IS THERE ANOTHER FIELD?                      
         BE    VFIL62              YES, STILL IN SUBPTYPE                       
         B     VFIL02              NO, A KEYWORD                                
*                                                                               
VFIL70   DS    0X                  ADD NEW FILTERS HERE                         
*                                                                               
         B     BADFILT                                                          
*                                                                               
VFILX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL VALFILT ROUTINES                                  *         
***********************************************************************         
*                                                                               
BMPSTRG  DS    0H                                                               
         MVC   PDHISPNM(1),8(R3)   JUST SAVE STARTING DISPLACEMENT              
         LR    RF,R0               LINES LEFT                                   
         CLI   1(R3),0                                                          
         BZ    BMPSERR             HAS TO BE AN EXPRESSION                      
         ZIC   R1,8(R3)            COMPUTE LENGTH OF FIRST PART                 
         ZIC   R0,1(R3)                                                         
         B     BMPS04                                                           
*                                                                               
BMPS02   CLI   1(R3),0                                                          
         BNZ   BMPSX                                                            
         CLI   0(R3),0                                                          
         BZ    BMPSX                                                            
         ZIC   R1,4(R3)                                                         
         ZIC   R0,0(R3)                                                         
*                                                                               
BMPS04   AR    R1,R0                                                            
         STC   R1,PDHISPNM+1                                                    
         LA    R3,42(R3)                                                        
         BCT   RF,BMPS02                                                        
*                                                                               
BMPSX    LR    R0,RF               RESTORE AMOUNT LEFT                          
         SR    R1,R1                                                            
         B     *+8                                                              
*                                                                               
BMPSERR  LA    R1,1                                                             
         LTR   R1,R1                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECK FILTERS                                                *         
***********************************************************************         
*                                                                               
VCHEFILT L     R4,0(R1)                                                         
         L     R2,4(R1)            FILTERS TABLE                                
         CLI   0(R2),X'FF'         ARE ANY SPECIFIED                            
         BE    CHEFYES             NO SO ITS OK                                 
         LA    R0,8                UP TO 8 MAY BE SPECIFIED                     
*                                                                               
CHEF02   CLI   0(R2),X'FF'         ARE ANY SPECIFIED                            
         BE    CHEFNO                                                           
         BAS   RE,CHEF06           GO CHECK                                     
         CLI   NEGSW,1             REVERSE THE FILTERS                          
         BE    CHEF04                                                           
         CLI   4(R1),0                                                          
         BE    CHEFYES             FOR ANY ONE TO BE SATISFIED                  
         LA    R2,6(R2)                                                         
         BCT   R0,CHEF02                                                        
         B     CHEFNO              NONE PASSED SO NO GOOD                       
*                                                                               
CHEF04   MVI   NEGSW,0             TRAP NEGATIVE FILTERS                        
         CLI   4(R1),0                                                          
         BE    CHEFNO              FOR ANY ONE TO BE SATISFIED                  
         LA    R2,6(R2)                                                         
         CLI   0(R2),X'FF'         END OF TABEL                                 
         BE    CHEFYES             IT'S FINE                                    
         BCT   R0,CHEF02                                                        
         B     CHEFYES             NONE PASSED SO ITS GOOD                      
*                                                                               
CHEF06   NTR1                                                                   
         LR    R3,R4                                                            
         LA    R0,4                                                             
         MVI   NEGSW,0                                                          
*                                                                               
         TM    0(R2),X'40'         DO WE HAVE A NEGATE?                         
         BO    *+8                 NO, GO ON CHECKING                           
         MVI   NEGSW,1             SET TO REVERSE                               
*                                                                               
CHEF08   MVC   BYTE,0(R2)          SAVE FOR NEGATIVE COMPARE                    
         OI    BYTE,X'40'          AND FORCE FOR COMPARE                        
         CLI   0(R2),C'*'          * IS WILD                                    
         BE    CHEF14                                                           
         CLI   BYTE,C'*'           THIS IN CASE '-' IS BEFORE * (-**R)          
         BE    CHEF14                                                           
         CLI   0(R2),0             SO IS ZERO                                   
         BE    CHEF14                                                           
         CLI   0(R2),C'?'          QUESTION SIGN IS SPECIAL CHAR MATCH          
         BE    CHEF12                                                           
         CLI   BYTE,C'?'           THIS IN CASE '-' IS BEFORE ? (-?**)          
         BE    CHEF12                                                           
*                                                                               
CHEF10   CLC   BYTE,0(R3)          MUST MATCH                                   
         BNE   CHEFNO                                                           
         B     CHEF14                                                           
*                                                                               
CHEF12   CLI   0(R3),C' '          MATCH ON ANY SPECIAL CHARACTER               
         BH    CHEFNO              INCLUDING SPACE AND BINARY ZERO              
*                                                                               
CHEF14   LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,CHEF08                                                        
         B     CHEFYES                                                          
*                                                                               
CHEFNO   MVI   4(R1),X'FF'                                                      
         B     CHEFX                                                            
*                                                                               
CHEFYES  DS    0H                  PART 2, CHECK HISPANIC PNAME FILT            
         OC    PDHISPLN,PDHISPLN   IF LENGTH IS ZERO, SKIP IT                   
         BZ    CHEFYES2                                                         
         MVI   BYTE,X'FF'          PRESET TO FAILED                             
         GOTO1 HISPNAME,DMCB,(PDHISPLN,PDHISPNM),(25,PDLPRO),SCANNER            
         BNE   *+8                                                              
         MVI   BYTE,X'00'          SUCCESS                                      
         GOTO1 HISPNAME,DMCB,(PDHISPLN,PDHISPNM),(16,PDPROG),SCANNER            
         BNE   *+8                                                              
         MVI   BYTE,X'00'          SUCCESS                                      
         GOTO1 HISPNAME,DMCB,(PDHISPLN,PDHISPNM),(6,PDSPRO),SCANNER             
         BNE   *+8                                                              
         MVI   BYTE,X'00'          SUCCESS                                      
         MVC   4(1,R1),BYTE                                                     
         B     CHEFX                                                            
*                                                                               
CHEFYES2 MVI   4(R1),X'00'                                                      
*                                                                               
CHEFX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE LEFT SIDE HEADING                                   *         
***********************************************************************         
*                                                                               
VVALLEFT XC    TOTWIDTH,TOTWIDTH   NOT CHECKING REPORT WIDTH YET                
         ZIC   R3,MAX                                                           
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,5                                                             
         LA    R4,4                START ON HEAD 4                              
         MVI   ANYROWSW,C'N'                                                    
         CLC   8(3,R2),=C'MED'     UNLESS FIRST HEAD NOT MEDIA                  
         BE    VLEF02                                                           
         LA    R4,5                                                             
*                                                                               
VLEF02   MVI   MYPOSO,C'H'                                                      
         STC   R4,MYPOSO+1                                                      
         MVI   MYPOSO+2,2          (COLUMN 2)                                   
         STC   R4,LASTHEAD                                                      
         BAS   RE,VALROW                                                        
         BAS   RE,BUMP                                                          
         CLI   5(R2),0                                                          
         BE    VLEFX                                                            
         ZIC   R4,LASTHEAD                                                      
         LA    R4,1(R4)                                                         
         BCT   R3,VLEF02                                                        
*                                                                               
VLEFX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE RIGHT SIDE HEADING                                  *         
***********************************************************************         
*                                                                               
VVALRGHT ZIC   R3,MAX                                                           
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,3                MAX 3 FIELDS                                 
         LA    R4,5                (START ON HEAD 5)                            
*                                                                               
VRIG02   CLI   5(R2),0                                                          
         BE    VRIG04                                                           
         MVI   MYPOSO,C'H'                                                      
         STC   R4,MYPOSO+1                                                      
         MVI   MYPOSO+2,96         (COLUMN 96)                                  
         CLI   NARROPT,C'Y'        (NOT ALLOWED FOR NARROW)                     
         BE    BADROW                                                           
         CLI   WIDEOPT,C'Y'                                                     
         BNE   *+8                                                              
         MVI   MYPOSO+2,129        (COLUMN 129 FOR WIDE)                        
         BAS   RE,VALROW                                                        
         LA    R4,1(R4)                                                         
*                                                                               
VRIG04   BAS   RE,BUMP                                                          
         BCT   R3,VRIG02                                                        
*                                                                               
         LA    R4,2(R4)                                                         
         CH    R4,=H'8'                                                         
         BH    *+8                                                              
         LA    R4,8                                                             
         IC    R3,MYFIRSTH                                                      
         CR    R4,R3                                                            
         BL    VRIGX                                                            
         STC   R4,MYFIRSTH                                                      
*                                                                               
VRIGX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE MID LINES                                           *         
***********************************************************************         
*                                                                               
VVALMID  MVI   MYPOSO,C'M'                                                      
         MVI   MYPOSO+1,1                                                       
         MVI   MYPOSO+2,1                                                       
         BAS   RE,VALROW                                                        
*                                                                               
VMIDX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE ROWS                                                *         
***********************************************************************         
*                                                                               
VVALROWS MVI   TOTWIDTH+1,1        START CHECKING REPORT WIDTH NOW              
         ST    R2,ALASTCOL                                                      
         ZIC   R3,MAX                                                           
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,8                                                             
         LA    R0,8                                                             
         BAS   RE,DELINS                                                        
*                                                                               
         XC    MYPOSO,MYPOSO                                                    
         TM    PDCOLRNK,4          NO RANK IN COLUMNS                           
         BZ    VVALR02                                                          
         TM    PDCOLRNK,3          IF COLUMNS HAVE RANK                         
         BO    RANKERR                                                          
         TM    SCR2IND,SCR2YES     ALREADY CREATED PHANTOM DETAIL               
         BO    VVALR02                                                          
         XC    WORK,WORK                  MAKE A PHANTOM DETAIL                 
         MVC   WORK(8),0(R2)              SAVE THE CURRENT HEADER               
         MVI   WORK+5,4                   NEW LENGTH                            
         MVC   WORK+8(16),=CL16'RANK'     RANK WORKS BEST                       
         LA    R2,WORK                    SEND THE PHANTOM THROUGH              
         BAS   RE,VALROW                                                        
         L     R2,ALASTCOL                USE ORIGINAL DETAIL                   
*                                                                               
VVALR02  XC    MYPOSO,MYPOSO                                                    
         BAS   RE,VALROW                                                        
         ZIC   R0,TOTWIDTH+1                                                    
         BCTR  R0,0                                                             
         CLI   ROW1WIDE,0                                                       
         BNE   *+8                                                              
         STC   R0,ROW1WIDE                                                      
         BAS   RE,BUMP                                                          
         BCT   R3,VVALR02                                                       
*                                                                               
         CLC   =C'SQAD',PODBEXT    IS THIS A SQAD REQUEST?                      
         BE    *+10                NO                                           
         CLC   =C'SQADR',PODBEXT    IS THIS A SQAD RADIO REQUEST?               
         BNE   VVALR04             NO                                           
         CLI   PDSQTROW,C'Y'       YES, MUST PRINT SQUARTER                     
         BNE   BADROW                                                           
                                                                                
VVALR04  STC   R0,ROWWIDTH                                                      
         L     R2,ALASTCOL                                                      
         CLI   ANYROWSW,C'N'                                                    
         BE    BADNEED1                                                         
*                                                                               
         ZIC   R4,LASTHEAD                                                      
         LA    R4,3(R4)                                                         
         CH    R4,=H'9'                                                         
         BH    *+8                                                              
         LA    R4,9                                                             
         STC   R4,MYFIRSTH                                                      
*                                                                               
         CLI   DAYPOPT,C'Y'        IF DAYPART OPTION                            
         BNE   VVALR10                                                          
         CLI   DYTMOVR,C'Y'        AND DAY/TIME OVERLAP                         
         BNE   VVALR10                                                          
         BAS   RE,CKDYTM           FORCE USER TO ENTER DAY & TIME               
         BNE   MISSDT                                                           
*                                                                               
VVALR10  CLC   =C'CCV',PODBEXT                                                  
         BNE   VVALR20                                                          
         BAS   RE,CKPROG          MAKE SURE PROG NAME WAS REQUESTED             
         BNE   MISSPRG                                                          
*                                                                               
VVALR20  CLI   PIVZZZ,1            ZZZ REQUEST FOR PIV                          
         BNE   VVALR30                                                          
         BAS   RE,CKNET            MAKE SURE NETWORK DETAIL REQUESTED           
         BNE   MISSNET                                                          
*                                                                               
VVALR30  TM    PDFLAG1,PDF1SYSC    IF SYSCODE LIST REQUESTED                    
         BNO   VVALR40                                                          
         TM    PDFLAG1,PDF1SCPV    FOR ALL POSSIBLE SYSCODES                    
         BO    VVALR40                                                          
         BAS   RE,CKPROG           MAKE SURE NO PROG NAME WAS REQUESTED         
         BE    PROGERR                                                          
*                                                                               
VVALR40  DS    0H                                                               
*                                                                               
VVALRX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL ROUTINES FOR VALROWS                              *         
***********************************************************************         
*                                                                               
*        CHECK IF DAY AND TIME WERE REQUESTED AS ROW ENTRIES                    
*                                                                               
CKDYTM   NTR1                                                                   
         L     R3,APODINPL         POINT MYSELF TO TABLE OF ROW/COLS            
         LA    R5,1(R3)            1ST BYTE IN TABLE = # OF ENTRIES             
         USING ENTRYTBD,R5                                                      
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,0(R3)          R1=# OF ENTRIES SO FAR                       
         LR    R0,R1                                                            
         BZ    DYTMNO                                                           
         MVI   BYTE,0                                                           
CKDYTM10 CLI   ETCOLROW,C'R'                                                    
         BNE   CKDYTM20                                                         
         CLC   ETENTRY,=CL8'D'                                                  
         BNE   *+8                                                              
         OI    BYTE,X'01'                                                       
         CLC   ETENTRY,=CL8'DAY'                                                
         BNE   *+8                                                              
         OI    BYTE,X'01'                                                       
         CLC   ETENTRY,=CL8'T'                                                  
         BNE   *+8                                                              
         OI    BYTE,X'02'                                                       
         CLC   ETENTRY,=CL8'TIME'                                               
         BNE   *+8                                                              
         OI    BYTE,X'02'                                                       
*                                                                               
CKDYTM20 LA    R5,ENTRYTBQ(R5)     BUMP TO NEXT SLOT IN TABLE                   
         BCT   R1,CKDYTM10                                                      
*                                                                               
         TM    BYTE,X'03'          CK IF BOTH DAY AN TIME REQUESTED             
         BNO   DYTMNO                                                           
*                                                                               
DYTMYES  CR    RB,RB                                                            
         B     CKDYTMX                                                          
DYTMNO   CHI   R8,0                                                             
CKDYTMX  J     XIT                                                              
         LTORG                                                                  
         SPACE 2                                                                
*        CHECK IF NETW NAME WAS REQUESTED AS A ROW ENTRY                        
*                                                                               
CKNET    NTR1                                                                   
         L     R3,APODINPL         POINT MYSELF TO TABLE OF ROW/COLS            
         LA    R5,1(R3)            1ST BYTE IN TABLE = # OF ENTRIES             
         USING ENTRYTBD,R5                                                      
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,0(R3)          R1=# OF ENTRIES SO FAR                       
         LR    R0,R1                                                            
         BZ    NETWNO                                                           
CKNETW10 CLI   ETCOLROW,C'R'                                                    
         BNE   CKNETW20                                                         
         L     RE,=A(VNETLST)                                                   
         A     RE,PGNR                                                          
CKNETW15 CLI   0(RE),X'FF'                                                      
         BE    CKNETW20            TRY NEXT ROW ENTRY                           
         CLC   ETENTRY,0(RE)                                                    
         BE    NETWYES                                                          
         LA    RE,L'ETENTRY(RE)                                                 
         B     CKNETW15                                                         
*                                                                               
CKNETW20 LA    R5,ENTRYTBQ(R5)     BUMP TO NEXT SLOT IN TABLE                   
         BCT   R1,CKNETW10                                                      
         B     NETWNO                                                           
*                                                                               
NETWYES  CR    RB,RB                                                            
         B     CKNETWX                                                          
NETWNO   CHI   R8,0                                                             
CKNETWX  B     XIT                                                              
         SPACE 2                                                                
*                                                                               
*        CHECK IF PROG NAME WAS REQUESTED AS A ROW ENTRY                        
*                                                                               
CKPROG   NTR1                                                                   
         L     R3,APODINPL         POINT MYSELF TO TABLE OF ROW/COLS            
         LA    R5,1(R3)            1ST BYTE IN TABLE = # OF ENTRIES             
         USING ENTRYTBD,R5                                                      
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,0(R3)          R1=# OF ENTRIES SO FAR                       
         LR    R0,R1                                                            
         BZ    PROGNO                                                           
CKPROG10 CLI   ETCOLROW,C'R'                                                    
         BNE   CKPROG20                                                         
         L     RE,=A(VPROG)                                                     
         A     RE,PGNR                                                          
CKPROG15 CLI   0(RE),X'FF'                                                      
         BE    CKPROG20            TRY NEXT ROW ENTRY                           
         CLC   ETENTRY,0(RE)                                                    
         BE    PROGYES                                                          
         LA    RE,L'ETENTRY(RE)                                                 
         B     CKPROG15                                                         
*                                                                               
CKPROG20 LA    R5,ENTRYTBQ(R5)     BUMP TO NEXT SLOT IN TABLE                   
         BCT   R1,CKPROG10                                                      
         B     PROGNO                                                           
*                                                                               
PROGYES  CR    RB,RB                                                            
         B     CKPROGX                                                          
PROGNO   CHI   R8,0                                                             
CKPROGX  B     XIT                                                              
*                                                                     *         
*        VALIDATE FOR KEYWORD                                         *         
*                                                                     *         
VALROW   NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    VROWX                                                            
         MVI   ANYROWSW,C'Y'                                                    
         GOTO1 SCANNER,DMCB,(R2),(6,BLOCK),0                                    
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    BADROW                                                           
         MVI   FIELDERR,1                                                       
         LA    R4,BLOCK                                                         
*                                                                               
         CLC   =C'DEMO ',12(R4)    DON'T ALLOW KEYWORD DEMO                     
         BE    BADROW                                                           
*                                                                               
         CLC   =C'LEP',12(R4)      EPISODE INFO REQUESTED?                      
         BE    VROW00                                                           
         CLC   =C'EP',12(R4)       EPISODE INFO REQUESTED?                      
         BE    VROW00                                                           
         CLC   =C'NTI',12(R4)      NTI SERIES INTO REQUESTED?                   
         BNE   *+8                                                              
VROW00   OI    OPTFLAG1,COMEPQ                                                  
*                                                                               
         CLI   PDUSECAB,C'Y'       CABLE USED?                                  
         BNE   VROW01                                                           
         CLC   12(2,R4),=C'L '     TEST IF LENGTH USED                          
         BE    BADLEN                                                           
*                                                                               
VROW01   CLC   12(5,R4),=C'TEXT '  TEST TEXT COLUMN                             
         BNE   VROW02                                                           
         BRAS  RE,GENTEXT                                                       
*                                                                               
VROW02   CLC   12(4,R4),=C'MGRP'   TEST ROW = LOWEST LEVEL MKTGRP               
         BNE   VROW03                                                           
         CLC   PDQMGR,SPACES                                                    
         BNH   BADROW                                                           
         MVI   15(R4),C'1'         YES - DETERMINE ITS LEVEL                    
         CLC   PDMGR1LN,PDMGR2LN                                                
         BE    VROW05                                                           
         MVI   15(R4),C'2'                                                      
         CLC   PDMGR2LN,PDMGR3LN                                                
         BE    VROW05                                                           
         MVI   15(R4),C'3'                                                      
         B     VROW05                                                           
*                                                                               
VROW03   CLC   =C'DDSEP',12(R4)                                                 
         BNE   VROW04                                                           
         CLI   OFFLINE,C'Y'                                                     
         BE    VROW04                                                           
         CLI   DDS,C'Y'                                                         
         BNE   BADROW                                                           
*                                                                               
VROW04   CLC   12(4,R4),=C'PRGGRP' NEED TO KNOW IF PROGRAM GROUP                
         BNE   *+8                                                              
         MVI   PDPRGROW,C'Y'                                                    
*                                                                               
         CLC   12(6,R4),=C'SQUART' NEED TO KNOW IF SQAD QUARTER                 
         BNE   *+8                                                              
         MVI   PDSQTROW,C'Y'                                                    
*                                                                               
         CLC   12(7,R4),=C'SYSCODE'                                             
         BNE   VROW05                                                           
         OI    PDFLAG1,PDF1SYSC    IF SYSCODE KYWORD REQUESTED                  
         TM    PDFLAG1,PDF1ALST    MAKE SURE ALL-STATION REQUESTS...            
         BNO   VROW04A                                                          
         TM    PDFLAG1,PDF1SCPV    ...HAVE SYSCODE FILTERS                      
         BNO   BADROW1                                                          
VROW04A  TM    PDFLAG1,PDF1SCPV    FOR ALL-SYSCODE REQUESTS...                  
         BO    *+8                                                              
         MVI   DAYPOPT,C'Y'        ...FORCE OPTION DAYPART=Y                    
*                                                                               
VROW05   GOTO1 VROWDRON            VALIDATE A ROW ENTRY                         
         CLI   DRERROR,0                                                        
         BNE   BADROW                                                           
         CLI   DRATTRIB,C'C'       COLUMN ONLY ENTRIES NOT ALLOWED              
         BE    BADROW                                                           
         CLI   DRATTRIB,C'D'       DETIAL ONLY ENTIES IN HEAD OR MID            
         BNE   *+12                NOT ALLOWED                                  
         CLI   MYPOSO,0                                                         
         BNE   BADROW                                                           
         CLI   DRATTRIB+1,C'P'     TEST PERIOD ROW                              
         BNE   VROW06                                                           
         CLI   PDQPER,0            YES-CHECK NO PERIOD ROW BEFORE               
         BNE   BADROW                                                           
         MVI   PDQPERLO,1                                                       
         MVI   PDQPERHI,X'FF'                                                   
         CLI   DRARGSI,C'Q'        TEST QUATERS                                 
         BNE   *+12                                                             
         OI    PDQPER,PDQPQT                                                    
         B     VROW06                                                           
         CLI   DRARGSI,C'M'        TEST MONTHS                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    PDQPER,PDQPMN                                                    
*                                                                               
VROW06   CLC   12(4,R4),=C'N   '   (ROW) NET                                    
         BE    *+14                                                             
         CLC   12(4,R4),=C'NET '                                                
         BNE   VROW06A                                                          
         OI    CABWFLAG,NET                                                     
         BAS   RE,CKAMOUNT                                                      
*                                                                               
VROW06A  L     RE,=A(VPROG)        LOOK FOR PROGRAM KEYWORDS                    
         A     RE,PGNR                                                          
VROW06AA CLC   =X'FFFF',0(RE)                                                   
         BE    VROW06B                                                          
         CLI   0(RE),X'FF'                                                      
         BNE   *+8                                                              
         LA    RE,1(RE)                                                         
         CLC   0(L'ETENTRY,RE),12(R4)                                           
         BE    VROW06AY                                                         
         LA    RE,L'ETENTRY(RE)                                                 
         B     VROW06AA                                                         
VROW06AY OI    CABWFLAG,PROG                                                    
         BAS   RE,CKAMOUNT                                                      
*                                                                               
VROW06B  CLC   12(4,R4),=C'T   '   (ROW) TIME                                   
         BE    *+14                                                             
         CLC   12(4,R4),=C'TIME'                                                
         BNE   VROW06C                                                          
         OI    CABWFLAG,TIME                                                    
         BAS   RE,CKAMOUNT                                                      
*                                                                               
VROW06C  CLC   12(4,R4),=C'DATE'   (ROW) DATE                                   
         BE    *+14                                                             
         CLC   12(5,R4),=C'RUNDT'  (ROW) DATE                                   
         BNE   VROW06D                                                          
         OI    CABWFLAG,RUNDATE                                                 
         BAS   RE,CKAMOUNT                                                      
*                                                                               
VROW06D  CLC   12(4,R4),=C'RANK'                                                
         BNE   VROW10                                                           
         OI    CABWFLAG,RANKING                                                 
         BAS   RE,CKAMOUNT                                                      
                                                                                
         TM    SCR2IND,SCR2YES     NO RANKING ALLOWED ON 2ND SCREEN             
         BO    BADROW                                                           
         OC    PDSTACK,PDSTACK     CAN'T DO STACK DEMOS AND RANK                
         BNZ   BADSTROW                                                         
         CH    R0,=H'1'            IF ON ITS OWN, MAKE NO PRINT                 
         BE    VROW26                                                           
         LA    R4,32(R4)           RANK NEEDS A COMPUTE EXPRESSION              
*                                                                               
VROW07   BCTR  R0,0                                                             
         AI    FIELDERR,1                                                       
         MVI   DRCMPMAX,C'P'                                                    
         CLC   44(2,R4),=CL2'NP'   SEE IF NOPRINT OPTION FOR RANK               
         BNE   *+24                                                             
         NI    DRFLAGO,X'7F'                                                    
         NI    DRHEAD1,X'7F'                                                    
         NI    DRHEAD2,X'7F'                                                    
         NI    DRHEAD3,X'7F'                                                    
         NI    DRHEAD4,X'7F'                                                    
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   VROW08                                                           
         GOTO1 GROWDRON                                                         
         GOTO1 GCMPDRON                                                         
         CLI   DRERROR,0                                                        
         BNE   BADROW                                                           
         B     VROWX                                                            
*                                                                               
VROW08   GOTO1 VCMPDRON            VALIDATE A COMPUTE EXPRESSION                
         CLI   DRERROR,0                                                        
         BNE   BADROW                                                           
         CLI   MYPOSO,0            IF WE ARE IN THE ROWS                        
         BNE   VROW10                                                           
         CH    R3,=H'1'            CHECK THIS IS NOT THE LAST ROW               
         BE    BADLRANK                                                         
         LR    R3,R2                                                            
         BAS   RE,BUMP                                                          
         CLI   5(R2),0             AND THERE IS INPUT IN NEXT                   
         LR    R2,R3                                                            
         BE    BADLRANK            NOT GOOD TO RANK ON LAST ROW                 
*                                                                               
VROW10   MVI   BYTE,C'R'           BUILD ENTRYTAB BEFORE DOING                  
         BAS   RE,BLDENTTB          SPECIAL FOR HEADS & MIDS                    
                                                                                
         CLI   MYPOSO,C'H'         SPECIAL FOR HEADS                            
         BNE   VROW12                                                           
         BAS   RE,HEADROW                                                       
         B     VROW36                                                           
*                                                                               
VROW12   CLI   MYPOSO,C'M'         SPECIAL FOR MID                              
         BNE   VROW36                                                           
         OI    DRLAST,X'80'        GENERATE LAST STATEMENT                      
         MVI   DRLSPACE,1          WITH ONE SPACE                               
         B     VROW36                                                           
*                                                                               
VROW14   CLC   12(2,R4),=C'* '     TOTAL EXPRESSION                             
         BNE   VROW16                                                           
         OI    DRTOTAL,X'80'                                                    
         MVI   DRTSPACE,1          SPACE AFTER TOTALS                           
         B     VROW36                                                           
*                                                                               
VROW16   CLC   12(5,R4),=C'SKIP '  SKIP TO CHANNEL 1 AFTER BREAK                
         BNE   VROW18                                                           
         OI    DRFIRST,X'80'       GENERATE FIRST STATEMENT                     
         OI    DRFOPTS,DRFSKIP     WITH SKIP OPTION                             
         B     VROW36                                                           
*                                                                               
VROW18   CLC   12(6,R4),=C'SPACE ' SPACE OPTION                                 
         BNE   VROW20                                                           
         OI    DRLAST,X'80'        GENERATE LAST STATEMENT                      
         MVI   DRLSPACE,1          WITH AT LEAST ONE SPACE                      
         CLI   1(R4),0             CHECK SECOND PARAMETER                       
         BE    VROW36                                                           
         MVC   DRLSPACE,11(R4)                                                  
         CLI   DRLSPACE,0          S/B 1-3 LINES                                
         BE    BADROW                                                           
         CLI   DRLSPACE,3                                                       
         BH    BADROW                                                           
         B     VROW36                                                           
*                                                                               
VROW20   CLC   12(4,R4),=C'DET '   DET=N OR D=N                                 
         BE    VROW22                                                           
         CLC   12(2,R4),=C'D '                                                  
         BNE   VROW24                                                           
*                                                                               
VROW22   OI    DRTOTAL,X'80'                                                    
         MVI   DRTSPACE,1          SPACE AFTER TOTALS                           
         MVC   DRTDET(1),11(R4)    NUMBER OF DETAILS                            
         CLI   DRTDET,0                                                         
         BE    BADROW                                                           
         B     VROW36                                                           
*                                                                               
VROW24   CLC   12(3,R4),=C'NP '    OPTION NOT TO PRINT                          
         BNE   VROW28                                                           
*                                                                               
VROW26   NI    DRFLAGO,X'7F'                                                    
         NI    DRHEAD1,X'7F'                                                    
         NI    DRHEAD2,X'7F'                                                    
         NI    DRHEAD3,X'7F'                                                    
         NI    DRHEAD4,X'7F'                                                    
         B     VROW36                                                           
*                                                                               
VROW28   TM    2(R4),X'80'         NUMERIC=OUTPUT WIDTH OVERRIDE                
         BNO   VROW30                                                           
         L     R1,4(R4)            OUTPUT WIDTH OVERRIDE                        
         STC   R1,DRLENO           (NEW OUTPUT LENGTH)                          
         B     VROW36                                                           
*                                                                               
VROW30   LA    R1,DRHEAD1          CHECK FOR HEADING OVERRIDES                  
         CLC   12(2,R4),=C'H '                                                  
         BE    VROW32                                                           
         CLC   12(3,R4),=C'H1 '                                                 
         BE    VROW32                                                           
         LA    R1,DRHEAD2                                                       
         CLC   12(3,R4),=C'H2 '                                                 
         BE    VROW32                                                           
         LA    R1,DRHEAD3                                                       
         CLC   12(3,R4),=C'H3 '                                                 
         BE    VROW32                                                           
         LA    R1,DRHEAD4                                                       
         CLC   12(3,R4),=C'H4 '                                                 
         BNE   VROW34                                                           
*                                                                               
VROW32   CLI   MYPOSO,0            CHECK NOT HEADLINE OR MIDLINE                
         BNE   BADROW                                                           
         XC    0(26,R1),0(R1)      TURN OFF ROUTINE & ARGS                      
         CLI   1(R4),0                                                          
         BE    VROW36              HN= CAUSES REMOVAL                           
         USING DRHEADD,R1                                                       
         OI    DRHEAD,X'80'        OTHERWISE TURN IT BACK ON                    
         MVC   DRHLITL,1(R4)       PASS LITERAL LENGTH TO DRONE                 
         CLC   DRHLITL,DRLENO                                                   
         BH    HEADERR             CHECK LITERAL NOT WIDER THAN COLUMN          
*                                                                               
         ZIC   RE,DRHLITL                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8              MOVE IN THE HEADER LITERAL                   
         B     VROW36                                                           
         MVC   DRHLIT(0),22(R4)    ** EXECUTED                                  
         DROP  R1                                                               
*                                                                               
VROW34   CLC   12(2,R4),=C'U '                                                  
         BNE   BADROW                                                           
         BAS   RE,VUSRDRON                                                      
*                                                                               
VROW36   LA    R4,32(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,VROW14                                                        
*                                                                               
         TM    DRTOTAL,X'80'       TEST SPACE AFTER TOTAL                       
         BZ    VROW38                                                           
         TM    DRLAST,X'80'        AND SPACE OPTION SET                         
         BZ    VROW38                                                           
         MVC   DRTSPACE,DRLSPACE   YES-MOVE SPACE OPTION TO SPACE               
         MVI   DRLAST,0                AFTER TOTAL AND FORGET LAST              
         MVI   DRLSPACE,0              SPACE                                    
*                                                                               
VROW38   OC    TOTWIDTH,TOTWIDTH   IF WE ARE CHECKING WIDTH                     
         BZ    VROW40                                                           
         TM    DRFLAGO,X'80'                                                    
         BZ    VROW40                                                           
         LH    R1,TOTWIDTH         ADJUST CURRENT WIDTH                         
         ZIC   RF,DRLENO                                                        
         AR    R1,RF                                                            
         LA    R1,1(R1)                                                         
         STH   R1,TOTWIDTH                                                      
*                                                                               
VROW40   CLI   OFFLINE,C'Y'        NOW GENERATE ELEMENTS                        
         BNE   VROWX                                                            
         MVC   DRPOSO,MYPOSO                                                    
*                                                                               
         CLI   DRPOSO,C'H'         TEST HEADLINE                                
         BNE   VROW46                                                           
         TM    DRTOTAL,X'80'       AND TOTAL REQUESTED                          
         BZ    VROW46                                                           
         CLI   DRTDET,0            AND NOT DETAILED TOTAL                       
         BNE   VROW46                                                           
         CLI   DRRTNO,C' '         AND OUT ROUTINE SPECIFIED                    
         BNH   VROW46                                                           
         L     R1,ATWA             AND (THERE IS A MIDLINE                      
         USING T325FFD,R1                                                       
         CLI   SPLMIDH+5,0                                                      
         BNE   VROW44                                                           
         DROP  R1                                                               
         LR    RE,R2                    OR THIS IS NOT LAST HEADLINE)           
         SR    RF,RF                                                            
*                                                                               
VROW42   IC    RF,0(RE)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),0                                                          
         BE    VROW46                                                           
         TM    1(RE),X'20'                                                      
         BO    VROW42              FIND AN UNPROTECTED FIELD                    
         CLI   5(RE),0                                                          
         BE    VROW46              WITH SOME DATA                               
*                                                                               
VROW44   MVC   DRTRTN,DRRTNO       YES-USE THIS FOR TOTAL AS WELL               
         MVC   DRTARGS,DRARGSO     AND PASS THROUGH THE ARGUMENTS               
         MVI   DRTNARGS,16                                                      
         MVI   DRTLITLN,0                                                       
*                                                                               
VROW46   GOTO1 GROWDRON                                                         
*                                                                               
VROWX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
* CHECK IF AMOUNT OF DATA EXCEEDS REASONABLE LIMITS                             
CKAMOUNT DS    0H                                                               
         ST    RE,SAVERE                                                        
         TM    CABWFLAG,NTITP+ZZZC+RANKING+RUNDATE                              
         BO    TOOMCHER                                                         
         GOTO1 =A(OVFLRTN),DMCB,(13,DUB),(RC),=CL5'NTI',RR=PGNR VCOUNTB         
         L     RF,0(R1)                                                         
         CHI   RF,15                NUMBER OF WEEKS                             
         BNH   CKAMNTX                                                          
         TM    CABWFLAG,NTI+ZZZC+RANKING+RUNDATE                                
         BO    TOOMCHE2                                                         
         TM    CABWFLAG,NTI+ZZZC+RANKING+TIME                                   
         BO    TOOMCHE2                                                         
CKAMNTX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL ROUTINES FOR VALROW                               *         
***********************************************************************         
*                                                                               
HEADROW  DS    0H                                                               
         OI    DRFIRST,X'80'                                                    
         OI    DRFOPTS,DRFSKIP     WITH SKIP OPTION                             
         MVI   DRFSPACE,0                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE COLUMNS                                             *         
***********************************************************************         
*                                                                               
VVALCOLS LA    R0,12                                                            
         BAS   RE,DELINS                                                        
         ZIC   R0,MAX                                                           
         LTR   R0,R0                                                            
         BZ    VVALCX                                                           
         CLI   5(R2),0             NEED AT LEAST ONE COLUMN                     
         BE    BADNEED1                                                         
         MVI   MYLABEL,C'A'                                                     
         ST    R2,ALASTCOL                                                      
         BAS   RE,SETMAX           SET LAST COLUMN FOR COMPUTE                  
         XC    EDITLIST,EDITLIST                                                
         LA    R3,EDITLIST                                                      
*                                                                               
VVALC02  XC    MYPOSO,MYPOSO                                                    
         MVC   0(1,R3),MYLABEL     SET LABEL IN EDIT LIST                       
         BAS   RE,VALCOL                                                        
         BAS   RE,BUMP                                                          
         MVC   MYLABEL,8(R2)                                                    
         BAS   RE,BUMP                                                          
         LA    R3,4(R3)                                                         
         BCT   R0,VVALC02                                                       
*                                                                               
         CLC   TOTWIDTH,=H'80'     CHECK NOT TOO BIG NOW                        
         BNH   VVALCX                                                           
*                                                                               
         TM    DOWNOPT,DOWNON      FOR DOWNLOAD ALLOW UP TO 255                 
         BZ    VVALC05                                                          
*        CLC   TOTWIDTH,=H'255'    DON'T NEED TO SET LIMIT ANYMORE              
*        BH    COLWIDE                                                          
         B     VVALCX                                                           
*                                                                               
VVALC05  CLI   NARROPT,C'Y'        ONLY 80 ALLOWED WITH NARROW OPT              
         BE    COLWIDE                                                          
         CLC   TOTWIDTH,=H'132'    CHECK NOT TOO BIG NOW                        
         BNH   VVALCX                                                           
         CLI   WIDEOPT,C'Y'                                                     
         BNE   COLWIDE                                                          
         CLC   TOTWIDTH,=H'165'                                                 
         BH    COLWIDE                                                          
*                                                                               
VVALCX   DS    0H                  CHECK IF NOT TOO MANY DEMOS                  
         BAS   RE,COLMANY          (INCLUDING RANGES LIKE D1,W1-10)             
         BE    BADCOLM                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL ROUTINES FOR VALCOLS                              *         
***********************************************************************         
* MAKE SURE NOT TOO MANY DEMOS FROM COLUMNS (INCLUDING RANGES)                  
* D1,W1-10 COUNTS FOR 10 DEMOS                                                  
* SET MAXIMUM TO 30 TO AVOIND OVERFLOWING BUFFER IN DRIVAL                      
COLMANY  NTR1                                                                   
         L     R3,APODINPL        POINT MYSELF TO TABLE OF ROW/COLS             
         LA    R5,1(R3)           1ST BYTE IN TABLE = # OF ENTRIES              
         USING ENTRYTBD,R5                                                      
         ZIC   R1,0(R3)           R1=# OF ENTRIES SO FAR                        
         LTR   R1,R1                                                            
         BZ    COLMNO                                                           
*                                                                               
         SR    RE,RE              COUNT DEMOS FROM COLUMNS                      
COLM10   CLI   ETCOLROW,C'C'                                                    
         BNE   *+8                                                              
         LA    RE,1(RE)                                                         
         LA    R5,ENTRYTBQ(R5)                                                  
         BCT   R1,COLM10                                                        
*                                                                               
COLM20   CHI   RE,30              MAX 30 DEMOS FROM COLUMNS                     
         BH    COLMYES                                                          
         B     COLMNO                                                           
*                                                                               
COLMYES  CR    RB,RB                                                            
         B     COLMX                                                            
COLMNO   CHI   RB,0                                                             
COLMX    B     XIT                                                              
*                                                                               
* VALIDATE ONE COLUMN                                                           
*                                                                               
VALCOL   NTR1                                                                   
         L     R5,PDAFLBUF                                                      
         USING PDFLBUFF,R5                                                      
         XC    EXPLIST,EXPLIST                                                  
         DROP  R5                                                               
*                                                                               
VCOL02   CLI   5(R2),0                                                          
         BE    VCOLX                                                            
         ST    R2,ALASTCOL                                                      
         LA    R4,BLOCK+42                                                      
         MVI   FIELDERR,1                                                       
         CLI   COLXTEND,2          HANDLING EXTENSION HERE                      
         BNE   VCOL04                                                           
         LA    R4,BLOCK+42+42                                                   
         B     VCOL06                                                           
*                                                                               
VCOL04   MVI   COLXTEND,0                                                       
         XC    BLOCK(252),BLOCK                                                 
         ZIC   R1,5(R2)                                                         
         LA    R1,8-1(R1,R2)       (R1=A(LAST CHARACTER))                       
         CLI   0(R1),C','          IF THIS IS A COMMA                           
         BNE   VCOL06                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         STC   R1,5(R2)            REDUCE APPARENT LENGTH                       
         MVI   COLXTEND,1          AND NOTE THAT THERE IS AN EXTENSION          
*                                                                               
VCOL06   GOTO1 SCANNER,DMCB,(20,(R2)),(6,(R4)),0                                
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    BADCOL                                                           
         ST    R4,FULL             SAVE START OF COLUMN FIELDS                  
*                                                                               
         CLC   =C'DEMO ',12(R4)    DON'T ALLOW KEYWORD DEMO                     
         BE    BADCOL                                                           
*                                                                               
         CLC   =C'DDSEP',12(R4)                                                 
         BNE   *+20                                                             
         CLI   OFFLINE,C'Y'                                                     
         BE    *+12                                                             
         CLI   DDS,C'Y'                                                         
         BNE   BADCOL                                                           
*                                                                               
         CLC   =C'TEXT ',12(R4)     TEXT COLUMN                                 
         BNE   *+8                                                              
         BRAS  RE,GENTEXT                                                       
*                                                                               
VCOLSTD  CLC   =C'STDATA',12(R4)    CAN'T HAVE STDATA,NP                        
         BE    VCOLST1                                                          
         CLC   =C'STD',12(R4)       OR STD(N),NP                                
         BNE   VCOLSTX                                                          
         TM    15(R4),X'F0'                                                     
         BNO   VCOLSTX                                                          
VCOLST1  CLC   =C'NP ',54(R4)                                                   
         BE    BADCOL                                                           
VCOLSTX  DS    0H                                                               
*                                                                               
         TM    SCR2IND,SCR2YES      IF CONTINUATION SCREEN                      
         BNO   VCOL07                                                           
         GOTO1 =A(OVFLRTN),DMCB,(9,DUB),(RC),RR=PGNR ADJDEMO                    
*                                                                               
VCOL07   CLI   COLXTEND,2          TEST THIS IS AN EXTENSION                    
         BE    VCOL30                                                           
         GOTO1 VCOLDRON            VALIDATE A COLUMN ENTRY                      
         CLI   DRERROR,0                                                        
         BNE   VCOL26                                                           
         CLI   DRATTRIB,C'R'       ROW ONLY ENTRIES NOT ALLOWED                 
         BE    BADCOL                                                           
         CLI   DRATTRIB,C'D'       ROW DETAIL ONLY ENTRIES NOT ALLOWED          
         BE    BADCOL                                                           
         MVI   BYTE,C'C'           STORE COLUMN ENTRY INTO                      
         BAS   RE,BLDENTTB          ENTRYTAB FIRST                              
*                                                                               
* DEAL WITH PREC FOR NON-DEMO COLUMNS                                           
         CLC   =C'AIR',12(R4)      DEMO OVERLAP COLS WILL BE                    
         BE    VCOL100             TRASHED BY PREC RTN WHEN STACKING            
*                                                                               
         CLI   DRATTRIB+1,C'D'     TEST FOR DOLLAR COLUMN                       
         BNE   VCOL08                                                           
         TM    COLIND,COLIRND      YES - TEST FOR ROUNDING OPTION               
         BZ    VCOL100                                                          
         MVI   DRDECO,0            YES - NO DECIMAL PLACES                      
         B     VCOL100                                                          
*                                                                               
VCOL08   MVC   DRDECO,13(R4)                                                    
         NI    DRDECO,X'0F'        ACTUAL DEMO #                                
*                                                                               
* FIND THE DEMO FOR THIS COLUMN                                                 
*                                                                               
         L     RE,PDADEMTB                                                      
         ZIC   R1,DRDECO                                                        
         BCTR  R1,0                                                             
         LTR   R1,R1               FIRST DEMO?                                  
         BZ    VCOL14              YES, DON'T NEED TO SEARCH                    
*                                                                               
VCOL10   CLI   0(RE),X'FF'         END OF DEMOS?                                
         BNE   VCOL12                                                           
         MVI   DRDECO,0            NO SUCH DEMO, USE ZERO                       
         B     VCOL100                                                          
*                                                                               
VCOL12   AH    RE,LNDEMCOD                                                      
         BCT   R1,VCOL10                                                        
*                                                                               
* FOUND DEMO, NOW FIND # OF DECS FOR THAT DEMO                                  
VCOL14   MVC   DRDECO,1(RE)                                                     
*                                                                               
         OC    1(2,RE),1(RE)       COMSCORE DEMO?                               
         JNZ   VCOL15                                                           
         MVI   DRDECO,C'I'         DEFAULT IMPRESSIONS                          
         L     RF,APDNTDMS         COMSCORE DEMO NAMES                          
         ZIC   R1,3(RE)            DEMO OFFSET                                  
         SHI   R1,1                                                             
         MH    R1,=H'8'                                                         
         AR    RF,R1                                                            
         CLI   0(RF),C'X'          IMPRESSIONS?                                 
         BE    VCOL15                                                           
         MVC   DRDECO,0(RF)                                                     
*                                                                               
VCOL15   L     R1,=A(PRECTABL)                                                  
         A     R1,PGNR                                                          
*                                                                               
         USING PRECTDST,R1                                                      
VCOL16   CLI   PRECSRCE,X'FF'      END OF SOURCE PREC TABLE                     
         BE    VCOL24              USE DEFAULT, ZERO                            
         CLC   =C'CNA',PODBOOK+1   CNAD IS ACTUALLY NTI PRECISION               
         BNE   *+14                (CNAW TOO)                                   
         CLC   PRECSRCE,=C'NTI'                                                 
         BE    *+14                                                             
         CLC   PRECSRCE,PODBFIL      LOOK UP SOURCE                             
         BNE   VCOL18                                                           
         L     R1,PRECADDR         GOT IT, LOAD PREC FOR SOURCE                 
         A     R1,PGNR                                                          
         B     VCOL20                                                           
*                                                                               
VCOL18   LA    R1,PRECTLEN(R1)                                                  
         B     VCOL16                                                           
*                                                                               
VCOL20   CLI   0(R1),X'FF'         END OF DEMOS?                                
         BE    VCOL24                                                           
         CLC   DRDECO,0(R1)        FOUND IT?                                    
         BNE   VCOL22                                                           
         MVC   DRDECO,3(R1)        YES, GRAB # OF DECS                          
         B     VCOL100                                                          
*                                                                               
VCOL22   LA    R1,PRECLEN(R1)                                                   
         B     VCOL20                                                           
*                                                                               
VCOL24   MVI   DRDECO,0                                                         
         B     VCOL100                                                          
*                                                                               
VCOL26   XC    BLOCK(42),BLOCK     MAY BE A COMPUTE EXPRESSION                  
         MVI   BLOCK,7                                                          
         MVC   BLOCK+12(30),SPACES                                              
         MVC   BLOCK+12(7),=C'COMPUTE'                                          
         LA    R4,BLOCK                                                         
         GOTO1 VCOLDRON            VALIDATE THE COMPUTE COLUMN                  
         CLI   DRERROR,0                                                        
         BNE   BADCOL                                                           
         LA    R4,BLOCK+42                                                      
         CLI   OFFLINE,C'Y'                                                     
         BE    VCOL28                                                           
         GOTO1 VCMPDRON            VALIDATE A COMPUTE EXPRESSION                
         CLI   DRERROR,0                                                        
         BNE   BADCOL                                                           
*                                                                               
VCOL28   BAS   RE,COMPEDIT         AUTO EDITS FOR COMPUTES                      
         B     VCOL100                                                          
*                                                                               
*                                                                               
* CHECK FOR SUBSIDIARY COLUMN EXPRESSIONS                                       
*                                                                               
VCOL30   TM    2(R4),X'80'         NUMERIC=OUTPUT WIDTH OVERRIDE                
         BNO   VCOL32                                                           
         L     R1,4(R4)            OUTPUT WIDTH OVERRIDE                        
         STC   R1,DRLENO           (NEW OUTPUT LENGTH)                          
         B     VCOL100                                                          
*                                                                               
VCOL32   DS    0H                                                               
         LA    R1,DRHEAD1          CHECK FOR HEADING OVERRIDES                  
         CLC   12(2,R4),=C'H '                                                  
         BE    VCOL34                                                           
         CLC   12(3,R4),=C'H1 '                                                 
         BE    VCOL34                                                           
         LA    R1,DRHEAD2                                                       
         CLC   12(3,R4),=C'H2 '                                                 
         BE    VCOL34                                                           
         LA    R1,DRHEAD3                                                       
         CLC   12(3,R4),=C'H3 '                                                 
         BE    VCOL34                                                           
         LA    R1,DRHEAD4                                                       
         CLC   12(3,R4),=C'H4 '                                                 
         BE    VCOL34                                                           
         CLC   12(3,R4),=C'H5 '    SPECIAL FOR H4 FILTER HEADER                 
         BNE   VCOL36                                                           
*                                                                               
         USING DRHEADD,R1                                                       
VCOL34   XC    0(64,R1),0(R1)      TURN OFF ROUTINE & ARGS                      
         CLI   1(R4),2                                                          
         BL    VCOL100             HN= CAUSES REMOVAL                           
         MVC   DRH1LIT-DRH1ALL(L'DRH1LIT,R1),SPACES  CLEAR OLD HL               
         OI    DRHEAD,X'80'        OTHERWISE TURN IT BACK ON                    
         MVC   DRHLITL,1(R4)       PASS LITERAL LENGTH TO DRONE                 
         CLC   DRHLITL,DRLENO                                                   
         BH    HEADERR             CHECK LITERAL NOT WIDER THAN COLUMN          
*                                                                               
         ZIC   RE,DRLENO                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8              CLEAR THE HEADER LITERAL AREA                
         B     *+10                                                             
         MVC   DRHLIT(0),SPACES    ** EXECUTED                                  
         ZIC   RE,DRHLITL                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8              MOVE IN THE HEADER LITERAL                   
         B     VCOL100                                                          
         MVC   DRHLIT(0),22(R4)    ** EXECUTED                                  
         DROP  R1                                                               
*                                                                               
VCOL36   CLC   12(3,R4),=C'NP '    OPTION NOT TO PRINT                          
         BNE   VCOL38                                                           
         MVI   MYPOSO,C'N'                                                      
*ORIGINALLY ADDED ON 4/28/04 TO FIX STDATA,NP.                                  
*REMOVED ON 9/9/04 BECAUSE IT BREAKS COMPUTATIONS WHEN NP IS PRESENT            
*STDATA,NP AND STD(N),NP NO LONGER ALLOWED                                      
**       NI    DRFLAGO,X'7F'                                                    
         NI    DRHEAD1,X'7F'                                                    
         NI    DRHEAD2,X'7F'                                                    
         NI    DRHEAD3,X'7F'                                                    
         NI    DRHEAD4,X'7F'                                                    
         B     VCOL100                                                          
*                                                                               
VCOL38   CLC   12(3,R4),=C'NT '    OPTION NOT TO TOTAL THIS COLUMN              
         BNE   VCOL40                                                           
         OI    DRARGSI+11,X'80'    12TH ARGUMENT X'80'                          
         MVI   DRNARGSI,16                                                      
         B     VCOL100                                                          
*                                                                               
VCOL40   CLC   12(8,R4),=C'BRACKET '   (MINUS NUMBERS)                          
         BNE   VCOL42                                                           
         OI    DROPTSO,DRBKMINO                                                 
         B     VCOL100                                                          
*                                                                               
VCOL42   CLC   12(2,R4),=C'U '     USER RECORD                                  
         BNE   VCOL44                                                           
         BAS   RE,VUSRDRON                                                      
         B     VCOL100                                                          
*                                                                               
VCOL44   MVI   COLRANK,0                                                        
         CLC   12(4,R4),=C'RANK'   (COLUMN) RANK                                
         BNE   VCOL46                                                           
         OI    CABWFLAG,RANKING                                                 
         BAS   RE,CKAMOUNT                                                      
         TM    SCR2IND,SCR2YES     NO RANK ALLOWED ON 2ND SCREEN                
         BO    BADCOL                                                           
         MVI   COLRANK,1                                                        
         CLI   16(R4),C'-'                                                      
         BE    VCOL100                                                          
         CLI   22(R4),C'-'                                                      
         BE    VCOL100                                                          
         MVI   COLRANK,2                                                        
         B     VCOL100                                                          
*                                                                               
VCOL46   CLC   12(3,R4),=C'DEC'    DECIMAL FOR COMPUTES                         
         BNE   VCOL48                                                           
         CLI   11(R4),0                                                         
         BL    BADCOL                                                           
         CLI   11(R4),5                                                         
         BH    BADCOL                                                           
         MVC   DRDECO,11(R4)                                                    
         B     VCOL100                                                          
*                                                                               
VCOL48   CLC   12(3,R4),=C'NET'    NETWORK FILTER BYTE 8                        
         BE    VCOL50                     - OR -                                
         CLC   12(3,R4),=C'STA'    STATION                                      
         BNE   VCOL54                                                           
*                                                                               
VCOL50   L     R1,=A(VCOLNTTB)                                                  
         A     R1,PGNR                                                          
*                                                                               
VCOL52   CLI   0(R1),X'FF'         (EOL?)                                       
         BE    BADCOL                                                           
         MVC   DRARGSI+7(1),0(R1)                                               
         CLC   22(2,R4),1(R1)      CHECK NET EXPRESSION                         
         BE    VCOL100                                                          
         CLC   22(2,R4),3(R1)      CHECK STATION EXPRESSION                     
         BE    VCOL100                                                          
         LA    R1,5(R1)                                                         
         B     VCOL52                                                           
*                                                                               
VCOL54   CLC   12(4,R4),=C'BOOK'   BOOK FILTER BYTE 13                          
         BNE   VCOL58                                                           
         L     R1,=A(VCOLBKTB)                                                  
         A     R1,PGNR                                                          
*                                                                               
VCOL56   CLI   0(R1),X'FF'         (EOL?)                                       
         BE    BADCOL                                                           
         MVC   DRARGSI+12(1),0(R1)                                              
         CLC   22(2,R4),1(R1)                                                   
         BE    VCOL100                                                          
         LA    R1,3(R1)                                                         
         B     VCOL56                                                           
*                                                                               
VCOL58   CLC   12(3,R4),=C'DAY'    DAY FILTER BYTE 9                            
         BNE   VCOL58A                                                          
         L     R1,=A(VCOLDYTB)                                                  
         A     R1,PGNR                                                          
*                                                                               
VCOL60   CLI   0(R1),X'FF'         (EOL?)                                       
         BE    BADCOL                                                           
         MVC   DRARGSI+8(1),0(R1)                                               
         CLC   22(3,R4),1(R1)                                                   
         BE    VCOL100                                                          
         LA    R1,4(R1)                                                         
         B     VCOL60                                                           
*                                                                               
*                                                                               
VCOL58A  CLC   12(3,R4),=C'F  '    DAY FILTER BYTE 9                            
         BNE   VCOL62                                                           
         L     R1,=A(VCOLF)                                                     
         A     R1,PGNR                                                          
*                                                                               
VCOL60A  CLI   0(R1),X'FF'         (EOL?)                                       
         BE    BADCOL                                                           
         MVC   DRARGSI+6(1),0(R1)                                               
         CLC   22(4,R4),1(R1)                                                   
         BE    VCOL100                                                          
         LA    R1,5(R1)                                                         
         B     VCOL60A                                                          
*                                                                               
VCOL62   CLC   12(3,R4),=C'DPT'    DAYPART FILTER 10                            
         BNE   VCOL66                                                           
         L     R1,=A(VCOLDPTB)                                                  
         A     R1,PGNR                                                          
*                                                                               
VCOL64   CLI   0(R1),X'FF'         (EOL?)                                       
         BE    BADCOL                                                           
         MVC   DRARGSI+9(1),0(R1)                                               
         CLC   22(1,R4),0(R1)                                                   
         BE    VCOL100                                                          
         LA    R1,1(R1)                                                         
         B     VCOL64                                                           
*                                                                               
VCOL66   L     RE,FULL             AVOID PRECISION ADJUSTMENT                   
         CLC   =C'AIR',12(RE)      FOR THIS COLUMN                              
         BE    VCOL67                                                           
         CLI   PRECOPT,C'Y'        PRECISION IS CABLE?                          
         BNE   *+8                 NO                                           
         MVI   DRDECO,2            YES, CHANGE DECIMALS                         
*                                                                               
VCOL67   BAS   RE,VCOLMXMN                                                      
         BE    VCOL100                                                          
         CLC   12(3,R4),=C'SRC'    SOURCE FILTER BYTE 11                        
         BE    VCOL68                                                           
         CLC   12(6,R4),=C'SOURCE'                                              
         BNE   VCOL73                                                           
                                                                                
VCOL68   L     R1,=A(VCOLSCTB)                                                  
         A     R1,PGNR                                                          
*                                                                               
VCOL70   CLI   0(R1),X'FF'         (EOL?)                                       
         BE    BADCOL                                                           
         MVC   DRARGSI+10(1),0(R1)                                              
         CLC   22(5,R4),1(R1)                                                   
         BE    VCOL72                                                           
         LA    R1,6(R1)                                                         
         B     VCOL70                                                           
*                                                                               
VCOL72   BAS   RE,SRC2HEAD         PUT THE SOURCE IN HEADLINE                   
         B     VCOL100                                                          
*                                                                               
VCOL73   CLC   =C'VT',12(R4)       VIEWING TYPE                                 
         BNE   VCOL74                                                           
         L     R1,=A(VCOLVTTB)                                                  
         A     R1,PGNR                                                          
VCOL73A  CLI   0(R1),X'FF'                                                      
         BE    BADCOL                                                           
         MVC   DRARGSI+13(1),0(R1)                                              
         CLC   22(2,R4),1(R1)                                                   
         BE    VCOL73B                                                          
         LA    R1,L'VCOLVTTB(R1)                                                
         B     VCOL73A                                                          
VCOL73B  BAS   RE,VTY2HEAD         PUT THE VIEWING TYPE IN HEADLINE             
         B     VCOL100                                                          
*                                                                               
VCOL74   CLC   =C'MN',12(R4)       MINUTE TYPE                                  
         BNE   VCOL75                                                           
         L     R1,=A(VCOLMTTB)                                                  
         A     R1,PGNR                                                          
VCOL74A  CLI   0(R1),X'FF'                                                      
         BE    BADCOL                                                           
         CLC   22(1,R4),1(R1)                                                   
         BE    VCOL74B                                                          
         LA    R1,L'VCOLMTTB(R1)                                                
         B     VCOL74A                                                          
VCOL74B  OC    DRARGSI+14(1),0(R1)                                              
         BAS   RE,VTY2HEAD         PUT THE MINUTE TYPE IN HEADLINE              
         B     VCOL100                                                          
*                                                                               
VCOL75   CLC   =C'SVT',12(R4)       SOURCE+VIEWING TYPE                         
         BNE   VCOL90                                                           
         L     R1,=A(VCOLSVT)                                                   
         A     R1,PGNR                                                          
         USING VCOLSVTD,R1                                                      
VCOL75A  CLI   0(R1),X'FF'                                                      
         BE    BADCOL                                                           
         CLC   VCSVTIN,22(R4)                                                   
         BE    VCOL75B                                                          
         LA    R1,VCOLSVTL(R1)                                                  
         B     VCOL75A                                                          
VCOL75B  MVC   DRARGSI+10(1),VCSVTSHX    ADD SOURCE FILTER                      
         MVC   DRARGSI+13(1),VCSVTVT     ADD VIEWING TYPE FILTER                
         BAS   RE,VTY2HEAD               PUT SVT IN HEADLINE                    
         B     VCOL100                                                          
         DROP  R1                                                               
*                                                                               
*                                                                               
VCOL90   GOTO1 =A(OVFLRTN),DMCB,(4,DUB),(RC),RR=PGNR VPEREXPR                   
         L     R1,PDAFLBUF                                                      
         USING PDFLBUFF,R1                                                      
         CLI   EXPLIST,0                                                        
         BE    BADCOL                                                           
         CLI   EXPLIST,X'FF'                                                    
         BE    BADCOL                                                           
         MVC   DRARGSI+15(1),EXPLIST                                            
         BAS   RE,HEADFILT                                                      
         DROP  R1                                                               
*                                                                               
VCOL100  LA    R4,42(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,VCOL30                                                        
*                                                                               
         CLI   COLXTEND,1          TEST THERE IS AN EXTENSION PENDING           
         BNE   VCOL110                                                          
         MVI   COLXTEND,2          YES-NOT TIME TO WRAP UP YET                  
         ZIC   R1,5(R2)                PUT BACK ACTUAL LENGTH                   
         LA    R1,1(R1)                                                         
         STC   R1,5(R2)                                                         
         B     VCOLX                                                            
*                                                                               
VCOL110  MVI   COLXTEND,0                                                       
         MVI   DRNARGSI,16         PASS ALL 16 ARGUMENTS                        
         MVI   DRNARGSO,16                                                      
         CLI   MYPOSO,C'N'         IF THERE IS ANY PRINTING                     
         BE    VCOL115                                                          
         LH    R1,TOTWIDTH         ADJUST CURRENT WIDTH                         
         ZIC   RF,DRLENO                                                        
         AR    R1,RF                                                            
         LA    R1,1(R1)                                                         
         STH   R1,TOTWIDTH                                                      
*                                                                               
VCOL115  CLI   OFFLINE,C'Y'        NOW GENERATE ELEMENTS                        
         BNE   VCOL120                                                          
         MVC   DRLABELI,MYLABEL                                                 
         MVC   1(1,R3),DRDECO      SAVE EDIT CHARACTERISTICS                    
         MVC   2(1,R3),DRDIVO                                                   
         MVC   DRPOSO,MYPOSO                                                    
         GOTO1 GCOLDRON                                                         
         CLC   BLOCK+12(7),=C'COMPUTE'                                          
         BNE   VCOL120                                                          
         LA    R4,BLOCK+42                                                      
         GOTO1 GCMPDRON                                                         
         CLI   DRERROR,0                                                        
         BNE   BADCOL                                                           
*                                                                               
VCOL120  BAS   RE,ANYMOCOL                                                      
         L     R1,PDAFLBUF                                                      
         USING PDFLBUFF,R1                                                      
         MVC   EXPLIST(15),EXPLIST+1                                            
         MVI   EXPLIST+15,0                                                     
         CLI   EXPLIST,0                                                        
         BE    VCOLX                                                            
         B     VCOL02                                                           
*                                                                               
VCOLX    B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL ROUTINES FOR VALCOLS                              *         
***********************************************************************         
*                                                                               
       ++INCLUDE DDVALMNMX                                                      
*                                                                     *         
*                                                                     *         
*                                                                     *         
ANYMOCOL NTR1                                                                   
         CLI   COLRANK,0           COLUMN RANKING                               
         BE    ANYMX                                                            
         CLI   OFFLINE,C'Y'                                                     
         BNE   ANYMX                                                            
         XC    BLOCK(12),BLOCK     FOR DATE TOTAL/DETAIL EXPRESSIONS            
         MVI   BLOCK,7                                                          
         MVC   BLOCK+12(30),SPACES                                              
         MVC   BLOCK+12(7),=C'COLRANK'                                          
         LA    R4,BLOCK                                                         
         GOTO1 VCOLDRON                                                         
         MVC   DRARGSI(1),COLRANK       PASS ARGUMENT                           
         MVI   DRNARGSI,1                                                       
         MVI   COLRANK,0                                                        
         GOTO1 GCOLDRON                                                         
*                                                                               
ANYMX    B     XIT                                                              
*                                                                     *         
*                                                                     *         
*                                                                     *         
SETMAX   NTR1                                                                   
         MVI   BYTE,C'A'           FIND LAST INPUT COLUMN                       
*                                                                     *         
SETM02   CLI   5(R2),0                                                          
         BE    SETMX                                                            
         MVC   DRCMPMAX,BYTE                                                    
         BAS   RE,BUMP                                                          
         MVC   BYTE,8(R2)                                                       
         BAS   RE,BUMP                                                          
         BCT   R0,SETM02                                                        
*                                                                     *         
SETMX    B     XIT                                                              
**                                                                              
*              ADD SOURCE TO HEADING                                  *         
**                                                                              
SRC2HEAD NTR1                                                                   
         LR    RF,R4                                                            
         L     R4,FULL             FIRST FIELD IN THE COLUMN                    
*                                                                               
SRCH02   CLI   0(R4),0                                                          
         BNE   SRCH04                                                           
         MVC   0(42,R4),0(RF)      COPY BLOCK                                   
         MVI   0(R4),2             'H5'                                         
         MVI   2(R4),0                                                          
         MVC   12(3,R4),=C'H5 '                                                 
         AH    R0,=H'1'            UPDATE SUB-COLUMN COUNT                      
         B     SRCHX                                                            
*                                                                               
SRCH04   CLC   =C'H4 ',12(R4)                                                   
         BE    SRCHX               HEADER GIVEN BY USER                         
         CLC   =C'H5 ',12(R4)                                                   
         BE    SRCH06              APPEND TO EXISTING FILTER HEADER             
         LA    R4,42(R4)                                                        
         B     SRCH02                                                           
SRCH06   ZIC   RE,1(R4)            LENGTH OF EXISTING HEADER                    
         AR    RE,R4                                                            
         LA    RE,22(RE)           GO PAST THE HEADER                           
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)            ADD NEW HEADER HERE                          
         ZIC   R1,1(RF)            LENGTH OF VTYPE TO ADD                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),22(RF)                                                   
         ZIC   RE,1(R4)            LENGTH OF EXISTING HEADER                    
         ZIC   R1,1(RF)            LENGTH OF VTYPE TO ADD                       
         AR    RE,R1                                                            
         AHI   RE,1                +L'COMMA                                     
         STC   RE,1(R4)                                                         
*                                                                               
SRCHX    XIT1  REGS=(R0)                                                        
**                                                                              
*              ADD VIEWING TYPE TO HEADING                                      
**                                                                              
VTY2HEAD NTR1                                                                   
         LR    RF,R4                                                            
         L     R4,FULL                                                          
*                                                                               
VTY202   CLI   0(R4),0                                                          
         BNZ   VTY204                                                           
         MVC   0(42,R4),0(RF)      COPY BLOCK                                   
         MVI   0(R4),2             'H5'                                         
         MVI   1(R4),2             LENGTH                                       
         MVI   2(R4),0                                                          
         MVC   12(3,R4),=C'H5 '                                                 
         AH    R0,=H'1'            UPDATE SUB-COLUMN COUNT                      
         B     VTYHX                                                            
*                                                                               
VTY204   CLC   =C'H4 ',12(R4)                                                   
         BE    VTYHX                HEADER GIVEN BY USER                        
         CLC   =C'H5 ',12(R4)                                                   
         BE    VTY206               APPEND TO EXISTING FILTER HEADER            
         LA    R4,42(R4)                                                        
         B     VTY202                                                           
VTY206   ZIC   RE,1(R4)            LENGTH OF EXISTING HEADER                    
         AR    RE,R4                                                            
         LA    RE,22(RE)           GO PAST THE HEADER                           
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)            ADD NEW HEADER HERE                          
         ZIC   R1,1(RF)            LENGTH OF VTYPE TO ADD                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),22(RF)                                                   
         ZIC   RE,1(R4)            LENGTH OF EXISTING HEADER                    
         ZIC   R1,1(RF)            LENGTH OF VTYPE TO ADD                       
         AR    RE,R1                                                            
         AHI   RE,1                +L'COMMA                                     
         STC   RE,1(R4)                                                         
*                                                                               
VTYHX    XIT1  REGS=(R0)                                                        
                                                                                
*              FIGURE OUT EDITS FOR COMPUTES                          *         
**                                                                    *         
COMPEDIT NTR1                                                                   
         ZIC   R1,0(R4)            PICK UP EXPRESSION LENGTH                    
         LA    R1,10(R1,R4)                                                     
         CLI   0(R1),C'%'          IS LAST OPERATOR PERCENT?                    
         BE    COMPPCT                                                          
         CLI   0(R1),C'I'          OR INDEX?                                    
         BE    COMPINX                                                          
         BCTR  R1,0                                                             
         CLI   0(R1),C'V'          OR VERTICAL PERCENT                          
         BNE   COMPDEF                                                          
*                                                                               
COMPPCT  MVI   DRDECO,2            PERCENTS HAVE 2 DEC                          
         MVI   DRTRAILO,C'%'       AND END WITH PERCENT SIGN                    
         B     XIT                                                              
*                                                                               
COMPINX  MVI   DRDECO,0            INDEXES HAVE 0 DEC                           
         B     XIT                                                              
*                                                                               
COMPDEF  MVI   DRDECO,0            DEFAULT HAVE 1 DEC                           
*                                                                               
* DEFAULT CALCULATIONS USES THE SAME # OF DEC AS THE FIRST OPERAND              
* SEARCH FOR THE FIRST COLUMN DATA                                              
*                                                                               
         LA    R1,EDITLIST                                                      
COMPDEF1 CLI   0(R1),0             END OF LIST?                                 
         BE    XIT                 YES, KEEP DEFAULT DEC                        
         CLC   0(1,R1),8(R2)       FOUND IT?                                    
         BNE   COMPDEF5            NO, NEXT ONE                                 
         MVC   DRDECO,1(R1)        COPY # OF DEC                                
         CLI   PRECOPT,C'Y'        EXTEND CABLE PRECISION                       
         BNE   XIT                                                              
         ZIC   R3,DRDECO           <----TEMP FIX                                
         LA    R3,1(R3)                 NEED REAL LOGIC FOR THIS                
         STC   R3,DRDECO                                                        
         B     XIT                                                              
COMPDEF5 LA    R1,4(R1)                                                         
         B     COMPDEF1                                                         
*                                                                               
CMPEDFND LA    R1,EDITLIST                                                      
CMPEDFN2 CLI   0(R1),0                                                          
         BNE   *+14                                                             
         LA    RF,1(RF)                                                         
         BCT   R0,CMPEDFND                                                      
         DC    H'0'                NO VARIABLES IN INPUT STRING                 
         CLC   0(1,R1),0(RF)                                                    
         BER   RE                                                               
         LA    R1,4(R1)                                                         
         B     CMPEDFN2                                                         
*                                                                               
*              BUILD ENTRY TABLE                                      *         
*              R4 = SCANNER BLOCK CONTAINING THE ENTRY                *         
*              BYTE = C'C' OR C'R' FOR COLUMN OR ROW                  *         
*                                                                               
BLDENTTB NTR1                                                                   
         L     R3,APODINPL         POINT MYSELF TO TABLE AREA                   
         LA    R5,1(R3)            1ST BYTE IN TABLE = # OF ENTRIES             
         USING ENTRYTBD,R5                                                      
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,0(R3)          R1=# OF ENTRIES SO FAR                       
         LR    R0,R1                                                            
         BZ    *+12                                                             
         LA    R5,ENTRYTBQ(R5)     BUMP TO NEXT SLOT IN TABLE                   
         BCT   R1,*-8                                                           
*                                                                               
         MVC   ETENTRY,12(R4)      MOVE ENTRY INTO TABLE                        
         MVC   ETCOLROW,BYTE       MOVE COL/ROW INDICATOR                       
         MVI   ENTRYTBQ(R5),0      CLEAR THE NEXT SLOT                          
*                                                                               
         LR    R1,R0                                                            
         LA    R1,1(R1)            UPDATE # OF ENTRIES                          
         STC   R1,0(R3)                                                         
*                                                                               
BLDEX    B     XIT                                                              
         DROP  R5                                                               
*                                                                     *         
*              FORMAT HEADINGS FOR FILTERS                            *         
*                                                                     *         
HEADFILT NTR1                                                                   
         TM    DRFLAGO,X'80'       NOT NEEDED IF NOT PRINTING                   
         BNO   HEADX                                                            
*                                                                               
         LA    R1,DRHEAD1          FIND AN EMPTY HEADING                        
         CLC   1(4,R1),=C'HDEM'    DONT SET FOR DEMO HEADER                     
         BE    HEAD06                                                           
         BAS   RE,HEAD02                                                        
         LA    R1,DRHEAD2                                                       
         BAS   RE,HEAD02                                                        
         LA    R1,DRHEAD3                                                       
         BAS   RE,HEAD02                                                        
         LA    R1,DRHEAD4                                                       
         BAS   RE,HEAD02                                                        
*                                                                               
HEADX    B     XIT                 NO ROOM!                                     
*                                                                               
HEAD02   TM    0(R1),X'80'         LOOK FOR EMPTY SLOT                          
         BNO   HEAD04                                                           
         CLC   1(8,R1),=C'HFILTER '     OR A PREVIOUS VISIT                     
         BNER  RE                                                               
*                                                                               
HEAD04   OI    0(R1),X'80'         FOUND A SPACE - SO TURN ON                   
         MVC   1(8,R1),=CL8'HFILTER '                                           
*                                                                               
HEAD06   MVC   9(16,R1),DRARGSI    PASS ALL THE ARGUMENTS                       
         MVI   25(R1),16           N'ARGUMENTS IS 16                            
         B     HEADX                                                            
*                                                                     *         
*              VALIDATE USER RECORD                                   *         
*                                                                     *         
VUSRDRON NTR1                                                                   
         MVI   DRACTION,DRUSER                                                  
         MVC   DRUSRKEY(2),AGENCY           KEY IS AGENCY                       
         MVC   DRUSRKEY+2(8),22(R4)                AND USER CODE                
         GOTO1 DRONE,DMCB,DRGEN                                                 
         CLI   DRERROR,0                                                        
         BNE   USERER                                                           
*                                                                     *         
VUSRX    B     XIT                                                              
*                                                                     *         
*              INSERT/DELETE UNPROTECTED FIELDS                       *         
*              INPUT   R2 = A(FIRST UNPROTECTED FIELD)                *         
*                      R0 = NUMBER OF INPUT FIELDS                    *         
*                                                                     *         
DELINS   NTR1                                                                   
         CLI   OFFLINE,C'Y'                                                     
         BE    XIT                                                              
         CLI   PFAID,3             WAS PF3 OR PF4 HIT                           
         BE    DI2                                                              
         CLI   PFAID,15                (15/16 EQUIVALENT)                       
         BE    DI2                                                              
         CLI   PFAID,4                                                          
         BE    DI2                                                              
         CLI   PFAID,16                                                         
         BNE   XIT                                                              
         SPACE 1                                                                
DI2      L     R4,SYSPARMS                                                      
         L     R4,0(R4)                                                         
         USING TIOBD,R4                                                         
         LH    R4,TIOBCURD         PICK UP RELATIVE DISPLACEMENT                
         A     R4,ATWA             INTO TWA                                     
         SPACE 1                                                                
DI4      CR    R2,R4                                                            
         BE    DI6                                                              
         BAS   RE,BUMPTOUN                                                      
         BCT   R0,DI4                                                           
         B     XIT                 (NOT IN THIS PART OF THE SCREEN)             
         SPACE 1                                                                
DI6      CLI   PFAID,3                                                          
         BE    DEL2                                                             
         CLI   PFAID,15                                                         
         BE    DEL2                                                             
         XC    BLOCK(80),BLOCK                                                  
         SPACE 1                                                                
INS2     MVC   BLOCK+80(80),8(R2)  SAVE THIS FIELD                              
         ZIC   R1,0(R2)            GET L'FIELD-1 INTO R1                        
         SH    R1,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BNO   *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),BLOCK       MOVE IN PREVIOUS (OR CLEAR)                  
         OI    6(R2),X'80'                                                      
         MVC   BLOCK(80),BLOCK+80                                               
         BAS   RE,BUMPTOUN                                                      
         BCT   R0,INS2                                                          
         B     INSFOUND                                                         
*                                                                               
         USING T325FFD,R4                                                       
INSFOUND L     R4,ATWA                                                          
         MVC   CONHEAD(L'INSMESS),INSMESS                                       
         DROP  R4                                                               
         USING TIOBD,R4                                                         
         L     R4,SYSPARMS                                                      
         L     R4,0(R4)                                                         
         LH    R2,TIOBCURD         PICK UP RELATIVE DISPLACEMENT                
         A     R2,ATWA             INTO TWA                                     
         B     VEXIT                                                            
         SPACE 1                                                                
DEL2     LR    R3,R2                                                            
         BAS   RE,BUMPTOUN                                                      
         CLI   5(R2),0                                                          
         BE    DEL4                                                             
         ZIC   R1,0(R3)            GET L'FIELD-1 INTO R1                        
         SH    R1,=H'9'                                                         
         TM    1(R3),X'02'                                                      
         BNO   *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),8(R2)       MOVE NEXT INTO THIS                          
         OI    6(R3),X'80'                                                      
         BCT   R0,DEL2                                                          
         SPACE 1                                                                
DEL4     EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R3),8(R3)       CLEAR LAST ONE                               
         OI    6(R3),X'80'                                                      
         LR    R2,R3                                                            
         USING T325FFD,R4                                                       
         L     R4,ATWA                                                          
         MVC   CONHEAD(L'DELMESS),DELMESS                                       
         B     VEXIT                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PERIOD EXPRESSIONS                                  *         
*              INPUTS : P1 = A(INPUT EXPRESSION)                      *         
*                       P2 = A(PERIOD S/E)                            *         
***********************************************************************         
*                                                                               
VVALPEX  LM    R2,R3,0(R1)                                                      
         XC    0(4,R3),0(R3)                                                    
         CLI   PDQPERHI,X'FF'      CHECK FOR PERIOD IN ROWS                     
         BE    VPERXNE             YES                                          
         CLI   0(R2),C'Q'          QUARTERS 1-5                                 
         BNE   VPER02                                                           
         CLI   2(R2),C' '                                                       
         BNE   VPERXNE                                                          
         CLI   1(R2),C'1'                                                       
         BL    VPERXEQ                                                          
         CLI   1(R2),C'5'                                                       
         BH    VPERXEQ                                                          
         TM    PDQPER,PDQPMN+PDQPWK  DON'T MIX PERIODS                          
         BNZ   VPERXEQ                                                          
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'0F'                                                       
         CLI   PDQPERLO,0                                                       
         BE    *+14                                                             
         CLC   BYTE,PDQPERLO                                                    
         BNL   *+10                                                             
         MVC   PDQPERLO,BYTE                                                    
         CLC   BYTE,PDQPERHI                                                    
         BNH   *+10                                                             
         MVC   PDQPERHI,BYTE                                                    
         ZIC   RF,BYTE                                                          
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         L     RE,AQTRS                                                         
         LA    RE,0(RE,RF)                                                      
         ST    RE,0(R3)                                                         
         OI    PDQPER,PDQPQT                                                    
         B     VPERXEQ                                                          
*                                                                               
VPER02   CLI   0(R2),C'M'          MONTHS 1-15                                  
         BNE   VPER04                                                           
         TM    PDQPER,PDQPQT+PDQPWK  DON'T MIX PERIODS                          
         BNZ   VPERXEQ                                                          
         L     RF,AMONTHS                                                       
         MVI   BYTE,C'5'                                                        
         BAS   RE,CHKPER                                                        
         OI    PDQPER,PDQPMN                                                    
         B     VPERXEQ                                                          
*                                                                               
VPER04   CLI   0(R2),C'W'          WEEKS 1-13                                   
         BNE   VPERXNE                                                          
         TM    PDQPER,PDQPQT+PDQPMN  DON'T MIX PERIODS                          
         BNZ   VPERXEQ                                                          
         L     RF,AWEEKS                                                        
         MVI   BYTE,C'3'                                                        
         BAS   RE,CHKPER                                                        
         OI    PDQPER,PDQPWK                                                    
*                                                                               
VPERXEQ  CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
VPERXNE  LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL VALPEX ROUTINES                                   *         
***********************************************************************         
*                                                                               
CHKPER   LR    R0,RE                                                            
         CLI   1(R2),C'1'                                                       
         BL    VPERXNE                                                          
         CLI   1(R2),C'9'                                                       
         BH    VPERXNE                                                          
         CLI   3(R2),C' '                                                       
         BNE   VPERXEQ                                                          
         LA    RE,X'70'                                                         
         CLI   2(R2),C' '                                                       
         BNH   CHKP02                                                           
         CLI   1(R2),C'1'                                                       
         BNE   VPERXEQ                                                          
         CLI   2(R2),C'0'                                                       
         BL    VPERXEQ                                                          
         CLC   2(1,R2),BYTE                                                     
         BH    VPERXEQ                                                          
         LA    RE,1(RE)                                                         
*                                                                               
CHKP02   EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  WORK(0),1(0,R2)                                                  
         CVB   R1,WORK                                                          
         CLI   PDQPERLO,0                                                       
         BE    *+12                                                             
         CLM   R1,1,PDQPERLO                                                    
         BNL   *+8                                                              
         STC   R1,PDQPERLO                                                      
         CLM   R1,1,PDQPERHI                                                    
         BNH   *+8                                                              
         STC   R1,PDQPERHI                                                      
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LR    RE,RF                                                            
         LA    RE,0(RE,R1)                                                      
         ST    RE,0(R3)                                                         
*                                                                               
CHKPX    LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        DRONE UTILITIES                                              *         
***********************************************************************         
*                                                                               
VINTDRON MVI   DRWHO,DRNETWHO      INITIALIZATION                               
         MVI   DRACTION,DRINIT                                                  
         MVC   DRDICT,=CL8'PDWRI'                                               
         MVC   DRALTDIC,=CL8'DRIVER'                                            
         MVC   DRCOMFAC,ACOMFACS                                                
         MVC   DRMAXWID,=H'999'    FORCE BIG - I CHECK WIDTH                    
         MVI   DRCMPMAX,C'N'       MAX COL FOR COMPS                            
         GOTO1 DRONE,DMCB,DRGEN                                                 
*                                                                               
VINTX    B     XIT                                                              
*                                                                               
VVRWDRON MVI   DRACTION,DRROW      VALIDATE A ROW                               
         B     ALLVAL                                                           
*                                                                               
VGRWDRON MVI   DRACTION,DRGENROW   GENERATE A ROW                               
         B     ALLDRONE                                                         
*                                                                               
VVCLDRON MVI   DRACTION,DRCOL      VALIDATE A COLUMN                            
         B     ALLVAL                                                           
*                                                                               
VGCLDRON MVI   DRACTION,DRGENCOL   GENERATE A COLUMN                            
         B     ALLDRONE                                                         
*                                                                               
VVCMDRON MVI   DRACTION,DRCMP      VALIDATE A COMP                              
         B     ALLVAL                                                           
*                                                                               
VGCMDRON MVI   DRACTION,DRGENCMP   GENERATE A COMP                              
         B     ALLVAL              (LOOKS LIKE A VALIDATION)                    
*                                                                               
VGUSDRON MVI   DRACTION,DRUSER     GENERATE USER ELEMENTS                       
         B     ALLDRONE                                                         
*                                                                               
VWRPDRON MVI   DRACTION,DRWRAPUP   WRAP UP                                      
         GOTO1 DRONE,DMCB,DRGEN                                                 
         BAS   RE,TRACDRON         (OPTIONAL TRACE)                             
*                                                                               
VWRPX    B     XIT                                                              
*                                                                               
*                                                                               
ALLVAL   XC    WORK,WORK           GENERATE A PSEUDO TWA HEADER                 
         MVC   WORK+5(1),0(R4)     (PASS THROUGH THE LENGTH)                    
         MVC   WORK+8(30),12(R4)                                                
         LA    R1,WORK                                                          
         ST    R1,DRACCFLD                                                      
         OI    DRFLAGS,DREXPDIC    TELL DRONE TO EXPLODE DICT.                  
         GOTO1 DRONE,DMCB,DRGEN                                                 
*                                                                               
ALLVX    B     XIT                                                              
*                                                                               
*                                                                               
ALLDRONE GOTO1 DRONE,DMCB,DRGEN                                                 
*                                                                               
ALLDX    B     XIT                 USER NEEDS TO TEST DRERROR                   
         EJECT                                                                  
***********************************************************************         
*        TRACE OPTION                                                 *         
***********************************************************************         
*                                                                               
TRACDRON NTR1                                                                   
         CLI   TRACEOPT,C'Y'       DRONE TRACING OPTION                         
         BNE   TRACX                                                            
         CLI   OFFLINE,C'Y'                                                     
         BNE   TRACX                                                            
         L     R3,ADPGPROG                                                      
*                                                                               
TRAC02   CLI   0(R3),0                                                          
         BE    TRACX                                                            
         ZIC   R4,1(R3)                                                         
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R3)                                                       
         LA    R4,1(R4)                                                         
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         GOTO1 HEXOUT,DMCB,(R3),BLOCK,(R4),=C'SEP'                              
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),BLOCK                                                       
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         LA    R5,BLOCK+1(R4)                                                   
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R5)                                                       
         BASR  RE,RF                                                            
         MVC   P,SPACES                                                         
         BASR  RE,RF                                                            
         LA    R3,1(R3,R4)                                                      
         B     TRAC02                                                           
*                                                                               
TRACX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        INITIALIZE DRIVER                                            *         
***********************************************************************         
*                                                                               
VINTDRIV GOTO1 =A(INTDRIV),DMCB,(R9),(RC),RR=PGNR                               
         L     RE,APODOBSQ                                                      
         XC    0(L'PODOBSQ,RE),0(RE)                                            
*                                                                               
VINTDX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        GENERAL HEADLINE HOOK ROUTINES                               *         
***********************************************************************         
*                                                                               
VGENHEAD L     R3,AH4              SET TO HEADLINE 4                            
         MVC   1(60,R3),0(R2)      MOVE BOOK FIELD TO HEADLINE                  
*                                                                               
         LHI   R0,60               REPLACE 0'S BY SPACES                        
         LA    R3,1(R3)                                                         
GENHEAD1 CLI   0(R3),0                                                          
         BNE   *+8                                                              
         MVI   0(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         BCT   R0,GENHEAD1                                                      
*                                                                               
         L     R2,AH1              DEAL WITH MAIN TITLE                         
         LA    R2,32(R2)                                                        
         CLI   WIDEOPT,C'Y'                                                     
         BNE   *+8                                                              
         LA    R2,16(R2)                                                        
         MVC   0(64,R2),TITLE      (TITLES ARE ALREADY CENTERED)                
         A     R2,PWIDTH                                                        
         GOTO1 UNDERLIN,DMCB,(64,TITLE),(X'BF',(R2))                            
*                                                                               
         CLC   =C'REN',PODBEXT     PRINT COPYRIGHT MESSAGE FOR RENTRAK          
         BE    GENH1_05                                                         
*                                                                               
         L     RE,APODBKL                                                       
         USING PODBD,RE                                                         
GENH1_01 CLI   0(RE),X'FF'         END OF SOURCE/BOOK TABLE?                    
         BE    GENH1_10            YES. NOT NIELSEN SOURCES                     
         L     R5,=A(BOOKTAB)                                                   
         A     R5,PGNR                                                          
GENH1_02 CLI   0(R5),X'FF'                                                      
         BNE   *+6                 JUST IN CASE. SOURCE SHOULD BE VALID         
         DC    H'0'                                                             
         CLC   0(5,R5),PODBEXT                                                  
         BE    GENH1_03                                                         
         LA    R5,BOOKTABL(R5)                                                  
         B     GENH1_02                                                         
GENH1_03 CLI   5(R5),C'N'          IS THIS A NIELSEN SOURCE?                    
         BE    GENH1_05            YES. GO PRINT COPYRIGHT INFO                 
         CLI   0(RE),0             IF RANGE BUMP TWICE                          
         BE    *+8                                                              
         LA    RE,PODBLNQ(RE)      BUMP BOOK TABLE                              
         LA    RE,PODBLNQ(RE)      BUMP BOOK TABLE                              
         B     GENH1_01                                                         
         DROP  RE                                                               
GENH1_05 A     R2,PWIDTH           H3                                           
         A     R2,PWIDTH           H4  PRINT COPYRIGHT INFO HERE                
         MVC   WORK(30),=C'RTG/IMP-(C) YYYY NIELSEN MEDIA'                      
         CLC   =C'REN',PODBEXT     COPYRIGHT MESSAGE FOR RENTRAK                
         BNE   *+10                                                             
         MVC   WORK(30),=C'RTG/IMP-(C) YYYY RENTRAK      '                      
*                                                                               
         TM    OPTFLAG1,COMSRCOQ   COM SOURCE ONLY REQUEST?                     
         BZ    *+14                                                             
         MVC   WORK(30),=C'Copyright @ YYYY comScore Inc.'                      
         B     GENH1_07                                                         
         TM    OPTFLAG1,COMSRCQ    COM SOURCE REQUEST?                          
         BZ    GENH1_07                                                         
         MVC   WORK(11),=C'Copyright @'                                         
         MVC   WORK+30(15),=C', comScore Inc.'                                  
*                                                                               
GENH1_07 GOTO1 DATCON,DMCB,(5,DUB),(20,DUB)                                     
         MVC   WORK+12(4),DUB      PUT YEAR HERE                                
         GOTO1 CENTER,DMCB,WORK,64                                              
         MVC   0(64,R2),WORK       CENTERED COPYRIGHT MESSAGE                   
*                                                                               
GENH1_10 CLI   PDMETHOD,3                                                       
         BL    GENHEAD2                                                         
         L     R2,AH3                                                           
         MVC   1(12,R2),=C'METHODOLOGY='                                        
         CLI   PDMETHOD,3                                                       
         BNE   *+10                                                             
         MVC   13(6,R2),=C'NETPAK'                                              
         CLI   PDMETHOD,4                                                       
         BE    *+8                                                              
         CLI   PDMETHOD,5          METHOD=SOURCE FOR NSI                        
         BNE   *+10                                                             
         MVC   13(6,R2),=C'SOURCE'                                              
         CLI   PDMETHOD,6                                                       
         BNE   *+10                                                             
         MVC   13(18,R2),=C'AUDIENCE ESTIMATOR'                                 
GENHEAD2 DS    0C                                                               
*                                                                               
         L     R2,AH5              LINE UP THE LEFT SIDE                        
         LA    R2,1(R2)                                                         
         LA    R3,15                                                            
         L     R1,AGLOBAL                                                       
         USING GLOBALD,R1                                                       
         ZIC   R4,GLFHEADL                                                      
         DROP  R1                                                               
         SH    R4,=H'7'                                                         
         BAS   RE,GETLONG                                                       
         LA    R3,16(R2)                                                        
         A     R2,FULL                                                          
         LA    R2,2(R2)                                                         
         BAS   RE,SHUFFLE                                                       
         LA    R3,12                                                            
         BAS   RE,GETLONG                                                       
         LA    R3,13(R2)                                                        
         A     R2,FULL                                                          
         LA    R2,2(R2)                                                         
         BAS   RE,SHUFFLE                                                       
*                                                                               
VGENX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL GENHEAD ROUTINES                                  *         
***********************************************************************         
**                                                                              
*        GET LONGEST FIELD                                            *         
*              INPUT : R2 = A(FIELD ON FIRST LINE)                    *         
*                      R3 = MAX WIDTH                                 *         
*                      R4 = NUMBER OF LINES                           *         
*              OUTPUT: FULL = WIDEST FOUND                            *         
**                                                                              
GETLONG  NTR1                                                                   
GETL02   ST    R3,FULL                                                          
         LTR   R3,R3                                                            
         BZ    GETLX                                                            
         LA    R1,0(R2,R3)                                                      
         BCTR  R1,0                R1=END OF PRESENT FIELD                      
         LR    R0,R4                                                            
*                                                                               
GETL04   CLI   0(R1),C' '          SEE IF ANYTHING SIGNIFICANT                  
         BH    GETLX                                                            
         A     R1,PWIDTH           ON EACH OF THE LINES                         
         BCT   R0,GETL04                                                        
         BCTR  R3,0                                                             
         B     GETL02                                                           
*                                                                               
GETLX    B     XIT                                                              
         EJECT                                                        *         
* INPUT  : R2=A(START DATA ON FIRST LINE)                                       
*          R3=A(FROM DATA)                                                      
*          R4=NUMBER OF LINES                                                   
*                                                                     *         
*                                                                     *         
*                                                                     *         
SHUFFLE  NTR1                                                                   
*                                                                               
SHUF02   MVC   0(50,R2),0(R3)                                                   
         A     R2,PWIDTH                                                        
         A     R3,PWIDTH                                                        
         BCT   R4,SHUF02                                                        
*                                                                               
SHUFX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        GENERATE EDICT CONTROL CARDS FOR TRANSMIT REPORT             *         
***********************************************************************         
*                                                                               
VGENEDCT L     R4,ATWA                                                          
         USING T325FFD,R4                                                       
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
GENED2   CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
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
GENED6   MVC   P,SPACES            *HDR* CARD                                   
         LA    R1,P                                                             
         MVC   4(5,R1),=C'*HDR*'                                                
         MVC   9(6,R1),=C'EDICT='                                               
         MVC   15(10,R1),WORK      DESTINATION                                  
         MVI   34(R1),C'W'         132 CHARS WIDE                               
         MVC   38(10,R1),WORK      FORMATTED DESTINATION NAME                   
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         MVC   P,SPACES            ++DDS TRN CARD                               
         MVC   P(14),=CL14'++DDS NEPODTRN'                                      
         MVC   P+6(2),RCPROG       CHANGE SYSTEM TO NE, RE OR SP                
         MVC   P+9(2),SPLNAM                                                    
         LA    R1,P+15                                                          
         USING SPEDICTD,R1                                                      
         MVI   SPWRTYPE,SPWRDATQ                                                
         MVC   SPWRNAME,SPLNAM                                                  
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
*                                                                               
GENEDX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        OTHER DATA HANDLING ROUTINES                                 *         
***********************************************************************         
*                                                                               
VNUMERIC TM    4(R2),X'08'                                                      
         BO    VPACK                                                            
         MVI   ERROR,NOTNUM                                                     
         B     VEXIT                                                            
*                                                                               
VPACK    SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         SR    R1,R1                                                            
         ZAP   DUB,=P'0'                                                        
         LTR   R3,R3                                                            
         BZ    VPACKX                                                           
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
*                                                                               
VPACKX   XIT1  REGS=(R1)                                                        
         EJECT                                                                  
***********************************************************************         
*        POSITION CURSOR TO CORRECT FIELD FOR ERRORS                  *         
*              INPUT : R2 = A(SCREEN HEADER)                          *         
*                      FIELDERR = NUMBER OF FIELD IN ERROR            *         
***********************************************************************         
*                                                                               
VCURSERR CLI   FIELDERR,0          APPLICATION MUST SET FIELD NUMBER            
         BE    VERRXIT                                                          
         CLI   OFFLINE,C'Y'                                                     
         BE    VERRXIT                                                          
         L     R4,ATIOB                                                         
         USING TIOBD,R4                                                         
         OI    6(R2),X'80'         TRANSMIT ERROR FIELD HEADER                  
         OI    TIOBINDS,TIOBSETC   INSTRUCT CURSOR SETTING                      
         LR    RF,R2                                                            
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD       DISPLACEMENT FROM START OF TWA               
         LA    RE,8(R2)                                                         
         SR    R1,R1               COMPUTE FIELD DISPLACEMENT INTO R1           
         SR    R0,R0                                                            
         ICM   R0,1,5(R2)          R0 HAS FIELD LENGTH                          
         BZ    CURS05                                                           
         ZIC   RF,FIELDERR                                                      
         BCT   RF,CURS02           CHECK IF ERROR IS IN FIELD 1                 
         B     CURS04                                                           
*                                                                               
CURS02   CLI   0(RE),C','          SCAN FOR THE COMMAS                          
         BNE   CURS04                                                           
         BCT   RF,CURS04                                                        
         LA    R1,1(R1)            FOUND ENOUGH - SPACE PAST LAST               
         B     CURS06                                                           
*                                                                               
CURS04   LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,CURS02                                                        
*                                                                               
CURS05   SR    R1,R1               ERROR - DIDN'T FIND ENOUGH COMMAS            
*                                                                               
CURS06   STC   R1,TIOBCURI         SET CURSOR DISPLACEMENT WITHIN FIELD         
         B     VEXIT                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE INTERFACES TO GENCON'S ERREX ROUTINE AND SETS SPECIAL  *         
* PARAMETERS TO MAKE IT CALL GETTXT INSTEAD OF GETMSG. SINCE WUNDERMAN*         
* USES MESSAGE NUMBERS GREATER THAN 255, GETTXT MUST BE USED BECAUSE  *         
* GETMSG IS NOT DESIGNED TO HANDLE THESE NUMBERS.                     *         
***********************************************************************         
*                                                                               
VPDSERR  OI    GENSTAT2,USGETTXT   FLAGS ERREX TO CALL GETTXT                   
         MVC   GTMSGNO,PERROR      MESSAGE NUMBER                               
         MVC   GTMTYP,PMSGTYPE     MESSAGE TYPE                                 
*                                                                               
         CLC   PERROR,=H'60'       IF MESSAGE NUMBER <= 60                      
         BH    *+8                                                              
         MVI   GTMSYS,X'FF'        USE GENERAL SYSTEM                           
*                                                                               
         CLC   PERROR,=H'300'      IF MESSAGE NUMBER >= 300                     
         BL    *+8                                                              
         MVI   GTMSYS,24           USE SYSTEM 24 MESSAGES                       
*                                                                               
         TM    SCR2IND,SCR2YES                                                  
         BNO   *+14                                                             
         L     R2,ACONTOPT                                                      
         MVC   FIELDERR,SVFLDER                                                 
         GOTO1 CURSERR                                                          
         EJECT                                                                  
BUMP     ZIC   RF,0(R2)            GET TO NEXT SCREEN FIELD                     
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
BUMPTOUN ZIC   RF,0(R2)            GET TO NEXT UNPROTECTED FIELD                
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         DROP  R4                                                               
*                                                                               
         GETEL (R5),DATADISP,ELCODE                                             
         EJECT                                                                  
***********************************************************************         
*        ERROR ROUTINES                                               *         
***********************************************************************         
*                                                                               
BADFILT  MVC   PERROR,=AL2(INVFEX)                                              
         B     FILTERR                                                          
*                                                                               
BADFILT2 MVC   PERROR,=AL2(TOOMANY)                                             
         B     FILTERR                                                          
*                                                                               
BADPREF  MVC   PERROR,=AL2(INVFPR)                                              
         B     FILTERR                                                          
*                                                                               
BADFMX8  MVC   PERROR,=AL2(MAX8FIL)                                             
         B     FILTERR                                                          
*                                                                               
FILTERR  L     R1,ATWA                                                          
         USING T325FFD,R1                                                       
         LA    R2,SPLFILTH                                                      
         B     PODERR1                                                          
         DROP  R1                                                               
*                                                                               
OPTCON   MVC   PERROR,=AL2(MRKCON)                                              
         B     PODERR1                                                          
*                                                                               
OPTCON2  MVC   PERROR,=AL2(MRGCON)                                              
         B     PODERR1                                                          
*                                                                               
FILCON   MVC   PERROR,=AL2(NTICON)                                              
         B     PODERR1                                                          
*                                                                               
BADDEM   MVC   PERROR,=AL2(INVDEM)                                              
         B     PODERR1                                                          
*                                                                               
BADDEM2  MVC   PERROR,=AL2(IMPRTG)                                              
         B     PODERR1                                                          
*                                                                               
INVPROG  MVC   PERROR,=AL2(INVFORM)                                             
         B     PODERR1                                                          
*                                                                               
HEADERR  MVC   PERROR,=AL2(WIDOVR)                                              
         B     PODERR1                                                          
*                                                                               
BADROW   MVC   PERROR,=AL2(INVROW)                                              
         B     PODERR1                                                          
*                                                                               
BADROW1  MVC   PERROR,=AL2(ALSTASYS) ALL-STATION NEEDS SYSCODE FILTER           
         B     PODERR1                                                          
*                                                                               
BADSTROW MVC   PERROR,=AL2(NOSTAK)                                              
         B     PODERR1                                                          
*                                                                               
BADLRANK MVC   PERROR,=AL2(NOLAST)                                              
         B     PODERR1                                                          
*                                                                               
BADLEN   MVC   PERROR,=AL2(NOCLEN)                                              
         B     PODERR1                                                          
*                                                                               
BADCOL   MVC   PERROR,=AL2(INVCOL)                                              
         B     PODERR1                                                          
*                                                                               
RANKERR  MVC   PERROR,=AL2(INVRNK)                                              
         B     PODERR1                                                          
*                                                                               
BADNEED1 MVC   PERROR,=AL2(NEED1)                                               
         B     PODERR1                                                          
*                                                                               
USERER   MVC   PERROR,=AL2(NOUSER)                                              
         B     PODERR1                                                          
*                                                                               
PERERR2  MVC   PERROR,=AL2(MISSPER)                                             
         B     PODERR1                                                          
*                                                                               
BADDAY   MVI   ERROR,INVDAY        INVALID DAY                                  
***      B     VEXIT                                                            
         L     R2,SAVER2           RESTORE R2 FOR CURSOR                        
         GOTO1 CURSERR             SET CURSOR AND EXIT                          
*                                                                               
BADTIME  MVI   ERROR,INVTIME                                                    
         L     R2,SAVER2           RESTORE R2 FOR CURSOR                        
***      B     VEXIT                                                            
         GOTO1 CURSERR             SET CURSOR AND EXIT                          
*                                                                               
BADTIME2 MVC   PERROR,=AL2(TALLERR)                                             
         L     R2,SAVER2           RESTORE R2 FOR CURSOR                        
         B     PODERR1                                                          
*                                                                               
BADTIME3 MVC   PERROR,=AL2(ITNERR)                                              
         L     R2,SAVER2           RESTORE R2 FOR CURSOR                        
         B     PODERR1                                                          
*                                                                               
BADTIME4 MVC   PERROR,=AL2(SINERR)                                              
         L     R2,SAVER2           RESTORE R2 FOR CURSOR                        
         B     PODERR1                                                          
*                                                                               
MISSPRG  MVC   PERROR,=AL2(NOPRGERR)                                            
         L     R2,ALASTCOL                                                      
         B     PODERR1                                                          
*                                                                               
MISSDT   MVC   PERROR,=AL2(NODTERR)                                             
         L     R2,ALASTCOL                                                      
         B     PODERR1                                                          
*                                                                               
MISSNET  MVC   PERROR,=AL2(NONETERR)                                            
         L     R2,ALASTCOL                                                      
         B     PODERR1                                                          
*                                                                               
PROGERR  MVC   PERROR,=AL2(PRGERR)                                              
         L     R2,ALASTCOL                                                      
         B     PODERR1                                                          
*                                                                               
TOOMCHER MVC   PERROR,=AL2(TOMCHERR)                                            
         B     PODERR1                                                          
TOOMCHE2 MVC   PERROR,=AL2(TOMCHER2)                                            
         B     PODERR1                                                          
*                                                                               
BADMED   MVI   ERROR,INVMED                                                     
         B     VEXIT                                                            
*                                                                               
BADDRONE L     R1,ATWA             ERROR - SO SHOW WHAT DRONE PASSED            
         USING T325FFD,R1                                                       
         MVC   CONHEAD,DRERRMSG                                                 
         MVI   ERROR,X'FE'                                                      
         GOTO1 CURSERR                                                          
         DROP  R1                                                               
*                                                                               
DTOVER   L     R1,ATWA                                                          
         USING T325FFD,R1                                                       
         MVC   CONHEAD(L'INVDTOV),INVDTOV    ERROR                              
         L     R2,SAVER2           RESTORE R2 FOR CURSOR                        
         B     MYEND                                                            
*                                                                               
COLWIDE  L     R1,ATWA                                                          
         USING T325FFD,R1                                                       
*        MVC   CONHEAD(36),=C'REPORT HAS NNN CHARACTERS - TOO WIDE'             
*        LA    R3,CONHEAD+11                                                    
         MVC   CONHEAD(L'WIDECOL),WIDECOL                                       
         LA    R3,CONHEAD+19                                                    
         EDIT  (2,TOTWIDTH),(3,(R3))                                            
         L     R2,ALASTCOL                                                      
         B     MYEND                                                            
*                                                                               
BADCOLM  MVC   PERROR,=AL2(MANYCOL)                                             
         L     R2,ALASTCOL                                                      
         B     PODERR1                                                          
*                                                                               
PODERR1  MVI   PMSGTYPE,C'E'                                                    
         GOTO1 VPDSERR                                                          
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*        COMMON EXIT ROUTINES                                         *         
***********************************************************************         
*                                                                               
MYEND    MVI   ERROR,X'FE'                                                      
*                                                                               
VEXIT    OI    6(R2),X'40'         POSITION CURSOR                              
*                                                                               
VERRXIT  DS    0H                  IF ERROR IN CONTINUATION SCREEN              
         TM    SCR2IND,SCR2YES     DISPLAY ORIGINAL SCREEN                      
         BNO   VERRXITT            WITH A GENERAL ERROR MESSAGE                 
         GOTO1 =A(OVFLRTN),DMCB,(10,DUB),(RC),RR=PGNR SETERROR                  
         L     R2,ACONTOPT                                                      
         OI    6(R2),X'40'         POSITION CURSOR                              
VERRXITT CLI   ERROR,X'FE'                                                      
         BE    VEXIT2                                                           
         GOTO1 ERREX               PUT OUT SYSTEM MESSAGE                       
*                                                                               
VEXIT2   GOTO1 ERREX2              PUT OUT CUSTOM MESSAGE                       
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS, TABLES, ETC.                                      *         
***********************************************************************         
*                                                                               
INVNTBK  DC    C'EX/0118 INVALID BOOK NETWORK COMBINATION'                      
INVDTOV  DC    C'EX/0119 DAY AND TIME OVERLAP REQUIRE DAYPART OPTION'           
INSMESS  DC    C'EX/0204 NEW FIELD INSERTED ON SCREEN'                          
DELMESS  DC    C'EX/0205 FIELD DELETED ON SCREEN'                               
WIDECOL  DC    C'EX/0206 REPORT HAS NNN CHARACTERS - TOO WIDE'                  
SOONERR  DC    C'EX/0207 ZZZ-C OVER 3 MONTHS MUST REQUEST OVERNIGHT'            
*                                                                               
RELOC    DC    A(*)                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
VALTITLE NTR1  BASE=*,LABEL=*                                                   
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(22),=C'RESEARCH REPORT WRITER'                             
         CLI   5(R2),0                                                          
         JE    XIT                                                              
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
         J     XIT                                                              
         LTORG                                                                  
*                                                                     *         
*        GET COMSCORE TOKEN RECORD                                    *         
*        ON EXIT - DMCB PARAM5 BUILT FOR DEMOVAL                      *         
*                                                                     *         
GETTOKEN NTR1  BASE=*,LABEL=*                                                   
         L     R2,ATWA                                                          
         USING T325FFD,R2                                                       
         MVC   HALF,TWAAGY         SAVE OFF THE AGENCY ALPHA                    
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CT5KEY,R4                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,TWAAGY                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',KEY,AIO1                
         L     R4,AIO1                                                          
         LA    R5,CT5DATA          POINT TO THE FIRST ELEMENT IN REC            
         USING CTSEAD,R5                                                        
         CLI   0(R5),CTSEAELQ      ALREADY ON SECURITY ALPHA ELEM?              
         JE    GTOKEN10            YES                                          
         MVI   ELCODE,CTSEAELQ     NO, FIND SECURITY ALPHA ELEM                 
         BRAS  RE,NEXTEL                                                        
         JNE   *+10                                                             
GTOKEN10 MVC   HALF,CTSEAAID       GET SECURITY AGENCY ALPHA AS WELL            
         DROP  R5                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TOKRECD,R4          BUILD TOKEN RECORD KEY                       
         MVI   TOKKMIN,TOKKMINQ                                                 
         MVI   TOKKTYP,TOKKRTRK                                                 
         MVC   TOKKSAGY,HALF       SECURITY AGENCY ALPHA                        
         MVC   TOKKAAGY,TWAAGY     AND AGENCY ALPHA                             
         MVI   TOKKSYS,X'03'       NET SYSTEM                                   
         MVC   KEYSAVE,KEY                                                      
                                                                                
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI '),=C'GENDIR ',KEY,KEY                 
         CLC   KEY(L'TOKKEY),KEYSAVE                                            
         JNE   GTOKENX                                                          
                                                                                
         L     R4,AIO1                                                          
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'GENFIL ',KEY+36,(R4),WORK             
                                                                                
         AHI   R4,TOKFIRST                                                      
         USING RTAUTHD,R4                                                       
         CLI   0(R4),RTAUTELQ      X'0A' ELEMENT                                
         JNE   GTOKENX                                                          
         OC    RTAUTID(L'RTAUTID+L'RTAUTSEC),RTAUTID                            
         JZ    GTOKENX                                                          
*                                                                               
         L     RF,AIO1                                                          
         USING P5XD,RF                                                          
         XC    P5XID(P5XDLNQ),P5XID                                             
         MVC   P5XID,=C'P5X '                                                   
         LA    RE,RTAUTID          USER TOKEN FOR DEMOVAL                       
         ST    RE,P5XLICNS                                                      
         ST    RF,PARAS+16          PARAM5 EXTEND BLOCK                         
         OI    PARAS+16,X'80'                                                   
*                                                                               
GTOKENX  J     XIT                                                              
         DROP  R2,R4,RF                                                         
         LTORG                                                                  
*                                                                     *         
*        LOAD UP HUT VALUES                                           *         
*                                                                     *         
HUTINIT  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,HUTVALS                                                       
         LA    R4,9                                                             
*                                                                               
HUTI02   XC    0(96,R3),0(R3)                                                   
         LA    R3,96(R3)                                                        
         BCT   R4,HUTI02                                                        
*                                                                               
HUTIX    J     XIT                                                              
         LTORG                                                                  
*                                                                     *         
*        GET QUARTERS                                                 *         
*                                                                     *         
GETQ     NTR1  BASE=*,LABEL=*                                                   
         LH    R1,0(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         CH    R1,=H'6'                                                         
         BNL   *+8                                                              
         LA    R1,24(R1)                                                        
         SH    R1,=H'6'                                                         
         SLL   R1,2                                                             
         LR    R2,R1                                                            
         SRDA  R0,32                                                            
         D     R0,=F'15'                                                        
         AR    R2,R1                                                            
         STC   R2,0(R3)                                                         
*                                                                     *         
GETQX    J     XIT                                                              
         LTORG                                                                  
***********************************************************************         
*        CHECK FOR DAY/TIME OVERLAPS                                  *         
***********************************************************************         
*                                                                     *         
CKOVLAP  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,PODDAYTM                                                      
*                                                                               
CKOV02   LA    R4,PODDAYTM                                                      
*                                                                               
CKOV04   CR    R3,R4               ARE WE AT SAME LOCATION                      
         BE    CKOV08              YES BYPASS                                   
         CLI   0(R3),X'FF'         IS DAY=ALL                                   
         BE    CKOV06                                                           
         CLI   0(R4),X'FF'         IS DAY=ALL                                   
         BE    CKOV06                                                           
         ZIC   R0,0(R3)                                                         
         ZIC   R1,0(R4)                                                         
         NR    R0,R1                                                            
         LTR   R0,R0               OVERLAP ON DAYS?                             
         BZ    CKOV08              NO BYPASS                                    
*                                                                               
CKOV06   CLI   1(R3),X'FF'         IS TIME=ALL                                  
         BE    CKOVERR                                                          
         CLI   1(R4),X'FF'         IS TIME=ALL                                  
         BE    CKOVERR                                                          
         CLC   1(2,R3),3(R4)       IS START1 GREATER THEN END2                  
         BNL   CKOV08              YES BYPASS                                   
         CLC   3(2,R3),1(R4)       IS END1 LESS THEN START2                     
         BNH   CKOV08              YES BYPASS                                   
         B     CKOVERR             ELSE ERROR CONDITION                         
*                                                                               
CKOV08   LA    R4,5(R4)                                                         
         CLI   0(R4),X'99'         IS TABLE TWO AT END                          
         BNE   CKOV04                                                           
         LA    R3,5(R3)                                                         
         CLI   0(R3),X'99'         IS TABLE ONE AT END                          
         BNE   CKOV02                                                           
         SR    R3,R3                                                            
*                                                                               
CKOVERR  LTR   R3,R3                                                            
*                                                                               
CKOVX    J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE NETWORK                                             *         
***********************************************************************         
VALNT    NMOD1 VALNTWKL,**VALNT**,CLEAR=YES                                     
         LR    R8,RC                                                            
         USING VALNTWKD,R8                                                      
*                                                                               
         L     R2,0(R1)                                                         
         L     R9,4(R1)                                                         
         L     RC,8(R1)                                                         
         MVI   FIELDERR,0                                                       
*                                                                               
         MVI   PDMGROPT,C'N'                                                    
         MVI   PDZZZMED,C'N'       FOR ZZZ NETS                                 
         CLI   5(R2),0             ANY NETWORK GIVEN?                           
         BNE   VNET02              YES                                          
         OI    PDFLAG1,PDF1ALST                                                 
         CLC   =C'TP ',PODBFIL     NO NETWORK/STATION VALID FOR THESE           
         BE    VNETX               FILES IF MARKETS REQUESTED (VALOPT)          
         CLC   =C'IUN',PODBFIL                                                  
         BE    VNETX                                                            
         CLC   =C'TPT',PODBFIL                                                  
         BE    VNETX                                                            
         CLC   =C'PAV',PODBFIL                                                  
         BE    VNETX                                                            
         CLC   =C'WTP',PODBFIL                                                  
         BE    VNETX                                                            
*                                                                               
VNET02   GOTO1 ANY                 GENERATE ERROR IF NO DATA                    
         USING T325FFD,R1                                                       
         GOTO1 SCANNER,PARAS,(R2),(14,BLOCK),C',=,('                            
         CLI   4(R1),0                                                          
         BE    BADNET                                                           
         MVI   BYTE,0                                                           
         MVC   PODNTNUM,4(R1)                                                   
         ZIC   R0,4(R1)            LOOP COUNTER                                 
         MVI   PIVZZZ,0            CLEAR FLAG FOR PIV                           
*                                                                               
         LA    R3,PDSIDMKT                                                      
         L     R4,APODNET                                                       
         LA    R5,BLOCK                                                         
         MVI   PDZZZTYP,0                                                       
         MVI   REPMENU,C'N'                                                     
*                                                                               
         L     RE,APODBKL                                                       
         MVI   NHFLAG,0            CHECK IF NHT SOURCES REQUESTED               
         MVI   ACMFLAG,0           OR ACM                                       
VNET02A  CLI   0(RE),X'FF'                                                      
         BE    VNET03                                                           
         CLC   =C'HP',PODBEXT-PODBD(RE)                                         
         BE    *+14                                                             
         CLC   =C'NH',PODBEXT-PODBD(RE)                                         
         BNE   *+8                                                              
         MVI   NHFLAG,1                                                         
         CLC   =C'ACM',PODBEXT-PODBD(RE)                                        
         BE    *+14                                                             
         CLC   =C'OOHC',PODBEXT-PODBD(RE)                                       
         BNE   *+8                                                              
         MVI   ACMFLAG,1                                                        
         CLI   PODBRNG-PODBD(RE),0                                              
         BE    *+8                                                              
         LA    RE,PODBLNQ(RE)                                                   
         LA    RE,PODBLNQ(RE)                                                   
         B     VNET02A                                                          
*                                                                               
VNET03   ZIC   RE,FIELDERR         UPDATE CURSOR POSITION                       
         LA    RE,1(RE)                                                         
         STC   RE,FIELDERR                                                      
*                                                                               
         CLI   NHFLAG,1            SYNDICATION NOT ALLOWED FOR                  
         BNE   VNET03A             NHT SOURCES                                  
         CLI   16(R5),C'S'                                                      
         BE    BADNET5                                                          
         CLI   16(R5),C' '                                                      
         BNE   VNET03A                                                          
         CLI   15(R5),C'S'                                                      
         BE    BADNET5                                                          
*                                                                               
VNET03A  TM    2(R5),X'80'         SIDMKT?                                      
         BNO   VNET04                                                           
         CLI   PDOVSYS,3           FOR NET, THIS IS A NUMERIC STATION           
         BE    VNET10                                                           
         CLI   PODNTNUM,7          MAXIMUM NUMBER ALLOWED IS 7                  
         BH    BADNET2                                                          
         MVC   0(2,R3),6(R5)       SAVE SIDMKT                                  
         MVC   0(5,R4),12(R5)                                                   
         B     VNET24                                                           
*                                                                               
VNET04   CLI   PDOVSYS,2           FOR SPOT ONLY                                
         BNE   VNET10                                                           
         TM    13(R5),C'0'         MARKET GROUP?                                
         BNO   VNET10                                                           
         CLI   12(R5),C'G'         MARKET GROUP DEF    G => ID <= Z             
         BL    VNET10                                                           
         CLI   12(R5),C'Z'                                                      
         BH    VNET10                                                           
         LA    RE,13(R5)                                                        
         ZIC   R1,0(R5)            LENGTH OF STATION                            
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    VNET08                                                           
*                                                                               
VNET06   TM    0(RE),C'0'          HAS TO BE NUMERIC                            
         BNO   VNET10              NOT, REGULAR STATION                         
         LA    RE,1(RE)                                                         
         BCT   R1,VNET06                                                        
*                                                                               
VNET08   MVC   0(5,R4),12(R5)      COPY MARKET GROUP                            
         MVI   PDMGROPT,C'Y'                                                    
         B     VNET24                                                           
*                                                                               
VNET10   CLI   PDOVSYS,3           FOR NET,                                     
         BNE   *+12                                                             
         BRAS  RE,NUMSTA           DISABLE REQUESTS FOR NUMERIC STATNS          
         BE    BADNET                                                           
*                                                                               
         CLC   =C'MENU=',12(R5)     COULD BE A SET RECORD                       
         BE    VNET54                                                           
*                                                                               
         CLC   12(3,R5),=C'ZZZ'    OR ALL STATIONS                              
         BNE   VNET12              OR A REAL STATION                            
         MVI   REQSML,C'L'         RE-DIRECT SOONS FOR BIG REQUESTS             
*                                                                               
         CLI   16(R5),C'C'                                                      
         BNE   *+8                 IE, ZZZ-C (ALL STA'S FOR CABLE)              
         OI    CABWFLAG,ZZZC                                                    
*                                                                               
         CLI   16(R5),C'H'         ZZZ-H NOT VALID FOR SOURCE ACM               
         BNE   *+12                                                             
         CLI   ACMFLAG,1                                                        
         BE    NOACMHSP                                                         
*                                                                               
         CLI   16(R5),C'S'                                                      
         BE    VNET10A                                                          
         CLC   PODBEXT(3),=C'TCAR' ONLY HAVE FOR SYNDICATORS                    
         BE    VNET10E                                                          
         CLC   PODBEXT(3),=C'WB1'                                               
         BE    VNET10E                                                          
         CLC   PODBEXT(5),=C'ACMWB'                                             
         BE    VNET10E                                                          
*                                                                               
         TM    WHEN,X'20'          FOR SOON REQUESTS                            
         BNO   VNET10A                                                          
         BRAS  RE,CNAD3MO                                                       
         BH    SOONZZZ             ERROR IF CNAD REQUESTS > 3 MONTHS            
*                                                                               
VNET10A  MVI   BYTE,X'FF'                                                       
         MVC   0(3,R4),12(R5)                                                   
         OI    3(R4),X'40'                                                      
         MVC   4(1,R4),16(R5)                                                   
         MVC   PDZZZMED,PODBEXT+4                                               
         CLI   PDZZZTYP,0          DON'T ALLOW MORE THAN ONE ZZZ                
         BE    *+12                                                             
         MVI   FIELDERR,0          DON'T PLACE CURSOR ON THIS NETW              
         B     BADNET                                                           
         MVC   PDZZZTYP,4(R4)                                                   
         CLI   16(R5),C'S'         SYNDICATION                                  
         BNE   *+12                                                             
         MVI   PDZZZTYP,C'M'                                                    
         B     VNET14                                                           
         CLI   16(R5),C'H'         HISPANIC                                     
         BE    VNET14                                                           
         CLI   16(R5),C'C'         CABLE                                        
         BNE   *+12                                                             
         MVI   PDUSECAB,C'Y'                                                    
         B     VNET14                                                           
         MVI   4(R4),C'T'                                                       
         CLI   16(R5),C'N'         NETWORK                                      
         BE    VNET14                                                           
VNET10E  L     R1,ATWA                                                          
         MVC   CONHEAD+29(5),12(R5)                                             
         B     BADNET                                                           
*                                                                               
VNET12   XC    WORK,WORK                                                        
         MVC   WORK+45(1),0(R5)                                                 
         MVC   WORK+48(10),12(R5)                                               
         L     RE,APODBKL                                                       
         CLC   =C'CCV',1(RE)       COUNTY COVERAGE IS NOT CABLE                 
         BE    VNET12D                                                          
         CLC   =C'FUS',1(RE)       FUSION DOESNT HAVE 5TH CHAR C                
         BE    VNET12D                                                          
*                                                                               
         BRAS  RE,CABSTA           CK IF CABLE STATION                          
         BNE   VNET12D                                                          
*                                                                               
VNET12C  MVI   16(R5),C'C'         MAKE 5TH CHARACTER A 'C' FOR CABLE           
         ZIC   RE,WORK+45          UPDATE LENGTH TO ACCOUNT FOR 'C'             
         LA    RE,1(RE)                                                         
         STC   RE,WORK+45                                                       
*                                                                               
         CLI   15(R5),C' '                                                      
         BNE   VNET12D                                                          
         MVI   15(R5),C'-'                                                      
         ZIC   RE,WORK+45                                                       
         LA    RE,1(RE)                                                         
         STC   RE,WORK+45                                                       
*                                                                               
VNET12D  DS    0C                                                               
         MVC   WORK+48(10),12(R5)                                               
*                                                                               
         CLI   WORK+49,C'-'        REPLACE DASH WITH UNDELINE                   
         BNE   *+8                                                              
         MVI   WORK+49,C'_'                                                     
         GOTO1 SCANNER,PARAS,WORK+40,(1,WORK),C',=$-'                           
         CLI   WORK+13,C'_'        REPLACE UNDELINE WITH DASH                   
         BNE   *+8                                                              
         MVI   WORK+13,C'-'                                                     
*                                                                               
         TM    OPTFLAG1,COMSRCOQ   ONLY COM SOURCE REQUESTED?                   
         JO    *+12                                                             
         CLI   WORK+16,C' '        NETWORK > 4 CHARS = COMSCORE                 
         JE    VNET12E                                                          
         MVC   0(10,R4),WORK+12                                                 
         MVI   10(R4),C'X'         DENOTE COMSCORE NETWORK                      
         J     VNET24                                                           
*                                                                               
VNET12E  MVC   0(4,R4),WORK+12                                                  
         MVI   4(R4),C'T'          DEFAULT MEDIA                                
*                                                                               
         CLI   WORK,5              REFORMAT IF NO DASH                          
         BNE   *+8                                                              
         CLI   WORK+1,0                                                         
*        BNE   *+16                                                             
         BNE   VNET13                                                           
         L     RE,APODBKL                                                       
         CLC   =C'CCV',1(RE)                                                    
         BNE   *+12                                                             
         CLI   3(R4),C'/'                                                       
         BE    VNET13              UNLESS FOLLWED BY /SINGLE-DIG-COUNTY         
         MVC   4(1,R4),WORK+16                                                  
         MVC   WORK+22(1),WORK+16                                               
*                                                                               
VNET13   CLI   WORK+1,0                                                         
         BE    *+10                                                             
         MVC   4(1,R4),WORK+22                                                  
         CLI   WORK+22,C'C'        CABLE STATION                                
         BNE   *+8                                                              
         MVI   PDUSECAB,C'Y'                                                    
         MVI   5(R4),0                                                          
*                                                                               
VNET14   CLI   4(R4),C'C'         CABLE STATION(S)                              
         BNE   VNET14A                                                          
         BAS   RE,CKNAD           CK IF ONE OF THE SOURCES IS NAD               
         BE    BADNET4                                                          
*--VALIDATE BOOK TYPE                                                           
VNET14A  L     R1,ATWA                                                          
         MVI   CONHEAD+29,C'('                                                  
         MVC   CONHEAD+30(1),22(R5)                                             
         CLI   4(R4),C'C'          IF CABLE, NO BOOK TYPE UNLESS                
         BNE   VNET16                 TYPE=U                                    
         CLI   22(R5),C'E'                                                      
         BE    *+8                                                              
         CLI   PODBBTY,C'U'                                                     
         BNE   VNET22                                                           
         CLI   PODBMED,C'N'                                                     
         BNE   VNET22                                                           
*                                                                               
VNET16   CLC   PODBEXT(3),=C'NHW'  HISPANIC WEEKLY DOESN'T FORCE                
         BE    VNET20                                                           
         CLC   PODBEXT(3),=C'HPW'  HISPANIC WEEKLY FROM NATIONAL SAMPLE         
         BE    VNET20                                                           
         CLC   PODBEXT(3),=C'NHC'  HISPANIC CABLE DOESN'T FORCE                 
         BE    VNET20                                                           
         CLC   PODBEXT(3),=C'HPC'  HISPANIC CABLE FROM NATIONAL SAMPLE          
         BE    VNET20                                                           
         CLC   PODBEXT(3),=C'TCA'  TCAR WEEKLY DOESN'T FORCE                    
         BE    VNET20                                                           
         CLC   PODBEXT(3),=C'WB1'  WB1 WEEKLY DOESN'T FORCE                     
         BE    VNET20                                                           
         CLC   PODBEXT(5),=C'ACMWB' WB1 ACM    DOESN'T FORCE                    
         BE    VNET20                                                           
         CLC   PODBEXT(3),=C'CCV'  NO BOOKTYPE SET HERE FOR CCV                 
         BE    VNET22                                                           
         CLC   =C'NAD',PODBFIL     NAD FILES DON'T FORCE                        
         BE    VNET20                                                           
         CLC   =C'NAW',PODBFIL     NAD WEEKLY FILES DON'T FORCE                 
         BE    VNET20                                                           
         CLC   =C'IUN',PODBFIL     IUN FILES DON'T FORCE                        
         BE    VNET20                                                           
         CLI   PODBMED,C'T'        SPOT FILES HAVE DIFF. BOOK TYPES             
         BE    VNET20                                                           
         CLI   PODBMED,C'D'                                                     
         BE    VNET20                                                           
         CLI   PODBMED,C'C'        CANADA TOO                                   
         BE    VNET20                                                           
         CLI   PODBMED,C'W'        DON'T FORGET WEEKLY                          
         BE    VNET20                                                           
         CLI   PODBMED,C'O'        OVERNIGHT SPOT TP                            
         BE    VNET20                                                           
         MVI   5(R4),C'A'                                                       
         CLI   1(R5),0                                                          
         BE    VNET22                                                           
         CLI   22(R5),C'A'                                                      
         BE    VNET18                                                           
         CLI   22(R5),C'S'                                                      
         BE    VNET18                                                           
         CLI   22(R5),C'O'                                                      
         BE    VNET18                                                           
         CLI   22(R5),C'D'                                                      
         BE    VNET18                                                           
         CLI   22(R5),C'E'         WEIGHTED WITH VCR                            
         BE    VNET18                                                           
         CLI   22(R5),C'F'         WEIGHTED WO/VCR                              
         BE    VNET18                                                           
         CLI   22(R5),C'I'                                                      
         BNE   BADNET                                                           
*                                                                               
VNET18   MVC   5(1,R4),22(R5)      BOOKTYPE                                     
         B     VNET22                                                           
*                                                                               
VNET20   CLI   1(R5),0                                                          
         BE    VNET22                                                           
         LA    RF,22(R5)                                                        
         BRAS  RE,VSPTBTYP         VALIDATE SPOT BOOKTYPES                      
         BNE   BADNET                                                           
         STC   RF,5(R4)            SAVE INTERNAL BOOKTYPE                       
*                                                                               
VNET22   CLI   PODBMED,C'N'        NO SPILL FOR NETWORK                         
         BE    VNET24                                                           
         XC    WORK,WORK           SPILL MARKETS                                
         MVC   WORK+45(1),0(R5)                                                 
         MVC   WORK+48(10),12(R5)                                               
         GOTO1 SCANNER,PARAS,WORK+40,(1,WORK),C',=$/'                           
         CLI   WORK+1,0            NO SPILL MARKET?                             
         BZ    VNET24                                                           
         TM    WORK+3,X'80'        HAS TO BE A NUMBER                           
         BO    VNET23                                                           
*                                                                               
         L     RE,APODBKL                                                       
         CLC   =C'CCV',1(RE)                                                    
         BE    BADNET              CCV DOESN'T TAKE ALPHA COUNTIES              
         LA    R3,DBLOCKA                                                       
         USING DBLOCKD,R3                                                       
         MVC   DBSELALF,WORK+22    ALPHA SPILL MARKET                           
         MVC   DBINTMED,PODBMED                                                 
         CLI   DBINTMED,C'O'       FOR OVERNIGHTS,                              
         BNE   *+8                                                              
         MVI   DBINTMED,C'T'       USE USTV MARKET NAMES                        
         MVC   DBACTSRC,PODBSRC                                                 
         CLI   DBACTSRC,C'F'                                                    
         BNE   *+8                                                              
         MVI   DBACTSRC,C'N'                                                    
         GOTO1 DEFINE,DMCB,=C'ANMKT',DBLOCK,DUB                                 
         MVC   DBACTSRC,PODBSRC    RESTORE ACT SOURCE FOR FUSION                
         MVC   DBINTMED,PODBMED    RESTORE INTMED FOR OVERNIGHTS                
         LH    RF,DUB              BINARY MARKET                                
         LTR   RF,RF                                                            
         BZ    BADNET                                                           
         B     VNET23A                                                          
         DROP  R3                                                               
*                                                                               
VNET23   SR    RF,RF               NUMERIC SPILL MARKET                         
         ICM   RF,15,WORK+8        (OR COUNTY FOR CCV)                          
         L     RE,APODBKL                                                       
         CLC   =C'CCV',1(RE)                                                    
         BNE   *+12                                                             
         CLI   PODBBTY,0           COUNTY GIVEN WITHOUT A STATE                 
         BE    BADNET3                                                          
VNET23A  STCM  RF,3,6(R4)          SAVE SPILL MARKET                            
         CLI   3(R4),C'/'          TAKE CARE OF 3CHAR STA                       
         BNE   *+8                                                              
         MVI   3(R4),C' '                                                       
*                                                                               
VNET24   LA    R5,32(R5)                                                        
         LA    R4,PODNETL(R4)                                                   
         LA    R3,2(R3)                                                         
         BCT   R0,VNET03                                                        
*                                                                               
         MVI   FIELDERR,0          RESET POINTER TO NETWORK                     
         OC    PDSIDMKT,PDSIDMKT                                                
         BNZ   VNET26                                                           
         CLI   PDMGROPT,C'Y'                                                    
         BNE   VNET28                                                           
         CLI   PODNTNUM,1                                                       
         BH    BADNET                                                           
*                                                                               
VNET26   MVI   0(R4),X'FF'                                                      
         L     RE,APODBKL                                                       
         CLC   =C'CCV',1(RE)        FOR COUNTY COVERAGE                         
         BNE   VNET26A                                                          
         OC    PDSIDMKT,PDSIDMKT                                                
         BZ    VNET26A                                                          
         CLI   PODBBTY,0                                                        
         BE    BADNET3              COUNTY WITHOUT A STATE IS ERROR             
VNET26A  B     VNETX                                                            
*                                                                               
VNET28   MVI   0(R4),X'FF'                                                      
*                                                                               
*--CHECK STATIONS AGAINST NTI CODES                                             
         LA    R3,DBLOCKA                                                       
         USING DBLOCKD,R3                                                       
         L     R4,APODNET                                                       
*                                                                               
VNET30   ZIC   RE,FIELDERR         UPDATE CURSOR POSITION                       
         LA    RE,1(RE)                                                         
         STC   RE,FIELDERR                                                      
         L     R5,APODBKL                                                       
         CLC   1(3,R5),=C'CNA'     CHECK CABLE NAD                              
         BNE   VNET32                                                           
         MVI   5(R4),C'N'          BOOK TYPE IS FORCED TO 'N' FOR CNAD          
         CLC   1(4,R5),=C'CNAW'                                                 
         BNE   *+8                                                              
         MVI   5(R4),X'00'         VALIDATE FOR REG CABLE NETW                  
*                                                                               
VNET32   CLC   0(3,R4),=CL3'ZZZ'    DONT VALIDATE "ALL" STATION                 
         BE    VNET50                                                           
         XC    DBLOCK,DBLOCK                                                    
         OC    6(2,R4),6(R4)       ANY SPILL MARKET?                            
         BZ    *+10                                                             
         MVC   DBSELRMK,6(R4)                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBAREC,AIO                                                       
         MVC   DBSELAGY,AGENCY                                                  
*                                                                               
VNET34   CLC   8(3,R5),=CL3'EVN'   DONT VALIDATE PIV TYPES                      
         BE    VNET50                                                           
         CLC   8(3,R5),=CL3'MPA'                                                
         BE    VNET50                                                           
         CLC   =C'IAG',PODBEXT-PODBD(R5)   NOR 'IAG' BOOKS                      
         BE    VNET50                                                           
         MVI   DBFUNCT,DBVLNBK                                                  
         L     RF,=A(DBFUNTAB)                                                  
         A     RF,PGNR                                                          
*                                                                               
VNET36   CLC   1(5,R5),0(RF)                                                    
         BE    VNET38                                                           
         LA    RF,5(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         BNE   VNET36                                                           
         MVI   DBFUNCT,DBVLSTBK                                                 
*                                                                               
VNET38   MVC   DBSELSTA,0(R4)                                                   
         CLC   =C'REN',1(R5)         RENTRAK -QTR DATA                          
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'Q'                                                  
         CLC   DBSELSTA+3(2),=C'ED'                                             
         BE    VNET50                                                           
         CLC   DBSELSTA+3(2),=C'PD'                                             
         BE    VNET50                                                           
         CLC   DBSELSTA+3(2),=C'LD'                                             
         BE    VNET50                                                           
         CLC   DBSELSTA,=C'NCA S'    DON'T VALIDATE NCA. NO TP DATA             
         BE    VNET50                                                           
         L     RF,=A(DBNADTAB)                                                  
         A     RF,PGNR                                                          
*                                                                               
VNET40   CLC   1(5,R5),0(RF)                                                    
         BE    VNET42                                                           
         LA    RF,7(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         BNE   VNET40                                                           
         B     VNET46                                                           
*                                                                               
VNET42   CLC   8(3,R5),=C'NAD'       NAD SYNDICATION                            
         BNE   *+8                                                              
         CLI   DBSELSTA+4,C'S'                                                  
         BNE   *+12                                                             
         MVI   DBSELSTA+4,C'M'                                                  
         B     VNET46                                                           
         CLI   DBSELSTA+4,C'S'     NTI SYNDICATION                              
         BE    VNET46                                                           
         CLC   PODBEXT-PODBD(3,R5),=C'CNA'  CABLE NAD AND MVGO                  
         BNE   *+12                                                             
         MVI   DBSELSTA+4,C'C'                                                  
         B     VNET46                                                           
         CLI   DBSELSTA+4,C'H'     HISPANIC                                     
         BNE   VNET44                                                           
         CLC   PODBEXT-PODBD(3,R5),=C'NHW'                                      
         BE    *+10                                                             
         CLC   PODBEXT-PODBD(3,R5),=C'HPW'                                      
         BE    *+10                                                             
         CLC   PODBEXT-PODBD(,R5),=C'NHT  '                                     
         BE    *+10                                                             
         CLC   PODBEXT-PODBD(,R5),=C'HPM  '                                     
         BE    *+10                                                             
         CLC   PODBEXT-PODBD(,R5),=C'NAD  '                                     
         BE    *+10                                                             
         CLC   PODBEXT-PODBD(,R5),=C'NAW  '                                     
         BE    *+10                                                             
         MVC   DBSELSTA+3(1),5(RF)     FOR NAD-T,NAD-D, NAW-T & NAW-D           
         MVI   DBSELSTA+4,C'H'                                                  
         B     VNET46                                                           
*                                                                               
VNET44   CLC   1(3,R5),=C'NHC'                                                  
         BE    VNET46                                                           
         CLC   1(3,R5),=C'HPC'                                                  
         BE    VNET46                                                           
         MVC   DBSELSTA+3(2),5(RF)                                              
*                                                                               
VNET46   CLI   DBSELSTA+4,C'H'     HISPANIC NOT VALID FOR SOURCE ACM            
         BNE   *+12                                                             
         CLI   ACMFLAG,1                                                        
         BE    NOACMHSP                                                         
*                                                                               
         CLC   PODBEXT-PODBD(3,R5),=C'BBM'                                      
         BNE   VNET47                                                           
         MVI   DBFUNCT,DBGETTLB                                                 
         MVC   DBBTYPE,PODBBTY-PODBD(R5)                                        
         CLI   DBBTYPE,C'W'        WEEKLY BOOK TYPE?                            
         BNE   VNET47              NO                                           
         MVI   DBBEST,C'M'         YES, ASSUME M/Y REQUEST                      
         CLI   SVBBMWK,C'Y'        WAS IS M/D/Y?                                
         BNE   *+8                 NO                                           
         MVI   DBBEST,C'N'         YES, DBBEST                                  
*                                                                               
VNET47   MVC   DBFILE,8(R5)                                                     
         MVC   DBSELMED,7(R5)                                                   
         MVC   DBSELSRC,6(R5)                                                   
         MVC   DBSELBK,12(R5)                                                   
         TM    DBSELBK+1,X'80'                                                  
         BNO   *+8                                                              
         NI    DBSELBK+1,X'7F'     EXTRACT ESTIMATE BIT                         
*                                                                               
         LA    RE,SETNFF         BUILD 'NETW' DBEXTEND                          
         USING DBXNTID,RE                                                       
         XC    SETNFF,SETNFF                                                    
         MVC   DBXNID,=C'NETW'                                                  
         MVI   DBXNNR2,C'Y'                                                     
         MVI   DBXNCR2,C'Y'                                                     
         MVI   DBXNHOPT,C'Y'                                                    
         MVI   DBXNFFBK,C'Y'                                                    
         DROP  RE                                                               
         ST    RE,DBEXTEND                                                      
*                                                                               
         CLC   =C'CNA',1(R5)     CABLE NAD AND MVGO (TP AND PROG)               
         JNE   VNCNAX                                                           
         MVC   DBFILE,=C'NTI'                                                   
         MVI   DBBTYPE,C'N'                                                     
*                                                                               
         CLC   =C'CNAD',1(R5)    CABLE NAD COMES IN AS MONTHLY REQ              
         BNE   *+12                                                             
         BRAS  RE,MONTOWK        GET FIRST WEEK OF THE MONTH                    
         BNZ   BADNTBK           CHECK RETURN CODE                              
*                                                                               
         CLC   =C'CNAW',1(R5)    VALIDATE FOR REG CABLE STATION(BTYP=0)         
         BNE   *+8                                                              
         MVI   DBBTYPE,0                                                        
*                                                                               
         B     TCWBVN                                                           
*                                                                               
VNCNAX   DS    0C                                                               
*                                                                               
         CLC   =C'NHC',1(R5)                                                    
         BE    *+10                                                             
         CLC   =C'HPC',1(R5)                                                    
         BE    *+10                                                             
         CLC   =C'NHW-H',1(R5)                                                  
         BE    *+10                                                             
         CLC   =C'HPW-H',1(R5)                                                  
         BE    *+10                                                             
         CLC   =C'NHT-H',1(R5)                                                  
         BE    *+10                                                             
         CLC   =C'HPM-H',1(R5)                                                  
         BE    *+10                                                             
         CLC   =C'NHI-T',1(R5)                                                  
         BE    *+10                                                             
         CLC   =C'OOH  ',1(R5)                                                  
         BNE   *+10                                                             
         MVC   DBFILE,=C'NTI'                                                   
                                                                                
TCWBVN   DS    0C                                                               
         CLC   PODBEXT-PODBD(3,R5),=C'TCA' TCAR WEEKLY NEEDS BOOK TYPE          
         BNE   *+12                                                             
         MVI   DBBTYPE,C'T'                                                     
         B     TCWBVN02                                                         
                                                                                
         CLC   PODBEXT-PODBD(5,R5),=C'ACMWB' WB1 ACM    NEEDS BOOK TYPE         
         BNE   *+12                                                             
         MVI   DBBTYPE,C'V'                                                     
         B     TCWBVN02                                                         
*                                                                               
         CLC   PODBEXT-PODBD(3,R5),=C'WB1'  WB1 WEEKLY NEEDS BOOK TYPE          
         BNE   *+12                                                             
         MVI   DBBTYPE,C'V'                                                     
         B     TCWBVN02                                                         
                                                                                
         CLC   PODBEXT-PODBD(3,R5),=C'NCO'  NCO WEEKLY NEEDS BOOK TYPE          
         BNE   TCWBVNOK                                                         
         MVI   DBBTYPE,C'C'                                                     
                                                                                
TCWBVN02 CLI   DBSELSTA+4,C'N'     ONLY VALID FOR NET                           
         BE    TCWBVNOK                                                         
         CLI   DBSELSTA+4,C'S'     OR SYNDICATION                               
         BE    TCWBVNOK                                                         
         CLI   DBSELSTA+4,C' '                                                  
         BE    TCWBVNOK                                                         
         B     BADNTBK                                                          
*                                                                               
TCWBVNOK DS    0C                                                               
         MVC   SAVSTA,DBSELSTA                                                  
         CLC   =C'OPI',PODBEXT-PODBD(R5)                                        
         BNE   OPIOK                                                            
         CLC   DBSELSTA,=C'WBN T'                                               
         BNE   OPIOK                                                            
         MVC   DBSELSTA,=C'WB  T'    VALIDATE FOR WB                            
OPIOK    DS    0H                                                               
*                                                                               
         CLC   DBFILE,=C'IUN'                                                   
         BE    VNET50                                                           
*                                                                               
         CLC   =C'ACM',PODBEXT-PODBD(R5)   COMMERCIAL AVERAGE                   
         BE    *+14                                                             
         CLC   =C'OOHC',PODBEXT-PODBD(R5)  COMMERCIAL AVERAGE                   
         BNE   ACMOK                                                            
         LA    RF,CAVGEXT         CREATE EXTENT 'CAVG'                          
         USING DBXCAVD,RF                                                       
         XC    CAVGEXT,CAVGEXT                                                  
         MVC   DBXCAVID,=C'CAVG'                                                
         MVI   DBXCAV,DBXCVYQ                                                   
         ICM   RE,15,DBEXTEND     ADD COMMERCIAL AVERAGE EXTENSION              
         STCM  RF,15,DBEXTEND                                                   
         STCM  RE,15,DBXCAVNX     LINK THEM                                     
         DROP  RF                                                               
ACMOK    DS    0H                                                               
*                                                                               
         GOTO1 DEMAND,DMCB,DBLOCK,0                                             
         MVC   DBFILE,8(R5)                                                     
         XC    DBEXTEND,DBEXTEND                                                
         CLC   =C'FUS',PODBEXT-PODBD(R5)                                        
         BE    *+10                                                             
         CLC   =C'OPI',PODBEXT-PODBD(R5)                                        
         BNE   *+10                                                             
         MVC   DBSELSTA,SAVSTA                                                  
         CLI   DBERROR,X'80'                                                    
         BE    VNET50                                                           
         CLI   DBERROR,0                                                        
         BE    VNET50                                                           
*                                                                               
*     TRY FOR SPILL AND/OR BOOK TYPE IF REQUESTED                               
         MVC   DBSELMK,6(R4)       SET KEY MARKET                               
         MVC   DBBTYPE,5(R4)       SET BOOK TYPE                                
         CLI   DBBTYPE,0                                                        
         BNE   VNET48                                                           
         CLI   PODBBTY-PODBD(R5),0                                              
         BE    VNET48A                                                          
         MVC   DBBTYPE,PODBBTY-PODBD(R5)                                        
*                                                                               
VNET48A  CLC   =C'CCV',1(R5)                                                    
         BNE   VNET48                                                           
         CLI   PODBBTY-PODBD(R5),0                                              
         BE    *+10                                                             
         MVC   DBBTYPE,PODBBTY-PODBD(R5) BOOK TYPE SET FROM BOOK FIELD          
         CLI   DBBTYPE,0                                                        
         BNE   VNET48                                                           
         L     R1,ATWA                                                          
         MVC   CONHEAD,VNSPACES    CLEAR ERROR AREA                             
         B     VNETX               NO STATE ENTERED. IT'S OK                    
*                                                                               
VNET48   GOTO1 DEMAND,DMCB,DBLOCK,0                                             
         MVC   DBFILE,8(R5)                                                     
         CLI   DBERROR,X'80'                                                    
         BE    VNET50                                                           
         CLI   DBERROR,0                                                        
         BNE   VNET52                                                           
*                                                                               
VNET50   DS    0H                                                               
         LA    R4,PODNETL(R4)                                                   
         CLI   0(R4),X'FF'                                                      
         BE    VNET58                                                           
         B     VNET30                                                           
*                                                                               
VNET52   DS    0H                                                               
         LA    R5,PODBLNQ(R5)                                                   
         CLI   0(R5),X'FF'                                                      
         BE    BADNTBK                                                          
         B     VNET34                                                           
*                                                                               
         USING RSETD,RE                                                         
VNET54   CLI   PDOVSYS,8                                                        
         BNE   BADNET                                                           
         LA    RE,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   RSETKTYP,RSETKTYQ                                                
         MVC   RSETKREP,AGENCY                                                  
         MVC   RSETKSET,=C'MS'     I'M CONFUSED HERE                            
         MVC   RSETKID(4),17(R5)   GET PAST "MENU="                             
         OC    RSETKID(4),VNSPACES                                              
         DROP  RE                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI  '),=C'REPDIR  ',KEY,KEY,0             
         CLC   KEY(RSETKID+4-RSETKTYP),KEYSAVE                                  
         BNE   BADNET                                                           
         GOTO1 DATAMGR,DMCB,(0,=CL8'GETREC'),=CL8'REPFIL',KEY+28,AIO1, X        
               DMWORK,0                                                         
         USING RSETD,RE                                                         
         L     RE,AIO1             SET TO IO AREA                               
         LA    RE,RSETELEM         DETERMINE NUMBER OF ENTRIES                  
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         USING RSETMEMD,RE                                                      
         ST    R0,FULL             SAVE COUNTER IN R0                           
         SR    R0,R0                                                            
         ZIC   R1,RSETMELN                                                      
         ZIC   RF,RSETMLEN                                                      
         DR    R0,RF                                                            
         L     R0,FULL             RESTORE R0                                   
         LA    RE,RSETMEMB                                                      
*                                                                               
VNET56   MVC   0(5,R4),0(RE)       SEED ENTRIES IN STATION LIST                 
         AR    RE,RF                                                            
         LA    R4,PODNETL(R4)                                                   
         BCT   R1,VNET56                                                        
*                                                                               
         SHI   R4,PODNETL                                                       
         MVI   REPMENU,C'Y'                                                     
         B     VNET24                                                           
*                                                                               
*--READ STATION RECORD TO VALIDATE NETWORK                                      
VNET58   MVI   FIELDERR,0                                                       
         L     R5,APODBKL                                                       
         L     R4,APODNET                                                       
*                                                                               
VNET60   CLI   0(R5),X'FF'                                                      
         BE    VNET66                                                           
         CLC   8(3,R5),=CL3'EVN'                                                
         BE    VNET62                                                           
         LA    R5,PODBLNQ(R5)                                                   
         B     VNET60                                                           
*                                                                               
VNET62   ZIC   RE,FIELDERR          UPDATE CURSOR POSITION                      
         LA    RE,1(RE)                                                         
         STC   RE,FIELDERR                                                      
         CLC   0(3,R4),=CL3'ZZZ'    DONT VALIDATE "ALL" STATION                 
         BE    VNET66                                                           
         MVC   KEY(17),=C'SNABC NAA00000000'                                    
         USING STAREC,R3                                                        
         LA    R3,KEY                                                           
         MVC   STAKCALL(4),0(R4)                                                
         MVC   STAKAGY,AGENCY                                                   
         MVI   USEIO,C'Y'                                                       
         MVC   FILENAME,=C'STATION'                                             
         GOTO1 HIGH                                                             
         XC    6(2,R4),6(R4)                                                    
         CLC   KEY(15),KEYSAVE                                                  
         BNE   VNET64                                                           
         L     R3,AIO                                                           
         PACK  DUB,SMKT            CONVERT 'MARKET' NUMBER                      
         CVB   R1,DUB                                                           
         STH   R1,DUB                                                           
         MVC   6(2,R4),DUB         MARKET                                       
*                                                                               
VNET64   LA    R4,PODNETL(R4)                                                   
         CLI   0(R4),X'FF'                                                      
         BNE   VNET62                                                           
         XC    FILENAME,FILENAME                                                
         DROP  R3                                                               
*                                                                               
VNET66   ZIC   R3,PODNTNUM                                                      
         L     R4,APODNET                                                       
         MVI   FIELDERR,0                                                       
*                                                                               
VNET68   DS    0H                                                               
         ZIC   RE,FIELDERR          UPDATE CURSOR POSITION                      
         LA    RE,1(RE)                                                         
         STC   RE,FIELDERR                                                      
         BAS   RE,NEWSTCHK                                                      
         BNZ   BADNTBK                                                          
         LA    R4,PODNETL(R4)                                                   
         CLI   0(R4),X'FF'                                                      
         BNE   VNET68                                                           
         L     R1,ATWA                                                          
         MVC   CONHEAD,VNSPACES      CLEAR ERROR AREA                           
*                                                                               
VNETX    XMOD1                                                                  
         EJECT                                                                  
*                                                                               
BADNET   MVC   PERROR,=AL2(INVNET)                                              
         CLI   PDOVSYS,3           NETWORK OR STATION                           
         BE    *+10                                                             
         MVC   PERROR,=AL2(INVSTA)                                              
         B     NETERR                                                           
*                                                                               
BADNET2  MVC   PERROR,=AL2(TOOMKT) TOO MANY MARKETS                             
         B     NETERR                                                           
*                                                                               
BADNET3  MVC   PERROR,=AL2(MISSTAT) COUNTY GIVEN WITHOUT A STATE                
         B     NETERR                                                           
*                                                                               
BADNET4  MVC   PERROR,=AL2(NADCABS) COUNTY GIVEN WITHOUT A STATE                
         B     NETERR                                                           
*                                                                               
BADNET5  MVC   PERROR,=AL2(SYNNOTNH) SYND NOT AVAIL FOR NHT                     
         B     NETERR                                                           
*                                                                               
NOACMHSP MVC   PERROR,=AL2(NOACMH)  NO HISPANIC DATA AVAILABLE FOR ACM          
         B     NETERR                                                           
*                                                                               
BADNTBK  DS    0H                                                               
         TM    OPTFLAG1,COMSRCOQ   ONLY COM SOURCE REQUESTED?                   
         JO    VNETX                                                            
*                                                                               
         L     R1,ATWA                                                          
         USING T325FFD,R1                                                       
         LA    R2,SPLNETH                                                       
         MVC   CONHEAD(L'INVNTBK),INVNTBK   INVALID NETWORK/BOOK                
         CLI   PDOVSYS,3           NETWORK OR STATION                           
         BE    *+10                                                             
         MVC   CONHEAD+21(7),=C'STATION'                                        
         MVC   CONHEAD+41(5),0(R4)                                              
         MVI   CONHEAD+46,C'-'                                                  
         MVC   CONHEAD+47(1),5(R4)                                              
         MVI   ERROR,X'FE'                                                      
         CLI   REPMENU,C'Y'                                                     
         BNE   *+8                                                              
         MVI   FIELDERR,0                                                       
         GOTO1 CURSERR                                                          
         DROP  R1                                                               
*                                                                               
SOONZZZ  L     R1,ATWA                                                          
         USING T325FFD,R1                                                       
         LA    R2,SPLNETH                                                       
         MVC   CONHEAD(L'SOONERR),SOONERR   ZZZ MUST REQUEST OVERNIGHT          
         MVI   ERROR,X'FE'                                                      
         GOTO1 CURSERR                                                          
         DROP  R1                                                               
*                                                                               
NETERR   L     R1,ATWA                                                          
         USING T325FFD,R1                                                       
         LA    R2,SPLNETH                                                       
         CLI   REPMENU,C'Y'                                                     
         BNE   *+8                                                              
         MVI   FIELDERR,0                                                       
         B     PODERR1                                                          
         DROP  R1                                                               
***********************************************************************         
*        ADDITIONAL VALNET ROUTINES                                   *         
***********************************************************************         
*                                                                               
*CKNAD - ROUTINE TO CHECK IF NAD IS ONE OF THE REQUESTED SOURCES                
*                                                                               
CKNAD    DS    0H                                                               
         L     RF,APODBKL                                                       
CKNAD05  CLI   0(RF),X'FF'                                                      
         BE    NADN                                                             
         CLC   =C'NAD',1(RF)       SOURCE IS NAD                                
         BE    NADY                                                             
         CLC   =C'NAW',1(RF)       SOURCE IS NAD WEEKLY                         
         BE    NADY                                                             
*                                                                               
         CLI   0(RF),0                                                          
         BE    *+8                                                              
         LA    RF,PODBLNQ(RF)      BUMP BOOK TABLE                              
         LA    RF,PODBLNQ(RF)      BUMP BOOK TABLE                              
         B     CKNAD05                                                          
NADY     CR    RB,RB                                                            
         B     CKNADX                                                           
NADN     CHI   RB,0                                                             
CKNADX   BR    RE                                                               
         SPACE 3                                                                
*                                                                     *         
*        CHECK SOURCE BOOKS                                           *         
*                                                                     *         
NEWSTCHK NTR1                                                                   
         MVC   DUB(3),=CL3'PIV'                                                 
         GOTO1 CHKSRCE,DMCB,(1,DUB)                                             
         BZ    NEWS02                                                           
         CLC   0(3,R4),=CL3'ZZZ'                                                
***      BE    NEWSERR             REMOVED 7/26/04                              
         BNE   *+8                                                              
         MVI   PIVZZZ,1                                                         
*                                                                               
NEWS02   MVC   DUB(3),=CL3'NAD'                                                 
         GOTO1 CHKSRCE,DMCB,(3,DUB)                                             
         BZ    NEWS04              SOURCE NOT FOUND                             
         CLI   5(R4),C'S'          TEST SEASON TO DATE                          
         BE    NEWSERR                                                          
*                                                                               
NEWS04   MVC   DUB(3),=CL3'NTI'                                                 
         GOTO1 CHKSRCE,DMCB,(3,DUB)                                             
         BZ    NEWSX               SOURCE NOT FOUND                             
         CLI   5(R4),C'S'          TEST SEASON TO DATE                          
         BE    NEWSERR                                                          
*                                                                               
NEWSX    SR    RE,RE                                                            
*                                                                               
NEWSERR  LTR   RE,RE                                                            
         B     XIT                                                              
*                                                                     *         
*        CHECK BOOK TABLE FOR SOURCE                                  *         
*                                                                     *         
CHKSRCE  NTR1                                                                   
         ZIC   R3,0(R1)                                                         
         BCTR  R3,0                                                             
         L     R4,0(R1)                                                         
         L     R5,APODBKL                                                       
*                                                                     *         
CHKS02   EX    R3,*+12                                                          
         BE    CHKSX                                                            
         BNE   CHKS04                                                           
         CLC   0(0,R4),1(R5)                                                    
*                                                                     *         
CHKS04   LA    R5,PODBLNQ(R5)                                                   
         CLI   0(R5),X'FF'                                                      
         BNE   CHKS02                                                           
         SR    R4,R4               SOURCE NOT FOUND                             
*                                                                               
CHKSX    LTR   R4,R4                                                            
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
VNSPACES DC    CL132' '            SPACES FIELD                                 
         EJECT                                                                  
                                                                                
*---------------------------------------------------------------------*         
*        CHECK IF CABLE STATION                                       *         
*---------------------------------------------------------------------*         
*                                                                     *         
CABSTA   NTR1  BASE=*,LABEL=*                                                   
         TM    OPTFLAG1,COMSRCOQ   ONLY COM SOURCE REQUESTED?                   
         JO    CABSNO                                                           
*                                                                               
*  CABLE MUST HAVE A C AS 5TH CHARACTER. GO THROUGH TABLE OF CABLE              
*  STATIONS AND SEE IF WE HAVE A MATCH.                                         
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,NECABCLL  GET A(NET CABLE STATIONS TABLE)              
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
*                                                                               
         USING NECBCLD,RE                                                       
         CLI   16(R5),C' '         DID USER GIVE US THE NUMERIC CODE?           
         BH    CABSNO              YES.                                         
CABST10  CLC   NECBALPH,12(R5)     NO. NEED TO CHECK IT                         
         BE    CABSYES                                                          
         AR    RE,RF               ADD LENGTH OF TABLE ENTRY                    
         CLI   0(RE),X'FF'                                                      
         BNE   CABST10             NEXT ENTRY IN THE TABLE                      
         DROP  RE                                                               
*                                  NOW CHECK HISPANIC CABLE STATIONS            
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,NEHCBCLL  GET A(HISP CABLE STATIONS TABLE)             
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
*                                                                               
         USING NEHCBCLD,RE                                                      
CABST20  CLC   NEHCBALP,12(R5)                                                  
         BE    CABSYES                                                          
         AR    RE,RF               ADD LENGTH OF TABLE ENTRY                    
         CLI   0(RE),X'FF'                                                      
         BNE   CABST20             NEXT ENTRY IN THE TABLE                      
         B     CABSNO              NOT A CABLE STATION                          
         DROP  RE                                                               
*                                                                               
CABSYES  CR    RE,RE                                                            
         B     CABSTAX                                                          
CABSNO   CHI   RB,0                                                             
CABSTAX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        GET FIRST WEEK OF THE MONTH FOR CABLE NAD                    *         
*---------------------------------------------------------------------*         
MONTOWK  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,DBLOCKA                                                       
         USING DBLOCKD,R3                                                       
*                                                                               
         L     RF,ACOMFACS         GET A(MONTH/DATES TABLE)                     
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,NENACWKS                                               
         ICM   RE,15,0(R1)         A(MONTH/DATES TABLE)                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING NEWKSD,RE                                                        
MONTOWK1 CLC   DBSELBK(1),NEWKSYR   COMPARE ON YEAR                             
         BNE   MONTOWK8                                                         
         CLC   DBSELBK+1(1),NEWKSMO COMPARE ON MONTH                            
         BNE   MONTOWK8                                                         
         MVC   DBSELBK+1(1),NEWKSFRS FIRST WEEK OF THE MONTH                    
         SR    R1,R1                                                            
         B     MONTOWKX                                                         
*                                                                               
MONTOWK8 LA    RE,NEWKSQ(RE)        NEXT ENTRY                                  
         CLI   0(RE),X'FF'                                                      
         BNE   MONTOWK1                                                         
         LA    R1,1                                                             
*                                                                               
MONTOWKX LTR   R1,R1                                                            
         XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
* ROUTINE TO GENERATE A TEXT ROW OR COLUMN                                      
*---------------------------------------------------------------------*         
GENTEXT  NTR1  BASE=*,LABEL=*                                                   
         SR    R1,R1                                                            
         ICM   R1,1,1(R4)                                                       
         BNZ   *+8                                                              
         LA    R1,1                                                             
         CH    R1,=Y(L'DRLITO)                                                  
         BL    *+8                                                              
         LH    R1,=Y(L'DRLITO)                                                  
         STC   R1,DRLENO                                                        
         STC   R1,DRLITLNO                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DRLITO(0),22(R4)                                                 
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
* VALIDATE SPOT 1 OR 2-CHARACTER BOOKTYPES                            *         
* ON INPUT, RF POINTS TO BOOKTYPE EXPRESSION                          *         
* RETURN CC (NE: INVALID BOOKTYPE)                                    *         
* IF VALID, ON OUTPUT RF HOLDS INTERNAL BOOKTYPE                      *         
*---------------------------------------------------------------------*         
VSPTBTYP NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'NSI',PODBEXT     2-CHAR BOOKTYPES ONLY FOR                    
         BE    VSPTB10             THESE SOURCES                                
         CLC   =C'TP',PODBEXT                                                   
         BE    VSPTB10                                                          
         CLC   =C'OTP',PODBEXT                                                  
         BE    VSPTB10                                                          
         CLC   =C'PAV',PODBEXT                                                  
         BE    VSPTB10                                                          
         CLC   =C'OPA',PODBEXT                                                  
         BE    VSPTB10                                                          
         CLI   0(RF),C'A'          1-CHAR BOOKTYPES FOR OTHER SOURCES           
         BL    VSPTBBAD            HAS TO BE IN RANGE A-Z                       
         CLI   0(RF),C'Z'                                                       
         BH    VSPTBBAD                                                         
         CLI   0(RF),C')'          ONLY 1 CHAR                                  
         BNE   VSPTBBAD                                                         
         ZIC   RF,0(RF)            SAVE BOOKTYPE                                
         B     VSPTBOK                                                          
                                                                                
VSPTB10  MVC   HALF,=C'  '                                                      
         MVC   HALF(1),0(RF)       SAVE 1ST CHARACTER OF BOOKTYPE               
         CLI   1(RF),C')'          1-CHAR BOOKTYPE?                             
         BE    VSPTB20             YES                                          
         CLI   2(RF),C')'          2-CHAR BOOKTYPE?                             
         BNE   VSPTBBAD                                                         
         MVC   HALF+1(1),1(RF)     YES.SAVE 2ND CHARACTER                       
                                                                                
VSPTB20  L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOKTYPE TABLE)                        
         ICM   RE,15,0(R1)         A(TABLE)RETURNED IN P1                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RE                                                      
VSPTB30  CLI   0(RE),X'FF'                                                      
         BE    VSPTBBAD            BOOKTYPE NOT FOUND. INVALID.                 
         CLC   HALF,SPBKTYPA                                                    
         BE    *+10                                                             
         AR    RE,R0                                                            
         B     VSPTB30                                                          
         ZIC   RF,SPBKTYPN         SAVE INTERNAL BOOKTYPE                       
         B     VSPTBOK                                                          
         DROP  RE                                                               
                                                                                
VSPTBOK  CR    RB,RB                                                            
         B     VSPTBX                                                           
VSPTBBAD CHI   RB,0                                                             
         B     VSPTBX                                                           
                                                                                
VSPTBX   XIT1  REGS=(RF)                                                        
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
* CHECK CABLE NAD REQUEST GREATER THAN 3 MONTHS                       *         
* RETURN CC (HIGHER,LESS,EQUAL)                                       *         
*---------------------------------------------------------------------*         
CNAD3MO  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         GOTO1 =A(OVFLRTN),DMCB,(13,DUB),(RC),=CL5'CNAD',RR=PGNR                
         L     RF,0(R1)                                                         
         CHI   RF,3                CK CNAD >3 MONTHS                            
         BH    CNAD3MOX                                                         
                                                                                
         GOTO1 =A(OVFLRTN),DMCB,(13,DUB),(RC),=CL5'CNADT',RR=PGNR               
         L     RF,0(R1)                                                         
         CHI   RF,3                CK CNADT >3 MONTHS                           
         BH    CNAD3MOX                                                         
                                                                                
         GOTO1 =A(OVFLRTN),DMCB,(13,DUB),(RC),=CL5'CNAW',RR=PGNR                
         L     RF,0(R1)                                                         
         CHI   RF,12               CK CNAW >12 WEEKS                            
         BH    CNAD3MOX                                                         
                                                                                
         GOTO1 =A(OVFLRTN),DMCB,(13,DUB),(RC),=CL5'CNAWT',RR=PGNR               
         L     RF,0(R1)                                                         
         CHI   RF,12               CK CNAWT >12 WEEKS                           
         BH    CNAD3MOX                                                         
                                                                                
CNAD3MOX J     XIT                 EXIT WITH CONDITION CODE SET                 
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
* MAKE SURE ALL NAD CATEGORIES ARE LESS THAN 191                                
* WILL NOT BE ABLE TO STACK THEM OTHERWISE                                      
*---------------------------------------------------------------------*         
CKNADOK  NTR1  BASE=*,LABEL=*                                                   
         LA    RE,BLOCK                                                         
         LA    RF,PDNSCRD          MAX NO OF DEMOS ON ONE SCREE                 
         MVI   FIELDERR,1                                                       
*                                                                               
CKNAD6   CLI   0(RE),0                                                          
         BE    CKNADOKY                                                         
         CLI   1(RE),0             IS DEMO IN NAD FORMAT                        
         BE    CKNAD8                                                           
         CLI   7(RE),191           IS NAD CATEGORY < 191                        
         BNL   CKNADOKN                                                         
*                                                                               
CKNAD8   LA    RE,32(RE)                                                        
         AI    FIELDERR,1                                                       
         BCT   RF,CKNAD6                                                        
*                                                                               
CKNADOKY CR    RB,RB                                                            
         J     XIT                                                              
CKNADOKN CHI   RB,0                                                             
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
* VALIDATE PROGRAM VIEWING SOURCE FILTER/OPTION                                 
* AT ENTRY: R3-> SCANNER FIELD WITH PVS EXPRESSION                              
*           BYTE=1 IF IT'S THE FIRST EXPRESSION                                 
*           VALID PVS EXPRESSION:1ST CHAR=SOURCE,2ND CHAR=VEWING TYPE           
*           EITHER OF THEM CAN BE WILD CARD '*'                                 
*           CHAR '-' IN FRONT OF FIRST EXPRESSION MEANS EXCLUDE                 
* AT EXIT:  DMCB(2) HAS VALID EXPRESSION                                        
*           CC SET TO EQUAL IF VALID                                            
*---------------------------------------------------------------------*         
VALPVS   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R2,12(R3)           START OF PVS EXPRESSION                      
                                                                                
         CLI   BYTE,1              FIRST EXPRESSION                             
         BNE   VALPVS10                                                         
         MVI   PVSNEG,0            YES                                          
         CLI   0(R2),C'-'          NEGATE FILTER?                               
         BNE   VALPVS10                                                         
         MVI   PVSNEG,PVSNEGQ      YES. SET FLAG                                
         LA    R2,1(R2)            SKIP NEGATE SIGN                             
                                                                                
VALPVS10 CLI   0(R2),C'*'          VALIDATE SOURCE                              
         BE    VALPVS20            WILD CARD                                    
         CLI   0(R2),NPG2SPGM      SOURCE IS EITHER PROGRAM AVERAGE             
         BE    VALPVS20             OR                                          
         CLI   0(R2),NPG2SCOM      COMMERCIAL AVERAGE                           
         BE    VALPVS20             OR                                          
         CLI   0(R2),NPG2SAPG      PROG AVG FROM ACM TAPES                      
         BE    VALPVS20             OR                                          
         CLI   0(R2),NPG2STP       TIME PERIOD                                  
         BNE   VALPVSNO            INVALID SOURCE                               
                                                                                
VALPVS20 CLI   1(R2),C'*'          VALIDATE VIEWING TYPE                        
         BE    VALPVS30            WILD CARD                                    
         L     RE,=A(PVSTAB)                                                    
         A     RE,PGNR                                                          
VALPVS25 CLI   0(RE),X'FF'                                                      
         BE    VALPVSNO            INVALID VIEWING TYPE                         
         CLC   0(1,RE),1(R2)                                                    
         BE    VALPVS30                                                         
         LA    RE,1(RE)                                                         
         B     VALPVS25                                                         
                                                                                
VALPVS30 MVC   DMCB(2),0(R2)       SAVE SOURCE+VIEWING TYPE                     
         B     VALPVSYS                                                         
                                                                                
VALPVSNO CHI   RB,0                                                             
         J     XIT                                                              
VALPVSYS CR    RB,RB                                                            
         J     XIT                                                              
         DROP  R8                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
* CHECK TO SEE IF STATION IS NUMERIC                                            
* AT ENTRY: R5-> SCANNER FIELD CONTAINING STATION                               
* AT EXIT:  CC SET TO EQUAL IF THIS IS A NUMERIC STATION                        
*---------------------------------------------------------------------*         
NUMSTA   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    2(R5),X'80'         ENTIRE FIELD IS NUMERIC                      
         BO    NUMSTAY                                                          
*                                                                               
         ZIC   R1,0(R5)            NUMERIC FOLLOWED BY '-' AND MEDIA            
         LA    RE,12(R5)                                                        
*                                                                               
NUMS10   TM    0(RE),X'F0'                                                      
         BO    NUMS20                                                           
         CLI   0(RE),C'-'          REACHED '-'                                  
         BE    NUMSTAY             THIS WAS A VALID NUMERIC STATION             
         B     NUMSTAN             FOUND SOME OTHER CHARACTER. NOT NUM          
NUMS20   LA    RE,1(RE)                                                         
         BCT   R1,NUMS10                                                        
         B     NUMSTAY                                                          
*                                                                               
NUMSTAY  CR    RB,RB                                                            
         B     NUMSTAX                                                          
NUMSTAN  CHI   RB,0                                                             
NUMSTAX  J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE BOOK                                                *         
***********************************************************************         
*                                                                               
VALBK    NMOD1 BKWORKL,**VALBK**,CLEAR=YES                                      
*                                                                               
         LR    R8,RC                                                            
         USING BKWORKD,R8                                                       
*                                                                               
         L     R2,0(R1)                                                         
         L     R9,4(R1)                                                         
         L     RC,8(R1)                                                         
*                                                                               
         CLI   5(R2),0             NO SOURCE, BOOK?                             
         BNE   VALB02                                                           
         MVI   PERROR+1,MISSING    * SPECIAL CASE *                             
         MVI   PMSGTYPE,C'E'       LET ERROR ROUTINE HANDLE IT                  
         GOTO1 VPDSERR                                                          
*                                                                               
VALB02   L     RE,APODBKL                                                       
*        LA    RF,451                                                           
         LA    RF,L'PODBKL                                                      
         XCEFL                                                                  
*                                                                               
         L     RE,APDREQDT         CLEAR REQUESTED DATE TABLE (EXACT)           
         LA    RF,L'PDREQDT                                                     
         XCEFL                                                                  
*                                                                               
         XC    PDAESTBK,PDAESTBK        RESET EST BOOK PTR                      
         MVI   PODFLAG,0                                                        
*                                                                               
         BRAS  RE,XSRC             IDENTIFY AND REPLACE CROSS-SOURCE            
*                                                                               
         GOTO1 PODSCAN,DMCB,(20,(R2)),(12,BLOCK),C',=,-'                        
         CLI   DMCB+4,2            MUST BE 2 PARTS FOR RANGE                    
         BL    BADBOOK                                                          
*                                                                               
         BAS   RE,SHRTRNG                                                       
*                                                                               
         ZIC   R2,DMCB+4                                                        
         LA    R3,BLOCK            SET UP DBLOCK                                
         L     R4,APODBKL                                                       
         OI    OPTFLAG1,COMSRCOQ   DEFAULT TO ONLY COM SOURCE REQUESTED         
*                                                                               
VALB04   L     R5,=A(BOOKTAB)                                                   
         A     R5,PGNR                                                          
                                                                                
         CLC   =C'INV',12(R3)                                                   
         BNE   *+10                                                             
         MVC   12(3,R3),=C'IUN'                                                 
                                                                                
         CLC   =C'CNAD',12(R3)                                                  
         JNE   *+8                                                              
         OI    PODFLAG,PODCNADQ                                                 
         CLC   =C'CNAW',12(R3)                                                  
         JNE   *+8                                                              
         OI    PODFLAG,PODCNAWQ                                                 
                                                                                
         CLI   1(R3),0             NO SECOND FIELD                              
         BE    VALB06                                                           
         MVI   15(R3),C'-'                                                      
         MVC   16(1,R3),24(R3)                                                  
         LA    RE,5                ALWAYS 5 AFTER THIS                          
         STCM  RE,1,0(R3)                                                       
*                                                                               
VALB06   CLC   0(5,R5),12(R3)                                                   
         BE    VALB08                                                           
         LA    R5,12(R5)                                                        
         CLI   0(R5),X'FF'                                                      
         BNE   VALB06                                                           
         B     BADSRCE                                                          
*                                                                               
VALB08   XC    TEMPFLD,TEMPFLD                                                  
         CLC   0(5,R5),=C'COM  '                                                
         BNE   VALB09                                                           
         OI    OPTFLAG1,COMSRCQ    SOURCE COM REQUESTED                         
         BRAS  RE,GETCSDAT         GET START/END DATES                          
         B     *+8                                                              
VALB09   NI    OPTFLAG1,X'FF'-COMSRCOQ   TURN OFF COM SOURCE ONLY FLAG          
*                                                                               
         CLC   0(5,R5),=C'PIV  '                                                
         BNE   *+8                                                              
         MVI   REQSML,C'L'         LONG JOB                                     
*                                                                               
         CLC   =C'REN-T',0(R5)     RENTRAK ALSO WEEKLY CABLE                    
         BE    *+10                                                             
         CLC   =C'NTI-T',0(R5)                                                  
         BNE   *+8                                                              
         OI    CABWFLAG,NTITP                                                   
*                                                                               
         CLC   =C'NTI  ',0(R5)                                                  
         BE    *+14                                                             
         CLC   =C'OOH  ',0(R5)                                                  
         BNE   *+8                                                              
         OI    CABWFLAG,NTI                                                     
*                                                                               
         CLC   =C'IAG',0(R5)       IAG IS NAD-LIKE DATA                         
         BE    *+14                                                             
         CLC   7(3,R5),=CL3'NAD'   NAD,NHI,CNAD,CNAW                            
         BNE   VALB10                                                           
         MVI   PODNADBK,X'FF'                                                   
         CLC   0(3,R5),=C'CNA'     CABLE NAD AND MVGO FORCES PDBASE             
         BNE   VALB14                                                           
         MVI   PDDUROPT,C'M'                                                    
         MVI   PDBASE,C'I'                                                      
         B     VALB14                                                           
*                                                                               
VALB10   CLC   0(3,R5),=CL3'MPA'                                                
         BE    VALB12                                                           
         CLC   7(2,R5),=CL3'TP '   THIS COVERS TP, T4 ,DPT, SRC, BBM            
         BE    VALB12                          CSI                              
         CLC   7(2,R5),=CL3'CTP'   THIS COVERS COUNTY COVERAGE                  
         BE    VALB12                          CSI                              
         CLC   7(3,R5),=CL3'PAV'                                                
         BE    VALB12                          CSI                              
*        BNE   VALB14                                                           
         CLC   7(3,R5),=CL3'IUN'                                                
         BNE   VALB14                                                           
VALB12   MVI   PODNADBK,X'F0'      SPOT BOOK WAS REQUESTED                      
*                                                                               
VALB14   CLC   0(3,R5),=CL3'PIV'                                                
         BNE   *+14                                                             
         OC    PODPSTRT(4),PODPSTRT   SEE IF DATES REQUESTED                    
         BZ    BADPIVDT                                                         
*                                                                               
         L     RE,APDREQDT                                                      
         STCM  RE,15,REQDPTR       SAVE A(REQUESTED DATE TABLE)                 
*                                                                               
         ZIC   RF,11(R5)                                                        
         SLL   RF,2                CK FOR NO INPUT                              
         B     VALB16(RF)                                                       
*                                                                               
VALB16   B     VALB100             ROUTINE DOES BOOKVAL                         
         B     VALB300             ROUTINE FOR EMI/NMI                          
         EJECT                                                                  
VALB100  DS    0H                                                               
         BAS   RE,BOOKBUMP                                                      
         BZ    BADBOOK                                                          
*                                                                               
VALB102  BAS   RE,BOOKNUM                                                       
         BNZ   BADBOOK                                                          
*                                                                               
         BAS   RE,SAVEREQ          SAVE REQUESTED DATE                          
         BNE   BADBOOK                                                          
*                                                                               
         L     RE,APODBKL                                                       
         MVC   PODBD(PODBLNQ),0(RE)                                             
         GOTO1 =A(OVFLRTN),DMCB,(6,DUB),(RC),RR=PGNR VSFTBOOK                   
         BE    VALB112                                                          
*                                                                               
         GOTO1 =A(OVFLRTN),DMCB,(12,DUB),(RC),RR=PGNR VLASTWKS                  
         BE    VALB112                                                          
*                                                                               
VALB104  XC    DMCB(12),DMCB                                                    
         ZIC   RF,0(R3)                                                         
         ST    RF,DMCB+4                                                        
         GOTO1 ONEBOOK,DMCB,12(R3),,FULL,(R5)                                   
         BNE   BADBOOK                                                          
         MVC   1(11,R4),0(R5)      INFO FROM BOOKTAB                            
         MVC   12(2,R4),FULL+1     MOVE BOOK TO TABLE                           
         MVC   14(1,R4),DMCB+20    MOVE IN BOOK TYPE                            
*        MVI   13(R4),36                                                        
*        MVI   14(R4),C'N'                                                      
         MVC   18(3,R4),TEMPFLD    START DATE (COMSCORE)                        
         MVC   21(3,R4),TEMPFLD+3  END DATE (COMSCORE)                          
         CLI   DMCB+8,C'L'         WAS THIS SET FOR LABEL (SQAD)?               
         BNE   VALB105             NO                                           
*                                                                               
         MVI   14(R4),0            CLEAR BOOK TYPE                              
         CLC   DMCB+20(5),=CL5' '  DID WE GET A QUARTER?                        
         BNH   BADBOOK             NO                                           
         LA    RF,DMCB+21          YES, POINT TO QUARTER PARAMETER              
         TM    0(RF),X'F0'                                                      
         BNO   BADBOOK             HAS TO BE NUMERIC                            
         PACK  DUB,0(1,RF)                                                      
         CVB   R1,DUB                                                           
         CHI   R1,1                MUST BE BETWEEN 1 AND 4                      
         BL    BADBOOK                                                          
         CHI   R1,4                                                             
         BH    BADBOOK                                                          
         STC   R1,17(R4)           STORE QUARTER AS BINARY                      
         LA    RF,1(0,RF)          BUMP TO YEAR OR /                            
*                                                                               
         CLI   0(RF),C'/'          SKIP OVER / IF THERE                         
         BNE   *+8                                                              
         LA    RF,1(0,RF)                                                       
         PACK  DUB,0(2,RF)                                                      
         CVB   R1,DUB                                                           
         CH    R1,=H'27'                                                        
         BH    *+8                                                              
         LA    R1,100(R1)                                                       
         STC   R1,16(R4)           STORE YEAR AS BINARY                         
*                                                                               
VALB105  CLC   =C'IUN',0(R5)       DECODE REP INV KEY SOURCE                    
         BNE   VALB108                                                          
         LA    RF,SVCLST                                                        
         MVI   15(R4),C'N'                                                      
*                                                                               
VALB106  CLI   0(RF),X'FF'         DEFAULT - JUST LEAVE AS C'N'                 
         BE    VALB110                                                          
         CLC   FULL(1),3(RF)       FOUND A MATCH (SOURCE/BOOK)                  
         BE    *+12                                                             
         LA    RF,L'SVCLST(RF)                                                  
         B     VALB106                                                          
         CLC   DMCB+20(1),4(RF)       FOUND A MATCH (BOOKTYPE)                  
         BE    *+12                                                             
         LA    RF,L'SVCLST(RF)                                                  
         B     VALB106                                                          
         MVC   15(1,R4),2(RF)      SET INV KEY SOURCE                           
         B     VALB110                                                          
*                                                                               
VALB108  TM    FULL,X'20'          DO WE USE ESTIMATED BOOK?                    
         BZ    *+12                                                             
         OI    13(R4),X'80'        SET HOB ON MONTH/WEEK                        
         STCM  R4,15,PDAESTBK                                                   
*                                                                               
VALB110  CLI   1(R3),0                                                          
         BE    VALB112                                                          
         OI    0(R4),X'01'         SET RANGE INDICATOR                          
         LA    R4,PODBLNQ(R4)                                                   
*                                                                               
         ZIC   RF,1(R3)            SET 2ND FLD LEN                              
         ST    RF,DMCB+4                                                        
         GOTO1 ONEBOOK,DMCB,24(R3),,FULL,(R5)                                   
         BNE   BADBOOK                                                          
         MVC   1(11,R4),0(R5)      INFO FROM BOOKTAB                            
         MVC   12(2,R4),FULL+1     MOVE BOOK TO TABLE                           
         MVC   14(1,R4),DMCB+20                                                 
         MVC   18(3,R4),TEMPFLD    START DATE (COMSCORE)                        
         MVC   21(3,R4),TEMPFLD+3  END DATE (COMSCORE)                          
*                                                                               
         CLC   0(3,R5),=CL3'PIV'   IF PROGRAM INVENTORY                         
         BNE   *+14                                                             
         OC    PODPSTRT,PODPSTRT   THEN PROGRAM DATE RANGE MUST EXIST           
         BZ    BADBOOK                                                          
*                                                                               
         LR    RE,R4                                                            
         LA    RF,PODBLNQ                                                       
         SR    RE,RF               CHECK THE YEARS AT LEAST                     
         CLC   12(1,RE),PODBLNQ+PODBBKS-PODBD(RE)                               
         BH    BADBOOK                                                          
*--SET PODBOOK AREA                                                             
*                                                                               
VALB112  LA    R4,PODBLNQ(R4)                                                   
         CLI   1(R4),0                                                          
         BNE   VALB112                                                          
         ZIC   RF,PODBKNUM                                                      
         LA    RF,1(RF)                                                         
         STC   RF,PODBKNUM                                                      
         BAS   RE,BOOKBUMP                                                      
         BE    VALBX                                                            
         BAS   RE,BOOKNUM                                                       
         BNZ   VALB04                                                           
         B     VALB102                                                          
*                                                                               
VALBX    MVI   0(R4),X'FF'         SET END OF TABLE MARKER                      
         BRAS  RE,CNADNTI                                                       
         BE    BADSRCE2                                                         
         BRAS  RE,CCVMORE                                                       
         BE    BADSRCE3            CCV SOURCE REQUESTED WITH NON-CCV            
         BRAS  RE,RENMORE                                                       
         BE    BADSRCE4            REN SOURCE REQUESTED WITH NON-REN            
*                                                                               
         BAS   RE,NHSRCBK          DATE RESTRICTIONS FOR HISP SOURCES           
         CHI   RF,1                                                             
         BE    BADHOLD             INVALID BOOK FOR OLD SOURCE                  
         CHI   RF,2                                                             
         BE    BADHNEW             INVALID BOOK FOR NEW SOURCE                  
*                                                                               
         L     RE,APODBKL                                                       
         MVC   PODBD(PODBLNQ),0(RE)                                             
         ICM   RE,15,REQDPTR       SET END OF REQUEST DATES TABLE               
         MVI   0(RE),X'FF'                                                      
         GOTO1 =A(OVFLRTN),DMCB,(3,DUB),(RC),RR=PGNR VFLOWTAB                   
*                                                                               
         XMOD1                                                                  
         EJECT                                                                  
*----------------------------------------------------------*                    
* THIS ROUTINE IS FOR VALIDATING EMI/NMI SOURCES AND BOOKS *                    
*----------------------------------------------------------*                    
VALB300  DS    0H                                                               
         LR    R0,R5               SAVE THE BOOK TABLE ADDRESS                  
         CLC   0(3,R5),=C'EMI'     SEE IF DATES REQUESTED                       
         BNE   *+14                                                             
         OC    PODPSTRT(4),PODPSTRT                                             
         BZ    BADPIVDT                                                         
*                                                                               
* FIRST BOOK IN EXPRESSION                                                      
*                                                                               
         BAS   RE,BOOKBUMP                                                      
         BZ    BADBOOK                                                          
VALB302  BAS   RE,BOOKNUM                                                       
         BNZ   BADBOOK                                                          
*                                                                               
         BAS   RE,SAVEREQ          SAVE REQUESTED DATE                          
         BNE   BADBOOK                                                          
*                                                                               
         GOTO1 =A(OVFLRTN),DMCB,(6,DUB),(RC),RR=PGNR VSFTBOOK                   
         BE    VALB304                                                          
*                                                                               
         XC    DMCB(12),DMCB                                                    
         ZIC   RF,0(R3)                                                         
         ST    RF,DMCB+4                                                        
         GOTO1 ONEBOOK,DMCB,12(R3),,FULL,(R5)                                   
         BNE   BADBOOK                                                          
         MVC   1(11,R4),0(R5)      INFO FROM BOOKTAB                            
         MVC   12(2,R4),FULL+1     MOVE BOOK TO TABLE                           
         MVC   14(1,R4),DMCB+20                                                 
         MVC   18(3,R4),TEMPFLD    START DATE (COMSCORE)                        
         MVC   21(3,R4),TEMPFLD+3  END DATE (COMSCORE)                          
*                                                                               
         CLI   1(R3),0                                                          
         BE    VALB304                                                          
         OI    0(R4),X'01'         SET RANGE INDICATOR                          
         LA    R4,PODBLNQ(R4)                                                   
*                                                                               
         ZIC   RF,0(R3)                                                         
         ST    RF,DMCB+4                                                        
         GOTO1 ONEBOOK,DMCB,24(R3),,FULL,(R5)                                   
         BNE   BADBOOK                                                          
         MVC   1(11,R4),0(R5)      INFO FROM BOOKTAB                            
         MVC   12(2,R4),FULL+1     MOVE BOOK TO TABLE                           
         MVC   14(1,R4),DMCB+20                                                 
         MVC   18(3,R4),TEMPFLD    START DATE (COMSCORE)                        
         MVC   21(3,R4),TEMPFLD+3  END DATE (COMSCORE)                          
*                                                                               
         CLC   0(3,R5),=CL3'EMI'   IF PROGRAM INVENTORY                         
         BNE   *+14                                                             
         OC    PODPSTRT,PODPSTRT   THEN PROGRAM DATE RANGE MUST EXIST           
         BZ    BADBOOK                                                          
*                                                                               
         LR    RE,R4                                                            
         LA    RF,PODBLNQ                                                       
         SR    RE,RF                                                            
         CLC   12(1,RE),PODBLNQ+PODBBKS-PODBD(RE) CHECK THE YEARS               
         BH    BADBOOK                                                          
*--SET PODBOOK AREA                                                             
*                                                                               
VALB304  DS    0H                                                               
         LA    R4,PODBLNQ(R4)                                                   
         CLI   1(R4),0                                                          
         BNE   VALB304                                                          
         ZIC   RF,PODBKNUM                                                      
         LA    RF,1(RF)                                                         
         STC   RF,PODBKNUM                                                      
         CR    R0,R5                                                            
         BNE   VALB306             FINISHED THE 2 PARTS, NTI & MPA              
         BAS   RE,BOOKBUMP                                                      
         BZ    BADBOOK2            NEED MPA BOOK                                
*                                                                               
*----------------------------------------------------------------               
* FIRST BOOK (NTI) FINISHED GRAB SECOND BOOK (MPA)                              
*----------------------------------------------------------------               
         BAS   RE,BOOKNUM                                                       
         BNZ   VALB04                                                           
*                                  NEED MPA INFO                                
         L     R5,=A(BOOKMPA)                                                   
         A     R5,PGNR                                                          
         B     VALB302                                                          
*                                                                               
VALB306  BAS   RE,BOOKBUMP                                                      
         BZ    VALBX                                                            
         BAS   RE,BOOKNUM                                                       
         BNZ   VALB04                                                           
         LR    R5,R0               RESTORE ORIGINAL SOURCE                      
         B     VALB302                                                          
         EJECT                                                                  
***********************************************************************         
*        DEAL WITH SHORT RANGE INPUT                                            
*        DD/MM-DD/MM/YY WILL BE CHANGED TO DD/MM/YY-DD/MM/YY                    
*        MM-MM/YY WILL BE CHANGED TO MM/YY-MM/YY                                
***********************************************************************         
SHRTRNG  NTR1                                                                   
         LA    R3,BLOCK            SET UP DBLOCK                                
*                                                                               
         BAS   RE,BOOKBUMP         GO TO START DATE                             
         BZ    SHRTX                                                            
*                                                                               
         CLI   1(R3),0             NO END DATE->NO RANGE                        
         BE    SHRTX                                                            
*                                                                               
         SR    R0,R0                                                            
         LA    RE,12(R3)           START OF FIELD                               
         ZIC   RF,0(R3)            LENGTH OF FIELD                              
SHRT5    CLI   0(RE),C'/'                                                       
         BNE   *+8                                                              
         AHI   R0,1                R0=NO OF SLASHES IN START DATE               
         LA    RE,1(RE)                                                         
         BCT   RF,SHRT5                                                         
*                                                                               
         SR    R1,R1                                                            
         LA    RE,24(R3)           START OF FIELD                               
         ZIC   RF,1(R3)            LENGTH OF FIELD                              
SHRT10   CLI   0(RE),C'/'                                                       
         BNE   *+10                                                             
         AHI   R1,1                R1=NO OF SLASHES IN END DATE                 
         LR    R2,RE               R2 -> LAST SLASH IN END DATE                 
         LA    RE,1(RE)                                                         
         BCT   RF,SHRT10                                                        
*                                                                               
         SR    R1,R0                                                            
         CHI   R1,1                                                             
         BNE   SHRTX                                                            
*                                  START DATE IS MISSING YEAR                   
         LA    RE,12(R3)                                                        
         ZIC   R0,0(R3)                                                         
         AR    RE,R0               RE -> AFTER START DATE                       
*                                                                               
         LA    RF,24(R3)                                                        
         ZIC   R0,1(R3)                                                         
         AR    RF,R0                                                            
         SR    RF,R2                                                            
         BCTR  RF,0                RF = LENGTH OF INPUT YEAR                    
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R2)       TRANSFER /YEAR TO START DATE                 
         AHI   RF,1                                                             
         ZIC   R0,0(R3)                                                         
         AR    R0,RF               UPDATE LENGTH OF START DATE                  
         STC   R0,0(R3)                                                         
*                                                                               
SHRTX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        DETERMINE IF CORRECT BOOKS REQUESTED WITH HISPANIC SOURCES             
*        NHT,NHW,NHC - HAS DATA ONLY BEFORE SEP/07                              
*        HPM,HPW,HPC - HAS DATA ONLY STARTING SEP/07                            
***********************************************************************         
NHSRCBK  NTR1                                                                   
         L     R4,APODBKL                                                       
         USING PODBD,R4                                                         
NHSRB05  CLI   0(R4),X'FF'                                                      
         BE    NHSRCBOK                                                         
*                                                                               
*OLD SOURCES (BASED ON HISPANIC SAMPLE)                                         
         CLC   =C'NHT',PODBEXT                                                  
         BE    NHSRB10             WITH MONTHLY BOOKS                           
         CLC   =C'NHW',PODBEXT                                                  
         BE    NHSRB15             WITH WEEKLY BOOKS                            
         CLC   =C'NHC',PODBEXT                                                  
         BE    NHSRB15                                                          
         B     NHSRB20                                                          
*CHECK MONTHLY BOOKS FOR OLD SOURCES                                            
NHSRB10  CLC   PODBBKS,=AL2(SEP_07) START BOOK < SEP/07?                        
         BNL   NHOLDBAD             NO. INVALID                                 
         CLI   PODBRNG,0                                                        
         BE    NHSRBNXT             SINGLE BOOK IS OK                           
         LA    R4,PODBLNQ(R4)                                                   
         CLC   PODBBKS,=AL2(SEP_07) END BOOK >= SEP/07?                         
         BL    NHSRBNXT                                                         
         MVC   PODBBKS,=AL2(AUG_07) REPLACE END BOOK WITH LAST VALID BK         
         B     NHSRBNXT                                                         
*CHECK WEEKLY BOOKS FOR OLD SOURCES                                             
*                                   START BOOK < AUG27/07?                      
NHSRB15  CLC   PODBBKS,=AL1(YR_2007,WEEK_35)                                    
         BNL   NHOLDBAD             NO. INVALID                                 
         CLI   PODBRNG,0                                                        
         BE    NHSRBNXT             SINGLE BOOK IS OK                           
         LA    R4,PODBLNQ(R4)       RANGE. ADVANCE TO END BOOK                  
*                                   END BOOK >= AUG27/07?                       
         CLC   PODBBKS,=AL1(YR_2007,WEEK_35)                                    
         BL    NHSRBNXT                                                         
*                                   REPLACE END BOOK WITH LAST VALID BK         
         MVC   PODBBKS,=AL1(YR_2007,WEEK_34)                                    
         B     NHSRBNXT                                                         
*                                                                               
*NEW SOURCES (BASED ON NATIONAL SAMPLE)                                         
NHSRB20  CLC   =C'HPM',PODBEXT                                                  
         BE    NHSRB25                                                          
         CLC   =C'HPW',PODBEXT                                                  
         BE    NHSRB30                                                          
         CLC   =C'HPC',PODBEXT                                                  
         BE    NHSRB30                                                          
         B     NHSRBNXT            ALL OTHER SOURCES ARE OK                     
*CHECK MONTHLY BOOKS FOR NEW SOURCES                                            
NHSRB25  CLC   PODBBKS,=AL2(SEP_07)                                             
         BNL   NHSRBNXT             START BOOK >= SEP/07 IS VALID               
         CLI   PODBRNG,0                                                        
         BE    NHNEWBAD             SINGLE BOOK BEFOR SEP/07 IS INVALID         
         LA    R4,PODBLNQ(R4)       RANGE. ADVANCE TO END BOOK                  
         CLC   PODBBKS,=AL2(SEP_07) END BOOK < SEP/07 IS INVALID                
         BL    NHNEWBAD                                                         
         SHI   R4,PODBLNQ           GO BACK TO START BOOK                       
         MVC   PODBBKS,=AL2(SEP_07) REPLACE START BK WITH FRST VALID BK         
         B     NHSRBNXT                                                         
*CHECK WEEKLY BOOKS FOR NEW SOURCES                                             
NHSRB30  CLC   PODBBKS,=AL1(YR_2007,WEEK_35)                                    
         BNL   NHSRBNXT             START BOOK >= AUG27/07 IS VALID             
         CLI   PODBRNG,0                                                        
         BE    NHNEWBAD             SINGLE BK BEFOR AUG27/07 IS INVALID         
         LA    R4,PODBLNQ(R4)       RANGE. ADVANCE TO END BOOK                  
*                                   END BOOK < AUG27/07 IS INVALID              
         CLC   PODBBKS,=AL1(YR_2007,WEEK_35)                                    
         BL    NHNEWBAD                                                         
         SHI   R4,PODBLNQ           GO BACK TO START BOOK                       
*                                   REPLACE START BK WITH FRST VALID BK         
         MVC   PODBBKS,=AL1(YR_2007,WEEK_35)                                    
         B     NHSRBNXT                                                         
*                                                                               
NHSRBNXT CLI   PODBRNG,0                                                        
         BE    *+8                                                              
         LA    R4,PODBLNQ(R4)       START BOOK FOR RANGE                        
         LA    R4,PODBLNQ(R4)       SINGLE BOOK OR END BOOK FOR RANGE           
         B     NHSRB05                                                          
*                                                                               
NHSRCBOK SR    RF,RF                                                            
         B     NHSRCBX                                                          
*                                                                               
NHOLDBAD LHI   RF,1                INVALID BOOK FOR OLD SOURCE                  
         B     NHSRCBX                                                          
NHNEWBAD LHI   RF,2                INVALID BOOK FOR NEW SOURCE                  
         B     NHSRCBX                                                          
*                                                                               
NHSRCBX  CHI   RF,0                                                             
         XIT1  REGS=(RF)                                                        
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        OPTIONALLY SAVE REQUESTED DATES.                             *         
*        VALID ENTRIES ARE MMM/DD/YY AND MMM/YY                       *         
***********************************************************************         
*                                                                               
SAVEREQ  NTR1                                                                   
         CLI   EXACTOPT,C'Y'       ARE WE RUNNING WITH EXACT DATES?             
         BNE   SAVEOK              NO, DON'T NEED THIS THEN                     
         ICM   R5,15,REQDPTR       GET NEXT SPACE IN TABLE                      
*                                                                               
SAVE01   LA    R4,12(R3)           POINT TO FIRST DATE                          
*                                                                               
SAVE02   GOTO1 DATVAL,DMCB,(R4),WORK                                            
         OC    DMCB(4),DMCB        MMDDYY?                                      
         BZ    SAVE20              NO, COULD BE THE SOURCE. SKIP IT             
*                                                                               
SAVE04   GOTO1 DATCON,DMCB,WORK,WORK+6                                          
         ORG   *-2                                                              
         CLC   WORK+4(2),=C'00'                                                 
         BNE   SAVE06                                                           
         MVC   WORK+4(2),=C'01'                                                 
         MVI   0(R1),X'30'                                                      
         LA    RE,WORK                                                          
         ST    RE,8(R1)                                                         
         MVI   8(R1),X'01'                                                      
*                                                                               
SAVE06   BASR  RE,RF                                                            
         OC    0(6,R5),0(R5)       DO WE ALREADY HAVE START DATE?               
         BNZ   SAVE08              YES, THIS IS THE SEND PASS THEN              
         MVC   0(6,R5),WORK        NO, SAVE THE START DATE                      
         CLC   WORK(6),WORK+6      ARE START AND END THE SAME?                  
         BE    *+10                YES                                          
         MVC   6(6,R5),WORK+6      NO, MOVE IN END OF MONTH                     
         LA    R4,24(R3)           BUMP TO NEXT FIELD IN CASE NEEDED            
         CLI   1(R3),0             IS THERE A DATE RANGE?                       
         BNE   SAVE02              YES, PROCESS SECOND HALF                     
         MVC   6(6,R5),WORK+6      NO, START/END THE SAME                       
         B     SAVE10              DONE                                         
*                                                                               
SAVE08   MVC   6(6,R5),WORK        SAVE SECOND DATE                             
*                                                                               
SAVE10   LA    R5,12(R5)           GET NEXT SPOT                                
         STCM  R5,15,REQDPTR       SAVE THE ADDRESS FOR NEXT TIME               
*                                                                               
SAVE20   LA    R3,42(R3)           GET NEXT BOOK/DATE                           
         BCT   R2,SAVE01           IF ANY, GO BACK                              
*                                                                               
SAVEOK   SR    R1,R1               EQUAL IF OK                                  
         B     *+8                                                              
*                                                                               
SAVENG   LA    R1,1                NOT EQUAL IF INVALID                         
         LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DEAL WITH ONE BOOK EXPRESSION                     *         
*              INPUT          (R2) P1=A(INPUT EXPRESSION)             *         
*                             (R3) P2=LENGTH OF EXPRESSION            *         
*                             (R4) P3=A(BOOK OUTPUT)                  *         
*                             (R5) P4=A(BOOKTAB)                      *         
***********************************************************************         
*                                                                               
ONEBOOK  NTR1                                                                   
         LM    R2,R5,0(R1)                                                      
         MVI   DMCB+20,X'00'       RESET POTENTIAL BOOKTYPE                     
         MVI   SVBBMWK,0           CLEAR BBM WEEKLY REQUEST INDICATOR           
         MVI   BYTE,0                                                           
*                                                                               
         GOTO1 DATVAL,PARAS,(R2),WORK                                           
         OC    PARAS(4),PARAS                                                   
         BNZ   ONEB02                                                           
*                                                                               
         GOTO1 DATVAL,PARAS,(2,(R2)),WORK                                       
         OC    PARAS(4),PARAS                                                   
         BZ    ONEB04                                                           
*                                                                               
ONEB02   CLC   WORK(4),=CL4'8709'  TEST BOOK LESS THEN SEP/87                   
         BNL   *+8                                                              
         OI    PODBKOPT,X'80'                                                   
*                                                                               
         CLC   WORK(4),=CL4'8710'  TEST BOOK GREATER THEN OCT/87                
         BL    *+8                                                              
         OI    PODBKOPT,X'40'                                                   
*                                                                               
         CLC   WORK(4),=CL4'8609'  TEST BOOK LESS THEN SEP/86                   
         BNL   ONEB04                                                           
         OI    PODBKOPT,X'20'                                                   
*                                  FIRST SEE IF BOOKVAL CAN HANDLE              
ONEB04   MVC   HALF(2),WORK        SAVE THE YEAR FOR CSI                        
         XC    WORK,WORK                                                        
         STC   R3,WORK+5                                                        
         MVC   WORK+8(12),0(R2)                                                 
         MVC   DMCB+8(4),SCANNER                                                
         MVI   DMCB+8,C'N'         NET  VALIDATES FOR WEEK  BOOK                
         LA    RF,ONEB06                                                        
         BAS   RE,ONEMCHK                                                       
         B     *+8                 NOT EQUAL DON'T SET FOR SPOT                 
*                                  FIRST SEE IF BOOKVAL CAN HANDLE              
ONEB06   MVI   DMCB+8,C'S'                                                      
         CLI   DMCB+8,C'S'         IF SPOT                                      
         BNE   ONEB08                                                           
         LA    RE,DMCB+20          SETUP FOR BOOKTYPE                           
         ST    RE,DMCB+12                                                       
         MVI   DMCB+8,C'B'                                                      
         CLC   0(4,R5),=C'SQAD'    EXCEPT IF SQAD                               
         BNE   ONEB08                                                           
         MVI   DMCB+8,C'L'         THEN LOOK FOR LABEL                          
*                                  FIRST SEE IF BOOKVAL CAN HANDLE              
ONEB08   SR    RF,RF                                                            
         IC    RF,=C'N'            DEFAULT SOURCE                               
         CLC   =C'IUN',0(R5)       NEED SOURCE FOR INVENTORY FILE               
         BNE   *+8                                                              
         CLI   3(R5),C'-'                                                       
         BNE   *+8                                                              
         IC    RF,4(R5)                                                         
*                                                                               
         CLC   =C'CCV',0(R5)     FUDGE BOOKTYPE FOR BOOKVAL                     
         BNE   ONEB09                                                           
         LA    R0,12              MAX LENGTH                                    
         LA    R1,WORK+8                                                        
         CLI   0(R1),C'('                                                       
         BE    ONEB08A                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
         B     ONEB09             NO BOOK TYPE ENTERED                          
*                                                                               
ONEB08A  L     RE,=A(STACODE)                                                   
         A     RE,PGNR                                                          
ONEB08B  CLC   =X'FFFF',0(RE)                                                   
         BE    ONENG              INVALID STATE                                 
         CLC   1(2,R1),1(RE)                                                    
         BE    *+12                                                             
         LA    RE,28(RE)                                                        
         B     ONEB08B                                                          
         MVC   1(1,R1),0(RE)       SET BOOKTYPE                                 
         MVC   2(2,R1),=C') '                                                   
         ZIC   R0,WORK+5           LENGTH OF EXPRESSION IS 1 LESS               
         SHI   R0,1                                                             
         STC   R0,WORK+5                                                        
*                                                                               
ONEB09   DS    0H                                                               
         GOTO1 BOOKVAL,DMCB,((RF),WORK),(1,(R4)),,,(C'C',ACOMFACS)              
         CLI   4(R1),1                                                          
         BE    ONEOK                                                            
*                                  NOW TRY FOR A DATE/NETWEEK APPROACH          
         LA    RF,ONENG                                                         
         BAS   RE,ONEMCHK                                                       
         GOTO1 DATVAL,PARAS,(R2),WORK                                           
         OC    PARAS(4),PARAS                                                   
         BZ    ONENG                                                            
*                                                                               
         CLC   0(3,R5),=C'OPA'     OVERNIGHT PAV USES NSIWEEK                   
         BE    *+14                                                             
         CLC   0(3,R5),=C'OTP'     OVERNIGHT TP USES NSIWEEK                    
         BNE   *+12                                                             
         MVI   BYTE,1              SET NSIWEEK FOR MONDAY START                 
         B     ONEB10                                                           
*                                                                               
         CLC   7(3,R5),=C'WTP'     WTP, CSI NAW AND BBM USE NSIWEEK             
         BE    ONEB10                                                           
         CLC   0(3,R5),=C'CSI'                                                  
         BE    ONEB10                                                           
         CLC   0(3,R5),=C'BBM'                                                  
         BNE   ONEB12                                                           
         MVI   SVBBMWK,C'Y'        BBM MUST BE MM/DD/YY(W) TO GET HERE          
         MVI   BYTE,1              SET NSIWEEK FOR MONDAY START                 
*                                                                               
ONEB10   DS    0H                                                               
         GOTO1 NSIWEEK,PARAS,WORK,(BYTE,GETDAY),ADDAY,DATCON                    
         MVI   0(R4),X'50'                                                      
         MVC   1(1,R4),PARAS+4     YEAR NO.                                     
         MVC   2(1,R4),PARAS       WEEK NO.                                     
         B     ONEOK                                                            
*                                                                               
ONEB12   GOTO1 NETWEEK,PARAS,WORK,GETDAY,ADDAY                                  
         MVI   0(R4),X'50'                                                      
         MVC   1(1,R4),PARAS+4     YEAR NO.                                     
         MVC   2(1,R4),PARAS+12    WEEK NO.                                     
         B     ONEOK                                                            
*                                                                               
ONEOK    ZIC   R0,1(R4)            FINAL CHECK OF YEAR                          
         CHI   R0,84               YEAR < 1984 IS INVALID                       
         BL    ONENG               THIS ALSO TAKES CARE OF YEARS > 2027         
*                                                                               
ONEGD    SR    R1,R1               BOOK WAS VALID                               
         B     *+8                                                              
*                                                                               
ONENG    LA    R1,1                BOOK INVALID                                 
         LTR   R1,R1                                                            
         B     XIT2                                                             
*                                                                               
ONEMCHK  CLC   0(3,R5),=C'NAW'     WEEKLY OK FOR NAW                            
         BER   RE                                                               
         CLC   0(4,R5),=C'CNAW'    WEEKLY OK FOR CABLE MVGO/WKLY NAD            
         BER   RE                                                               
         CLC   7(3,R5),=C'NAD'     MONTHLY BOOK CHECK                           
         BER   RF                                                               
         CLC   0(3,R5),=C'MPA'                                                  
         BER   RF                                                               
         CLC   0(3,R5),=C'BBM'                                                  
         BE    ONEMC04                                                          
         CLC   0(3,R5),=C'NTI'                                                  
         BE    ONEMC04                                                          
         CLC   0(4,R5),=C'OOH '                                                 
         BE    ONEMC04                                                          
         CLC   0(5,R5),=C'ACM  '                                                
         BE    ONEMC04                                                          
         CLC   0(5,R5),=C'OOHC '                                                
         BE    ONEMC04                                                          
         CLC   0(5,R5),=C'MXM  '                                                
         BE    ONEMC04                                                          
         CLC   0(3,R5),=C'NHW'                                                  
         BE    ONEMC04                                                          
         CLC   0(3,R5),=C'HPW'     NHW FROM NATIONAL SAMPLE                     
         BE    ONEMC04                                                          
         CLC   0(5,R5),=C'NSI-W'                                                
         BE    ONEMC04                                                          
         CLI   6(R5),C'O'                                                       
         BE    ONEMC04                                                          
*                                                                               
         CLC   0(3,R5),=C'CSI'     CSI WEEKLY AFTER 96                          
         BNE   ONEMC02                                                          
         CLC   HALF(2),=C'96'                                                   
         BLR   RF                                                               
         BR    RE                                                               
*                                                                               
ONEMC02  CLI   6(R5),C'O'          OVERNIGHTS ARE WEEKLY BOOKS                  
         BER   RE                                                               
         CLC   7(3,R5),=C'TP '                                                  
         BER   RF                                                               
         CLC   7(3,R5),=C'CTP'                                                  
         BER   RF                                                               
         CLC   0(3,R5),=C'PAV'                                                  
         BER   RF                                                               
         CLC   0(3,R5),=C'IUN'                                                  
         BER   RF                                                               
         CLC   0(3,R5),=C'SRC'                                                  
         BER   RF                                                               
         BR    RE                                                               
*                                                                               
ONEMC04  ST    RE,SAVERE           SAVE RETURN ADDRESS                          
         ST    RF,SAVERF                                                        
         LA    RE,ONEB06           SEE IF FIRST TIME IN                         
         CR    RF,RE               IS THIS THE FIRST TIME IN?                   
*        BER   RF                  YES, MONTHLY OK FOR BBM                      
         BNE   ONEMC06                                                          
         CLC   0(3,R5),=C'BBM'                                                  
         BER   RF                  YES, MONTHLY OK FOR BBM                      
         L     RE,SAVERE                                                        
         BR    RE                  WEEKLY OK FOR NTI,NHW,NSI OVERNIGHTS         
*                                                                               
ONEMC06  XC    WORK,WORK           IF WE GET HERE A SECOND TIME,                
         LR    RF,R3                 IT FAILED BOOKVAL AND WE HAVE TO           
         BCTR  RF,0                   FIND BOOKTYPE OURSELVES                   
         STC   R3,WORK+45                                                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK+48(0),0(R2)                                                 
         GOTO1 SCANNER,PARAS,WORK+40,(10,WORK),C',=,('                          
         L     RE,SAVERE                                                        
         L     RF,SAVERF                                                        
         LA    R1,WORK                                                          
         MVC   DMCB+20(1),22(R1)                                                
*                                                                               
         CLC   0(3,R5),=C'NHW'     NTI AND NHW AND ACM                          
         BE    ONEMC08                                                          
         CLC   0(3,R5),=C'HPW'     NHW FROM NATIONAL SAMPLE                     
         BE    ONEMC08                                                          
         CLC   0(3,R5),=C'NTI'                                                  
         BE    ONEMC08                                                          
         CLC   0(4,R5),=C'OOH '                                                 
         BE    ONEMC08                                                          
         CLC   0(3,R5),=C'MXM'                                                  
         BE    ONEMC08                                                          
         CLC   0(5,R5),=C'ACM  '                                                
         BE    ONEMC08                                                          
         CLC   0(5,R5),=C'OOHC '                                                
         BNE   ONEMC10                                                          
ONEMC08  CLI   DMCB+20,C'E'        ONLY WEIGHTED BOOK TYPE                      
         BE    *+8                 ANYTHING ELSE FORCE TO NO BTYP               
         MVI   DMCB+20,X'00'                                                    
         BR    RE                                                               
                                                                                
ONEMC10  CLC   0(5,R5),=C'NSI-W'   NSI-W                                        
         BNE   ONEMC20                                                          
         CLI   DMCB+20,C'P'        ONLY LPM BOOK TYPE                           
         BE    *+8                 ANYTHING ELSE FORCE TO NO BTYP               
         MVI   DMCB+20,X'00'                                                    
         BR    RE                                                               
*                                                                               
ONEMC20  CLI   6(R5),C'O'          NSI OVERNIGHTS                               
         BNE   ONEMC30                                                          
         CLI   DMCB+20,C'L'        LIVE                                         
         BE    ONEMC25                                                          
         CLI   DMCB+20,C'C'        CABLE OVERNIGHTS                             
         BE    ONEMC25                                                          
         CLI   DMCB+20,C'W'        CABLE OVERNIGHTS                             
         BE    ONEMC25                                                          
         CLI   DMCB+20,C'U'        CABLE OVERNIGHTS                             
         BE    ONEMC25                                                          
         CLI   DMCB+20,C'Z'        CABLE OVERNIGHTS                             
         BE    ONEMC25                                                          
         MVI   DMCB+20,X'00'       ANYTHING ELSE FORCE TO NO BTYP               
ONEMC25  BR    RE                                                               
*                                  BBM                                          
ONEMC30  CLI   DMCB+20,C'W'        IS IT WEEKLY BOOKTYPE?                       
         BNER  RF                  NO, ERROR                                    
         BR    RE                  OK, CONTINUE                                 
         EJECT                                                                  
BOOKBUMP LA    R3,42(R3)                                                        
         BCTR  R2,0                                                             
         C     R2,=F'0'                                                         
         BNL   *+6                                                              
         DC    H'0'                                                             
         BR    RE                                                               
*                                                                               
BOOKNUM  NTR1                                                                   
         ZIC   RF,0(R3)            LENGTH OF FIELD                              
         BCTR  RF,0                                                             
         LA    RE,12(R3)           START OF FIELD                               
         CLC   0(2,RE),=C'T4'      IS IT 4 WEEK FILE                            
         BE    BKNTNUM                                                          
         CLC   0(3,RE),=C'WB1'     IS IT WB1                                    
         BE    BKNTNUM                                                          
         AR    RE,RF               POINT TO LAST CHARACTER                      
*--CHECK FOR NUMERIC                                                            
         CLI   0(RE),C')'          POSSIBLE BOOKTYPE                            
         BE    BOOKNUM2                                                         
         CLI   0(RE),X'F0'                                                      
         BL    BKNTNUM                                                          
         CLI   0(RE),X'F9'                                                      
         BH    BKNTNUM                                                          
*                                                                               
BOOKNUM2 SR    RF,RF                                                            
         LTR   RF,RF                                                            
XIT2     XIT1                                                                   
*                                                                               
BKNTNUM  LTR   RE,RE                                                            
         B     XIT2                                                             
*                                                                               
BOOKHD   MVC   WORK+8(0),12(R3)                                                 
VVDXIT   B     XIT2                                                             
         EJECT                                                                  
***********************************************************************         
*        ERROR ROUTINES                                               *         
***********************************************************************         
*                                                                               
BADBOOK  MVC   PERROR,=AL2(INVBOOK)                                             
         B     BOOKERR                                                          
*                                                                               
BADBOOK2 MVC   PERROR,=AL2(MPABOOK)                                             
         B     BOOKERR                                                          
*                                                                               
BADSRCE  MVC   PERROR,=AL2(INVSRC)                                              
         B     BOOKERR                                                          
*                                                                               
BADSRCE2 MVC   PERROR,=AL2(CNDNTI)                                              
         B     BOOKERR                                                          
*                                                                               
BADSRCE3 MVC   PERROR,=AL2(CCVMERR)                                             
         B     BOOKERR                                                          
*                                                                               
BADSRCE4 MVC   PERROR,=AL2(RENMERR)                                             
         B     BOOKERR                                                          
*                                                                               
BADPIVDT MVC   PERROR,=AL2(NOPIVD)                                              
         B     BOOKERR                                                          
*                                                                               
BADHOLD  MVC   PERROR,=AL2(BADHSOLD)                                            
         B     BOOKERR                                                          
*                                                                               
BADHNEW  MVC   PERROR,=AL2(BADHSNEW)                                            
         B     BOOKERR                                                          
*                                                                               
BOOKERR  L     R1,ATWA                                                          
         USING T325FFD,R1                                                       
         LA    R2,SPLSBKH                                                       
         B     PODERR1                                                          
         DROP  R1                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RESVCTABN                                                      
         EJECT                                                                  
***********************************************************************         
*CCVMORE - CHECK IF SOURCE CCV HAS BEEN REQUESTED WITH OTHER SOURCES            
***********************************************************************         
CCVMORE  NTR1  BASE=*,LABEL=*                                                   
         SR    R0,R0               FLAG FOR CCV                                 
         SR    R1,R1               FLAG FOR NON-CCV                             
         L     RF,APODBKL                                                       
CCVMO05  CLI   0(RF),X'FF'                                                      
         BE    CCVMO10                                                          
         CLC   =C'CCV',1(RF)                                                    
         BNE   *+12                                                             
         LHI   R0,1                                                             
         B     *+8                                                              
         LHI   R1,1                                                             
         CLI   0(RF),0                                                          
         BE    *+8                                                              
         LA    RF,PODBLNQ(RF)      BUMP BOOK TABLE IF RANGE                     
         LA    RF,PODBLNQ(RF)      BUMP BOOK TABLE                              
         B     CCVMO05                                                          
*                                                                               
CCVMO10  CHI   R0,1                                                             
         BNE   CCVMONO                                                          
         CHI   R1,1                                                             
         BNE   CCVMONO                                                          
*                                                                               
CCVMOYES CR    RB,RB               BOTH CCV AND NON-CCV                         
         B     CCVMOREX                                                         
CCVMONO  CHI   RB,0                                                             
CCVMOREX J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*CNADNTI - CHECK IF SOURCES CNAD AND NTI WERE REQUESTED TOGETHER                
***********************************************************************         
CNADNTI  NTR1  BASE=*,LABEL=*                                                   
         MVI   DMCB,0                                                           
         L     RF,APODBKL                                                       
CDNTI05  CLI   0(RF),X'FF'                                                      
         BE    CDNTI10                                                          
         CLC   =C'NTI',1(RF)                                                    
         BNE   *+8                                                              
         OI    DMCB,NTISRCQ                                                     
         CLC   =C'CNAD',1(RF)                                                   
         BE    *+10                                                             
         CLC   =C'CNAW',1(RF)                                                   
         BNE   *+8                                                              
         OI    DMCB,CNADSRQ                                                     
         CLI   0(RF),0                                                          
         BE    *+8                                                              
         LA    RF,PODBLNQ(RF)      BUMP BOOK TABLE IF RANGE                     
         LA    RF,PODBLNQ(RF)      BUMP BOOK TABLE                              
         B     CDNTI05                                                          
*                                                                               
CDNTI10  TM    DMCB,NTISRCQ                                                     
         BNO   CDNTINO                                                          
         TM    DMCB,CNADSRQ                                                     
         BNO   CDNTINO                                                          
*                                                                               
CDNTIYES CR    RB,RB                                                            
         B     CNADNTIX                                                         
CDNTINO  CHI   RB,0                                                             
CNADNTIX J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
* REPLACE SOURCE XSRC WITH SOURCES NTI+ACM+MXM                                  
*---------------------------------------------------------------------*         
XSRC     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   DUMYFLD(8),0(R2)                                                 
         MVI   DUMYFLD,L'DUMYFLD   FIELD LENGTH                                 
*                                                                               
         LA    R3,DUMYFLD+8        PUT DUMMY INPUT DATA HERE                    
         LA    R4,8(R2)            INPUT DATA                                   
         SR    R0,R0               HOLD DUMMY INPUT LENGTH                      
*                                                                               
         LA    RE,8(R2)            START OF INPUT FIELD                         
         ZIC   RF,5(R2)                                                         
         AR    RE,RF               +INPUT DATA LENGTH                           
         ST    RE,FULL             A(BYTE AFTER END OF INPUT FIELD)             
*                                                                               
         MVI   BYTE,0              FLAG FOR XSRC FOUND                          
*                                                                               
XSRC10   CLC   =C'XSRC',0(R4)                                                   
         BE    XSRC20                                                           
XSRC15   MVC   0(1,R3),0(R4)                                                    
         LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         AHI   R0,1                                                             
         C     R4,FULL                                                          
         BNL   XSRC100             END OF FIELD                                 
         B     XSRC10                                                           
*                                                                               
XSRC20   LA    R4,4(R4)            GO PAST THE SOURCE                           
         CLI   0(R4),C','          SHOULD BE FOLLOWED BY COMMA                  
         BNE   XSRC15              ELSE NOT THE XSRC SOURCE. CONTINUE           
         MVI   BYTE,1              XSRC FOUND                                   
         LA    R4,1(R4)            FOLLOWED BY BOOK EXPRESSION                  
*                                                                               
         SR    R1,R1               HOLD LENGTH OF BOOK EXPRESSION               
         LR    RE,R4                                                            
XSRC30   C     RE,FULL             SEARCH FOR THE END OF BK EXPRESSION          
         BNL   XSRC40                                                           
         CLI   0(RE),C','                                                       
         BE    XSRC40                                                           
         LA    R1,1(R1)            UPDATE LENGTH OF BOOK EXPRESSION             
         LA    RE,1(RE)                                                         
         B     XSRC30                                                           
*                                                                               
XSRC40   STCM  R1,15,BOOKLN        STORE THE BOOK EXPR LENGTH                   
*                                                                               
         C     RE,FULL                                                          
         BNL   XSRC50                                                           
*                                  ATTEMPT TO FIND MORE BOOKS                   
         LA    RE,1(RE)            GO PAST THE COMMA                            
XSRC42   C     RE,FULL                                                          
         BNL   XSRC44                                                           
         CLI   0(RE),C','                                                       
         BE    XSRC44                                                           
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         B     XSRC42                                                           
XSRC44   SHI   RE,1                BACK OUT 1 POSITION                          
         CLI   0(RE),C')'          CK LAST CHAR TO DETERMINE IF A BOOK          
         BE    XSRC46              ENDS WITH A BOOKTYPE - IT'S A BOOK           
         CLI   0(RE),X'F0'                                                      
         BL    XSRC50              NOT NUMERIC. NOT A BOOK                      
         CLI   0(RE),X'F9'                                                      
         BH    XSRC50              NOT NUMERIC. NOT A BOOK                      
*                                                                               
XSRC46   LA    RE,2(RE)            GO PAST THE END OF THE BOOK                  
         STCM  R1,15,BOOKLN        UPDATE THE BOOK EXPR LENGTH                  
         B     XSRC42              GO SEE IF ANY MORE BKS FOR THIS SRC          
*                                                                               
XSRC50   MVC   0(4,R3),=C'NTI,'                                                 
         LA    R3,4(R3)                                                         
         AHI   R0,4                                                             
         ICM   R1,15,BOOKLN                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R4)       MOVE BOOK EXPRESSION                         
         ICM   R1,15,BOOKLN                                                     
         AR    R3,R1               POINT TO NEXT AVAILABLE SLOT                 
         AR    R0,R1               UPDATE DUMMY INPUT LENGTH                    
*                                                                               
         MVC   0(5,R3),=C',ACM,'                                                
         LA    R3,5(R3)                                                         
         AHI   R0,5                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R4)       MOVE BOOK EXPRESSION                         
         ICM   R1,15,BOOKLN                                                     
         AR    R3,R1               POINT TO NEXT AVAILABLE SLOT                 
         AR    R0,R1               UPDATE DUMMY INPUT LENGTH                    
*                                                                               
         MVC   0(5,R3),=C',MXM,'                                                
         LA    R3,5(R3)                                                         
         AHI   R0,5                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R4)       MOVE BOOK EXPRESSION                         
         ICM   R1,15,BOOKLN                                                     
         AR    R3,R1               POINT TO NEXT AVAILABLE SLOT                 
         AR    R0,R1               UPDATE DUMMY INPUT LENGTH                    
*                                                                               
         AR    R4,R1                                                            
         C     R4,FULL             CHECK END OF INPUT FIELD                     
         BL    XSRC10                                                           
*                                                                               
XSRC100  CLI   BYTE,0                                                           
         BE    XSRCNF              XSRC NOT FOUND                               
*                                                                               
         STC   R0,DUMYFLD+5        XSRC FOUND. POINT TO MODIFIED FIELD          
         LA    R2,DUMYFLD                                                       
*                                                                               
XSRCNF   DS    0X                                                               
         XIT1  REGS=(R2)                                                        
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET COMSCORE START/END DATES                                                  
***********************************************************************         
GETCSDAT NTR1  BASE=*,LABEL=*                                                   
         LA    R4,54(R3)           POINT TO FIRST DATE                          
         GOTO1 DATVAL,DMCB,(R4),WORK                                            
         OC    DMCB(4),DMCB        MMDDYY?                                      
         BZ    GCSDATX             NO, COULD BE THE SOURCE. SKIP IT             
         GOTO1 DATCON,DMCB,WORK,(3,TEMPFLD)                                     
*                                                                               
         AHI   R4,12               POINT TO END DATE                            
         GOTO1 DATVAL,DMCB,(R4),WORK                                            
         OC    DMCB(4),DMCB        MMDDYY?                                      
         BZ    GCSDATX             NO, COULD BE THE SOURCE. SKIP IT             
         GOTO1 DATCON,DMCB,WORK,(3,TEMPFLD+3)                                   
*                                                                               
GCSDATX  J     XIT                                                              
         LTORG                                                                  
***********************************************************************         
*RENMORE - CHECK IF SOURCE REN HAS BEEN REQUESTED WITH OTHER SOURCES            
***********************************************************************         
RENMORE  NTR1  BASE=*,LABEL=*                                                   
         SR    R0,R0               FLAG FOR REN                                 
         SR    R1,R1               FLAG FOR NON-REN                             
         L     RF,APODBKL                                                       
RENMO05  CLI   0(RF),X'FF'                                                      
         BE    RENMO10                                                          
         CLC   =C'REN',1(RF)                                                    
         BNE   *+12                                                             
         LHI   R0,1                                                             
         B     *+8                                                              
         LHI   R1,1                                                             
         CLI   0(RF),0                                                          
         BE    *+8                                                              
         LA    RF,PODBLNQ(RF)      BUMP BOOK TABLE IF RANGE                     
         LA    RF,PODBLNQ(RF)      BUMP BOOK TABLE                              
         B     RENMO05                                                          
*                                                                               
RENMO10  CHI   R0,1                                                             
         BNE   RENMONO                                                          
         CHI   R1,1                                                             
         BNE   RENMONO                                                          
*                                                                               
RENMOYES CR    RB,RB               BOTH REN AND NON-REN                         
         B     RENMOREX                                                         
RENMONO  CHI   RB,0                                                             
RENMOREX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
         DROP  R8                                                               
***********************************************************************         
*       VALIDATE OPTIONS                                              *         
***********************************************************************         
*                                                                               
VALOPXNS NMOD1 0,**VALOPXNS**,R8,RA                                             
         L     R2,4(R1)                                                         
         L     R9,8(R1)                                                         
         L     RC,12(R1)                                                        
*                                                                               
         CLI   0(R1),X'FF'                                                      
         BE    VALO04              ALSO SET 2ND TIME FLAG ON                    
         MVI   BOXOPT,C'Y'                                                      
         MVI   LEFTOPT,C'N'                                                     
         MVI   SPACOPT,1                                                        
         MVI   DOWNOPT,0                                                        
         MVI   DOWNOPT2,0                                                       
         MVI   DOWNCHAR,0                                                       
         MVI   PAYOPT,0                                                         
         MVI   WIDEOPT,C'N'                                                     
         MVI   TRACEOPT,C'N'                                                    
         MVI   NARROPT,C'N'                                                     
         MVI   PEROPT,C'M'                                                      
         MVI   SCNDTIME,C'N'                                                    
         MVI   TYPEOPT,0                                                        
         MVI   SCHEMOPT,0                                                       
         MVI   HUT52OPT,0                                                       
         MVI   DPTOPT,0                                                         
         MVI   WKINDS2,0                                                        
         MVI   PDDETOPT,0          DEFAULT VARIES BY REQ. DATA                  
         MVI   PDHUTTYP,0                                                       
         MVI   EXACTOPT,0                                                       
*                                                                               
*        MVI   OPTFLAG1,0                                                       
         NI    OPTFLAG1,X'FF'-ACMMWTQ                                           
         NI    OPTFLAG1,X'FF'-EXCLNTI                                           
         NI    OPTFLAG1,X'FF'-INCLOSQ                                           
         NI    OPTFLAG1,X'FF'-COMSRCQ                                           
         NI    OPTFLAG1,X'FF'-COMSRCOQ                                          
*                                                                               
         MVI   OPTMINTY,0                                                       
         MVI   OPTMINCM,0                                                       
         MVI   OPTSCWGT,0                                                       
*                                                                               
         XC    DAYPOPT(15),DAYPOPT                                              
         ZAP   MAXOPT,=PL8'999999999999999'                                     
         ZAP   MINOPT,=PL1'0'                                                   
         XC    UNIVOPT,UNIVOPT                                                  
         XC    HOMEOPT,HOMEOPT                                                  
         XC    PDRNKMAX,PDRNKMAX                                                
         XC    PDMGRP,PDMGRP                                                    
         MVI   PDBSTOPT,0                                                       
         MVI   PDDUROPT,0                                                       
*                                                                               
         L     RE,APDSYSC          CLEAR SYSCODE TABLE                          
         LHI   RF,PDSYSCDL                                                      
         XCEF                                                                   
         L     RE,APDSYSC                                                       
         MVC   0(2,RE),=X'FFFF'    SET END MARKER                               
*                                                                               
         L     RE,APDNTIFT         CLEAR NTI/PROGRAM CODE TABLE                 
         LA    RF,L'PDNTIFT                                                     
         XCEFL                                                                  
*                                                                               
         L     RE,APODMKTL         CLEAR MARKET LIST TABLE                      
         LA    RF,L'PODMKTL                                                     
         XCEFL                                                                  
         L     RE,APODMKTL                                                      
         MVI   0(RE),X'FF'         SET END MARKER                               
*                                                                               
         L     RE,APDAFFLT         CLEAR AFFILIATE LIST TABLE                   
         XC    0(L'PDAFFLT,RE),0(RE)                                            
         MVI   0(RE),X'FF'         SET END MARKER                               
*                                                                               
         L     RE,APDUFFLT         CLEAR UAFFILIATE LIST TABLE                  
         XC    0(L'PDUFFLT,RE),0(RE)                                            
         MVI   0(RE),X'FF'         SET END MARKER                               
*                                                                               
         L     RE,APODPRGL         CLEAR PROGRAM GROUP TABLE                    
         XC    0(L'PODPRGL,RE),0(RE)                                            
         MVI   0(RE),X'FF'         SET END MARKER                               
*                                                                               
*                                                                               
         CLC   AGENCY,=C'OM'                                                    
         BNE   *+8                                                              
         MVI   PDDUROPT,C'Y'                                                    
*                                                                               
         XC    PDSDPER,PDSDPER                                                  
         MVI   PDNUMQH,0           DEFAULT                                      
         MVI   PDWKSOPT,0          DEFAULT                                      
         MVI   PDINSEQ,0           DEFAULT                                      
         MVI   PDDAYLY,C'N'        DEFAULT                                      
         MVI   PDBASE,C'R'         DEFAULT                                      
         MVI   PDMETHOD,0          DEFAULT                                      
         MVI   MINDUR,0            DEFAULT                                      
         MVI   MINCOMM,0           DEFAULT                                      
         MVI   PDTRNKST,C'N'                                                    
         MVI   PDTRNKET,C'N'                                                    
*                                                                               
         L     RE,APODBKL                                                       
         CLC   =C'REN',1(RE)       RENTRAK ALWAYS SET QH=1                      
         BNE   *+8                                                              
         MVI    PDNUMQH,1                                                       
*                                                                               
         L     R1,ATWA                                                          
         USING T325FFD,R1                                                       
         CLC   CONWHEN(3),=C'OV,'  STOP EOD FUCKUP                              
         BNE   VALO02                                                           
         CLI   CONWHEN+5,C' '                                                   
         BH    VALO02                                                           
         MVI   CONWHEN+5,C'.'                                                   
         MVI   CONWHENH+5,6                                                     
         FOUT  CONWHENH                                                         
         DROP  R1                                                               
*                                                                               
VALO02   MVI   PDQSPILL,C'N'       SET NO SPILL                                 
         MVI   PDIUNOPT,C'Y'       DEFAULT, USE IUN                             
         CLI   PDOVSYS,3           UNLESS NET SYSTEM                            
         BNE   *+8                                                              
         MVI   PDIUNOPT,C'N'                                                    
         B     *+8                                                              
*                                                                               
VALO04   MVI   SCNDTIME,C'Y'                                                    
         CLI   5(R2),0                                                          
         BE    OPTX                                                             
         MVI   FIELDERR,1                                                       
*                                                                               
         ICM   RE,15,AVALBLCK                                                   
         LA    RF,L'VALBLCK                                                     
         XCEFL                                                                  
*                                                                               
         GOTO1 SCANNER,DMCB,(20,(R2)),(X'8F',AVALBLCK),0 15 LINES MAX           
         CLI   4(R1),6             USE LONG SCAN IF BUFFER CAN HANDLE           
         BH    VALO06               OTHERWISE LEAVE WHAT WE GOT                 
         MVI   OPTSCNLN,87                                                      
         GOTO1 SCANNER,DMCB,(65,(R2)),(7,AVALBLCK),0                            
         B     VALO08                                                           
*                                                                               
VALO06   MVI   OPTSCNLN,42                                                      
*                                                                               
VALO08   ZIC   R0,4(R1)                                                         
         ICM   R4,15,AVALBLCK                                                   
         LTR   R0,R0                                                            
         BZ    BADOPT                                                           
         STC   R0,NUMLINES                                                      
*                                                                               
VALO10   CLC   12(3,R4),=C'BOX'    BOX OPTION                                   
         BNE   VALO12                                                           
         MVC   BOXOPT,22(R4)                                                    
         B     OPTEND                                                           
*                                                                               
VALO12   CLC   12(3,R4),=C'PAY'    PAY OPTION                                   
         BNE   VALO14                                                           
         MVC   PAYOPT,22(R4)                                                    
         B     OPTEND                                                           
*                                                                               
VALO14   CLC   12(4,R4),=C'LEFT'   LEFT OPTION                                  
         BNE   VALO16                                                           
         MVI   LEFTOPT,C'Y'                                                     
         B     OPTEND                                                           
*                                                                               
VALO16   CLC   12(2,R4),=C'S   '   SPACING OPTION                               
         BNE   VALO18                                                           
         MVC   SPACOPT,11(R4)                                                   
         CLI   SPACOPT,1                                                        
         BL    BADOPT                                                           
         CLI   SPACOPT,5                                                        
         BH    BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
VALO18   CLC   12(4,R4),=C'DOWN'   DOWNLOADING OPTION                           
         BNE   VALO22                                                           
         OI    DOWNOPT,DOWNON                                                   
         OI    REQRTYP,REQTDOWN                                                 
         CLC   16(3,R4),=C'FIX'    DOWNFIX=REMOVE EXTRA COLUMNS                 
         BNE   *+12                                                             
         OI    DOWNOPT2,GLD2FIX                                                 
         B     VALO20                                                           
         CLC   16(4,R4),=C'HEAD'   DOWNLOAD HEADLINES OPTION                    
         BNE   *+12                                                             
         OI    DOWNOPT,GLDLHEAD                                                 
         B     VALO20                                                           
         CLC   16(3,R4),=C'TOT'    DOWNLOAD TOTALS OPTION                       
         BNE   VALO20                                                           
         OI    DOWNOPT,GLDLTOTS                                                 
         MVI   DOWNCHAR,0                                                       
         NI    DOWNOPT2,255-GLDLTTXT                                            
         CLI   19(R4),C'T'                                                      
         BNE   *+8                                                              
         OI    DOWNOPT2,GLDLTTXT   DOWNLOAD TOTAL ROUTINE TEXT                  
         CLI   1(R4),0                                                          
         BNH   *+10                                                             
         MVC   DOWNCHAR,22(R4)     CHARACTER FOR DOWNLOADING TOTALS             
*                                                                               
VALO20   L     R1,ATWA                                                          
         USING T325FFD,R1                                                       
         CLI   CONOUT,C' '                                                      
         BH    OPTEND                                                           
         MVC   CONOUT(8),=CL8'DOWN'                                             
         MVI   CONOUTH+5,4                                                      
         MVC   TWAOUT,CONOUT                                                    
         FOUT  CONOUTH                                                          
         DROP  R1                                                               
         B     OPTEND                                                           
*                                                                               
VALO22   CLC   12(4,R4),=C'WIDE'   WIDE PRINTING (165)                          
         BNE   VALO24                                                           
         MVI   WIDEOPT,C'Y'                                                     
         B     OPTEND                                                           
*                                                                               
VALO24   CLC   12(5,R4),=C'TRACE'  TRACE OPTION                                 
         BNE   VALO26                                                           
         MVI   TRACEOPT,C'Y'                                                    
         MVI   PDQTRACE,C'Y'       +++ ALSO SET SPOTIO TRACE                    
         B     OPTEND                                                           
*                                                                               
VALO26   CLC   12(6,R4),=C'NARROW'  NARROW OPTION                               
         BNE   VALO28                                                           
         MVI   NARROPT,C'Y'                                                     
         B     OPTEND                                                           
*                                                                               
VALO28   CLC   12(5,R4),=C'EXACT'  EXACT DATES OPTION                           
         BNE   VALO30                                                           
         MVI   EXACTOPT,C'Y'                                                    
         B     OPTEND                                                           
*                                                                               
VALO30   CLC   12(2,R4),=C'BK'     BOOK                                         
         BNE   VALO34                                                           
         CLI   PDOVSYS,3                                                        
         BNE   VALO186             USED FOR UPGRADES                            
         CLI   15(R4),C' '                                                      
         BNE   BADOPT                                                           
         LA    R3,PDQBOOK                                                       
         CLI   14(R4),C' '                                                      
         BE    VALO32                                                           
         CLI   14(R4),C'1'                                                      
         BE    VALO32                                                           
         LA    R3,PDQBKS                                                        
         CLI   14(R4),C'2'                                                      
         BE    VALO32                                                           
         LA    R3,PDQBKS+4                                                      
         CLI   14(R4),C'3'                                                      
         BNE   BADOPT                                                           
*                                                                               
VALO32   MVC   0(4,R3),=C'ACT '                                                 
         CLC   22(4,R4),=C'ACT '                                                
         BE    OPTEND                                                           
         GOTO1 DATVAL,DMCB,(2,22(R4)),DUB                                       
         OC    0(4,R1),0(R1)                                                    
         BZ    BADOPT                                                           
         MVC   0(4,R3),DUB                                                      
         B     OPTEND                                                           
*                                                                               
VALO34   CLC   12(5,R4),=C'MENU '  DEMO MENU                                    
         BNE   VALO36                                                           
         MVC   PDQDEMOS,22(R4)                                                  
         B     OPTEND                                                           
*                                                                               
VALO36   CLC   12(5,R4),=C'DEMO '  DEMO OPTION                                  
         BNE   VALO38                                                           
         CLC   22(3,R4),=C'TGT'                                                 
         BNE   *+12                                                             
         MVI   DEMOPT,DEMOTGT                                                   
         B     OPTEND                                                           
         CLC   22(3,R4),=C'SEC'                                                 
         BNE   *+12                                                             
         MVI   DEMOPT,DEMOSEC                                                   
         B     OPTEND                                                           
*                                                                               
         MVC   PDQDEMOS,22(R4)     *** FOR THE TIME BEING                       
         B     OPTEND              ***                                          
*                                                                               
VALO38   CLC   12(6,R4),=C'ROUND ' ROUND                                        
         BNE   VALO40                                                           
         OI    COLIND,COLIRND                                                   
         B     OPTEND                                                           
*                                                                               
VALO40   CLC   12(6,R4),=C'SPILL ' SPILL                                        
         BNE   VALO46                                                           
         CLI   1(R4),1                                                          
         BNE   BADOPT                                                           
         MVC   PDQSPILL,22(R4)                                                  
         CLI   22(R4),C'N'         NO SPILL                                     
         BE    VALO44                                                           
         CLI   22(R4),C'C'         COMBINED                                     
         BE    VALO44                                                           
         CLI   22(R4),C'S'         SEPARATE                                     
         BE    VALO44                                                           
         B     BADOPT                                                           
*                                                                               
VALO44   L     R1,APODMKTL                                                      
         CLI   0(R1),X'FF'         DO WE HAVE A MARKET LIST?                    
         BE    OPTEND              NO                                           
         L     RE,APODNET                                                       
         LA    RF,LPODNET                                                       
         XCEF                                                                   
         B     OPTEND                                                           
*                                                                               
VALO46   CLC   12(9,R4),=C'DEMOPGRP '                                           
         BNE   VALO48                                                           
         MVI   DEMGRP,C'P'                                                      
         B     OPTEND                                                           
*                                                                               
VALO48   CLC   12(9,R4),=C'DEMROUND '   DEMO ROUNDING                           
         BNE   VALO50                                                           
         CLI   22(R4),C'Y'                                                      
         BE    OPTEND                                                           
         OI    COLIND,COLINDR                                                   
         CLI   22(R4),C'N'                                                      
         BNE   BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
VALO50   CLC   12(7,R4),=C'NOHEAD '     NO HEADINGS FOR DOWNLOAD                
         BNE   VALO52                                                           
         OI    DOWNOPT,DOWNNOH                                                  
         B     OPTEND                                                           
*                                                                               
VALO52   CLC   12(6,R4),=C'SCHEME'                                              
         BNE   VALO54                                                           
         OC    PODPSTRT,PODPSTRT   OPTION FOR PROGRAM AVG. ONLY                 
         BZ    BADOPT                                                           
         MVC   SCHEMOPT,22(R4)                                                  
         B     OPTEND                                                           
*                                                                               
VALO54   CLC   12(2,R4),=C'52'                                                  
         BNE   VALO56                                                           
         OC    PODPSTRT,PODPSTRT   OPTION FOR PROGRAM AVG. ONLY                 
         BNZ   *+12                                                             
         CLI   22(R3),C'Y'                                                      
         BE    BADOPT                                                           
         MVC   HUT52OPT,22(R4)                                                  
         B     OPTEND                                                           
*                                                                               
VALO56   CLC   12(4,R3),=C'TYPE'   HUT TYPE (D, A, I OR C)                      
         BNE   VALO58                                                           
         OC    PODPSTRT,PODPSTRT   OPTION FOR PROGRAM AVG. ONLY                 
         BZ    BADOPT                                                           
         MVC   TYPEOPT,22(R4)                                                   
         CLI   22(R4),C'D'         DIARY                                        
         BE    OPTEND                                                           
         CLI   22(R4),C'A'         ASCRIBED                                     
         BE    OPTEND                                                           
         CLI   22(R4),C'I'         INTEGRATED                                   
         BE    OPTEND                                                           
         CLI   22(R4),C'C'         CONFORMED                                    
         BE    OPTEND                                                           
         CLI   22(R4),C'Z'         Z-BOOK (TEST)                                
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
VALO58   CLC   12(3,R4),=C'DPT'    DAYPART OPTION                               
         BNE   VALO60                                                           
         OC    PODPSTRT,PODPSTRT   OPTION FOR PROGRAM AVG. ONLY                 
         BZ    BADOPT                                                           
         GOTO1 =A(OVFLRTN),DMCB,(16,DUB),(RC),RR=PGNR   GETDPT                  
         BNE   BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
VALO60   CLC   12(4,R4),=C'UNIV'   UNIVERSE OPTION                              
         BNE   VALO62                                                           
         OC    PODPSTRT,PODPSTRT   OPTION FOR PROGRAM AVG. ONLY                 
         BZ    BADOPT                                                           
         OC    8(4,R4),8(R4)                                                    
         BZ    BADOPT                                                           
         MVC   UNIVOPT,8(R4)                                                    
         BAS   RE,CKUNIV           SEE IF VALID UNIVERSE                        
         BNE   BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
VALO62   CLC   12(4,R4),=C'HOMES'  HOMES OPTION                                 
         BNE   VALO64                                                           
         OC    PODPSTRT,PODPSTRT   OPTION FOR PROGRAM AVG. ONLY                 
         BZ    BADOPT                                                           
         OC    8(4,R4),8(R4)                                                    
         BZ    BADOPT                                                           
         MVC   HOMEOPT,8(R4)                                                    
         B     OPTEND                                                           
*                                                                               
VALO64   CLC   12(3,R4),=C'HUT'    HUT=123.45 PERCENT ADJUST                    
         BNE   VALO66                                                           
         ZIC   R1,1(R4)            L'SECOND HALF                                
         ST    R1,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,22(R4)                                              
         CLI   DMCB,X'FF'                                                       
         BE    BADOPT                                                           
         OC    DMCB+4(2),DMCB+4                                                 
         BNZ   BADOPT                                                           
         MVC   HUTOVER,DMCB+6                                                   
         B     OPTEND                                                           
*                                                                               
VALO66   CLC   12(4,R4),=C'WEEK'   WEEK BREAKOUT                                
         BNE   VALO70                                                           
         CLI   1(R4),0                                                          
         BZ    VALO68                                                           
         CLI   16(R4),C'S'         HAS TO BE WEEKS=ALL                          
         BNE   VALO70                                                           
         CLC   =C'ALL',22(R4)                                                   
         BNE   OPTEND                                                           
         MVI   PDWKSOPT,X'FF'                                                   
         B     OPTEND                                                           
*                                                                               
VALO68   MVI   PEROPT,C'W'                                                      
         B     OPTEND                                                           
*                                                                               
VALO70   CLC   12(5,R4),=C'MONTH'  MONTH BREAKOUT                               
         BNE   *+12                                                             
         MVI   PEROPT,C'M'                                                      
         B     OPTEND                                                           
         CLC   12(7,R4),=C'QUARTER'  QUARTER BREAKOUT                           
         BNE   VALO72                                                           
         MVI   PEROPT,C'Q'                                                      
         B     OPTEND                                                           
*                                                                               
VALO72   CLC   12(7,R4),=C'DAYPART'  DAYPART OPTION                             
         BNE   VALO74                                                           
         MVI   DAYPOPT,C'Y'                                                     
         B     OPTEND                                                           
*                                                                               
VALO74   CLC   12(2,R4),=C'15'       15 MINUTE OPTION                           
         BNE   VALO76                                                           
         CLI   22(R4),C'Y'                                                      
         BNE   *+12                                                             
         MVI   FTNYOPT,C'Y'        INCLUDE 15 MINUTE PROGRAMS                   
         B     OPTEND                                                           
         CLI   22(R4),C'O'                                                      
         BNE   BADOPT                                                           
         MVI   FTNYOPT,C'O'        ONLY 15 MINUTE PROGRAMS                      
         B     OPTEND                                                           
*                                                                               
VALO76   CLC   12(4,R4),=C'SOLO'     SINGLE TELECAST OPTION                     
         BNE   VALO78                                                           
         MVC   SOLOOPT,22(R4)                                                   
         CLI   22(R4),C'N'                                                      
         BE    OPTEND                                                           
         CLI   22(R4),C'Y'                                                      
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
VALO78   CLC   12(3,R4),=C'SEP'      SHOULD SEPTEMBER PRINT                     
         BNE   VALO80                                                           
         MVC   NSEPOPT,22(R4)                                                   
         CLI   22(R4),C'N'                                                      
         BE    OPTEND                                                           
         CLI   22(R4),C'Y'                                                      
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
VALO80   CLC   12(3,R4),=C'MAX'      MAXIMUM DEMO AMOUNT                        
         BNE   VALO81                                                           
         ZIC   R1,1(R4)            L'SECOND HALF                                
         ST    R1,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,(1,22(R4))                                          
         ORG   *-2                                                              
         CLI   PRECOPT,C'Y'        USING CABLE PRECISION?                       
         BNE   *+8                 NO                                           
         MVI   0(R1),2             YES, CHANGE DECIMALS                         
         BASR  RE,RF                                                            
         CLI   DMCB,X'FF'                                                       
         BE    BADOPT                                                           
         ICM   R1,15,DMCB+4                                                     
         CVD   R1,MAXOPT                                                        
         B     OPTEND                                                           
*                                                                               
VALO81   CLC   =C'MINTYP',12(R4)   MINUTE TYPE                                  
         BNE   VALO82                                                           
         LA    RE,22(R4)                                                        
VALO81A  CLI   0(RE),C'C'          C'C' COMMERCIAL                              
         BNE   *+12                                                             
         OI    OPTMINTY,OPTMCOMQ                                                
         B     VALO81B                                                          
         CLI   0(RE),C'R'          C'R' PROMO                                   
         BNE   *+12                                                             
         OI    OPTMINTY,OPTMPROQ                                                
         B     VALO81B                                                          
         CLI   0(RE),C'S'          C'S' PSA                                     
         BNE   BADOPT                                                           
         OI    OPTMINTY,OPTMPSAQ                                                
VALO81B  LA    RE,1(RE)            CK FOR MORE INPUT TYPES                      
         CLI   0(RE),0             EXIT IF END OF FIELD                         
         BE    OPTEND                                                           
         CLI   0(RE),C' '          EXIT IF NO MORE OPTIONS                      
         BE    OPTEND                                                           
         CLI   0(RE),C','          EXIT IF NEXT OPTION                          
         BE    OPTEND                                                           
         CLI   0(RE),C'/'          DELIMITER FOR MORE INPUT TYPES               
         BNE   BADOPT                                                           
         LA    RE,1(RE)            SKIP OVER DELIMITER                          
         B     VALO81A             GO CK INPUT TYPE                             
*                                                                               
VALO82   CLC   12(3,R4),=C'MIN'      MINIMUM DEMO AMOUNT                        
         BNE   VALO84                                                           
         ZIC   R1,1(R4)            L'SECOND HALF                                
         ST    R1,DMCB+4                                                        
         CLC   12(4,R4),=C'MIND'    MINDUR                                      
         BE    VALO83                                                           
         CLC   =C'MINCOM',12(R4)                                                
         BE    VALO330                                                          
         GOTO1 CASHVAL,DMCB,(1,22(R4))                                          
         ORG   *-2                                                              
         CLI   PRECOPT,C'Y'        USING CABLE PRECISION?                       
         BNE   *+8                 NO                                           
         MVI   0(R1),2             YES, CHANGE DECIMALS                         
         BASR  RE,RF                                                            
         CLI   DMCB,X'FF'                                                       
         BE    BADOPT                                                           
         ICM   R1,15,DMCB+4                                                     
         CVD   R1,MINOPT                                                        
         B     OPTEND                                                           
*                                                                               
VALO83   GOTO1 CASHVAL,DMCB,(0,22(R4))                                          
         CLI   DMCB,X'FF'                                                       
         BE    BADOPT                                                           
         ICM   R1,15,DMCB+4                                                     
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         CHI   R1,240                                                           
         BH    BADOPT                                                           
         STC   R1,MINDUR                                                        
         B     OPTEND                                                           
*                                                                               
VALO84   CLC   12(3,R4),=C'TOP'      PRINT ONLY TOP X SHOWS                     
         BNE   VALO86                                                           
         TM    3(R4),X'80'                                                      
         BZ    BADOPT                                                           
         MVC   PDRNKMAX,8(R4)                                                   
         B     OPTEND                                                           
*                                                                               
VALO86   CLC   12(5,R4),=C'STACK'    STACK DEFINITION                           
         BNE   VALO88                                                           
         CLI   PDGSTACK,X'FF'      USING GENDER STACK?                          
         BE    BADOPT5             YES, ERROR                                   
         GOTO1 =A(OVFLRTN),DMCB,(0,DUB),(RC),RR=PGNR VALSTACK                   
         BNE   BADOPT                                                           
         MVI   PDSTACK,X'FF'                                                    
         GOTO1 =A(OVFLRTN),DMCB,(1,DUB),(RC),RR=PGNR VALSTDEM                   
         BNE   BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
VALO88   CLC   12(4,R4),=C'PRGS'     FILTER ON NTI CODES                        
         BNE   VALO90                                                           
         GOTO1 =A(OVFLRTN),DMCB,(5,DUB),(RC),RR=PGNR VALNTINM                   
         BNE   BADPRGS                                                          
         B     OPTEND                                                           
*                                                                               
VALO90   CLC   12(4,R4),=C'XBOX'     EXTRA BOX AFTER ROWS                       
         BNE   VALO92                                                           
         OI    WKINDS2,X'40'                                                    
         B     OPTEND                                                           
*                                                                               
VALO92   CLC   12(4,R4),=C'BOOK'       BOOK OPTION                              
         BE    VALO184                                                          
         CLC   12(2,R4),=C'BA'     OTHER OPTIONS START WITH B                   
         BE    VALO94                                                           
         CLC   12(2,R4),=C'BO'         BREAKOUT OPTION                          
         BE    *+10                                                             
         CLC   12(1,R4),=C'B'          BREAKOUT OPTION                          
         BNE   VALO94                                                           
         MVC   BRKOOPT,22(R4)                                                   
         CLI   22(R4),C'N'                                                      
         BE    OPTEND                                                           
         CLI   22(R4),C'Y'                                                      
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
VALO94   CLC   12(3,R4),=C'PRE'      CABLE PRECISSION                           
         BNE   VALO98                                                           
         CLC   12(4,R4),=C'PREM'     PREMIERE OPTIONS                           
         BE    VALO96                                                           
         MVI   PRECOPT,C'Y'                                                     
         CLC   22(3,R4),=CL3'CAB'  CABLE PRECISION?                             
         BNE   VALO95              NO                                           
         CP    MINOPT,=P'0'        YES, ERROR IF MIN/MAX ALREADY SET            
         BNE   MINERR                                                           
         CP    MAXOPT,=PL8'999999999999999'                                     
         BNE   MINERR                                                           
         B     OPTEND                                                           
*                                                                               
VALO95   CLC   =C'NO',22(R4)                                                    
         BNE   BADOPT                                                           
         MVI   PRECOPT,C'N'                                                     
         B     OPTEND                                                           
*                                                                               
VALO96   MVI   PDPRMOPT,C'Y'                                                    
         CLC   12(5,R4),=C'PREM+'                                               
         BNE   *+8                                                              
         MVI   PDPRMOPT,C'+'                                                    
         B     OPTEND                                                           
*                                                                               
VALO98   CLC   12(3,R4),=C'VAR'      VARIOUS DAYS OPTION                        
         BNE   VALO100                                                          
         MVI   VAROPT,C'Y'                                                      
         B     OPTEND                                                           
*                                                                               
VALO100  CLC   12(3,R4),=C'DAY'      BREAKOUT OF M-F, AND M-S DAYS              
         BNE   VALO102                                                          
         MVC   DAYSOPT(1),22(R4)                                                
         CLI   22(R4),C'I'                                                      
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
VALO102  CLC   12(5,R4),=C'HHHUT'    BREAKOUT HUTS BY HALF HOUR                 
         BNE   VALO104                                                          
         MVI   HHHUT,C'Y'                                                       
         B     OPTEND                                                           
*                                                                               
VALO104  CLC   =C'MRKT',12(R4)       LOOK FOR MARKET                            
         BNE   VALO122                                                          
         CLI   1(R4),0                                                          
         BE    BADOPT                                                           
         CLC   =C'CCV',PODBEXT                                                  
         BE    BADOPT                                                           
         L     R5,APODMKTL         ADDRESS MARKET LIST                          
         MVC   0(1,R4),1(R4)       SHIFT DATA FIRST TIME IN                     
         MVC   2(1,R4),3(R4)                                                    
         MVC   4(4,R4),8(R4)                                                    
         MVC   12(10,R4),22(R4)                                                 
*                                                                               
VALO106  TM    2(R4),X'80'         2ND HALF IS VALID NUMERIC                    
         BO    VALO108             GET MARKET NUMBER                            
         TM    2(R4),X'40'         2ND HALF IS VALID ALPHA                      
         BO    VALO114             GO LOOK FOR ALPHA MARKET                     
         B     BADOPT              BAD MARKET                                   
*                                                                               
VALO108  ICM   R1,15,4(R4)         BINARY VALUE OF MARKET=???                   
         BZ    BADOPT                                                           
         STCM  R1,3,0(R5)                                                       
         CLI   PODBEXT+4,C'A'      EMI-A/NMI-A USES ARB MPA                     
         BE    VALO110                                                          
         CLI   PODBSRC,C'N'                                                     
         BNE   VALO110                                                          
         AH    R1,=H'400'                                                       
         STCM  R1,3,0(R5)                                                       
*                                                                               
VALO110  LA    R5,2(R5)            GET NEXT SPOT IN LIST                        
         MVI   0(R5),X'FF'         MARK IN CASE LAST                            
*                                                                               
         L     RF,APODMKTL                                                      
         LA    RF,L'PODMKTL(RF)    A(END OF MARKET LIST)                        
         CR    R5,RF                                                            
         BNL   BADOPT3             ERROR IF PAST END                            
*                                                                               
         ZIC   RE,OPTSCNLN                                                      
         AR    R4,RE                                                            
         AI    FIELDERR,1                                                       
         ZIC   RE,NUMLINES                                                      
         BCTR  RE,0                                                             
         STC   RE,NUMLINES                                                      
         LTR   RE,RE               ANY FIELDS LEFT?                             
         BZ    VALO112             NO, BACK IT UP                               
         CLI   1(R4),0             IS THERE A SECOND FIELD?                     
         BE    VALO106             NO, WE'RE STILL IN MRKT                      
*                                                                               
VALO112  ZIC   RE,NUMLINES                                                      
         LA    RE,1(RE)                                                         
         STC   RE,NUMLINES                                                      
         ZIC   RE,OPTSCNLN                                                      
         SR    R4,RE                                                            
         B     OPTEND                                                           
*                                                                               
*--------------------------------------------------------------                 
* CHECK IF THE ALPHA MARKET EXIST IN THE CONTROL FILE                           
*--------------------------------------------------------------                 
*                                                                               
VALO114  XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING CTDMREC,R1                                                       
         MVI   CTDMKTYP,CTDMKTEQ   SET UP KEY                                   
         MVI   CTDMKTY2,CTDMKT2E                                                
         MVI   CTDMKMED,C'T'       MEDIA                                        
         CLI   PODBMED,C'C'                                                     
         BNE   *+8                                                              
         MVI   CTDMKMED,C'C'       MEDIA                                        
         CLI   PODBMED,C'R'                                                     
         BNE   *+8                                                              
         MVI   CTDMKMED,C'R'       MEDIA                                        
         MVC   CTDMKSRC,PODBSRC    SOURCE (A/N)                                 
         CLI   CTDMKSRC,C'F'                                                    
         BNE   *+8                                                              
         MVI   CTDMKSRC,C'N'       FUSION READS NSI MARKETS                     
         CLI   PODBEXT+4,C'A'      EMI-A/NMI-A USES ARBITRON MPA                
         BNE   *+8                                                              
         MVI   CTDMKSRC,C'A'                                                    
         MVC   CTDMKMKT,12(R4)     MARKET NAME                                  
         MVC   CTDMKBKT,PODBBTY                                                 
         CLI   CTDMKBKT,0                                                       
         BNE   *+14                                                             
         L     RE,APODNET                                                       
         MVC   CTDMKBKT,5(RE)      BOOK TYPE                                    
         CLI   PODBMED,C'O'        OVERNIGHTS SHOULD GET SPOT DEFAULT           
         BNE   *+8                                                              
         MVI   CTDMKBKT,0                                                       
         CLI   CTDMKBKT,C'A'       DEFAULT FOR NON-SPOT                         
         BE    VALO116                                                          
         CLC   =C'SQADH',PODBEXT   SQUAD HISPANIC IS BOOKTYPE 'R'               
         BNE   *+12                                                             
         MVI   CTDMKBKT,C'R'                                                    
         B     VALO116A                                                         
         CLI   CTDMKBKT,C'S'       DEFAULT FOR SQAD                             
         BE    VALO116                                                          
         CLI   CTDMKBKT,0          DEFAULT FOR SPOT                             
         BNE   *+8                                                              
*                                                                               
VALO116  MVI   CTDMKBKT,X'FF'      DEFAULT BOOK TYPE                            
VALO116A MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI  '),=C'CTFILE  ',KEY,KEY,0             
         CLC   KEY+1(22),KEYSAVE+1 CAN'T FIND IT? 1ST BYTE DESTROYED            
*        BNE   BADOPT              BAD ONE                                      
         BE    VALO118                                                          
         CLI   KEYSAVE+CTDMKBKT-CTDMREC,X'FF' LOOKING FOR DEFAULT BTYP?         
         BE    BADOPT              BAD ONE                                      
         LA    R1,KEY                                                           
         MVC   KEY,KEYSAVE         BTYP NOT FOUND. GO READ DEFAULT              
         MVI   CTDMKTYP,CTDMKTEQ   RESTORE FIRST BYTE                           
         B     VALO116                                                          
*                                                                               
VALO118  LA    R1,KEY              RELOAD R1                                    
         MVC   0(2,R5),CTDMKNUM    GOT IT, SAVE MARKET NUMBER                   
         CLI   PODBEXT+4,C'A'      EMI-A/NMI-A USES ARB MPA                     
         BE    VALO110                                                          
         CLI   PODBSRC,C'N'        NSI MARKET NEEDS ADJUSTMENT                  
         BNE   VALO110                                                          
         SR    R1,R1                                                            
         ICM   R1,3,0(R5)                                                       
         AH    R1,=H'400'                                                       
         STCM  R1,3,0(R5)                                                       
         B     VALO110                                                          
         DROP  R1                                                               
*                                                                               
VALO122  CLC   =C'MPA',12(R4)      LOOK FOR INDEX                               
         BNE   VALO146                                                          
         CLI   1(R4),0                                                          
         BE    OPTEND                                                           
         TM    3(R4),X'80'         NUMERIC INPUT?                               
         BO    VALO142             YES, SAVE THE INDEX                          
         TM    3(R4),X'40'         IF NOT ALPHA THEN                            
         BNO   BADPRGS             IT IS A BAD PROGRAM                          
         L     R1,=F'9600'                                                      
         LR    R3,R4               SAVE BLOCK ADDR                              
         CLI   22(R4),C'C'         IS IT A CLEARED PROGRAM?                     
         BNE   VALO124                                                          
         LA    R3,1(R4)            GET RID OF THE 'C'                           
         B     VALO126                                                          
*                                                                               
VALO124  LA    R1,100(R1)          NOT, HUNDREDS PLACE IS 7                     
*                                                                               
VALO126  CLC   =C'PRIME',22(R3)                                                 
         BNE   VALO128                                                          
         LA    R1,1(R1)                                                         
         B     VALO132                                                          
*                                                                               
VALO128  CLC   =C'DAY',22(R3)                                                   
         BNE   VALO130                                                          
         LA    R1,2(R1)                                                         
         B     VALO132                                                          
*                                                                               
VALO130  CLC   =C'KIDS',22(R3)                                                  
         BNE   BADPRGS             BAD PROGRAM                                  
         LA    R1,3(R1)                                                         
*                                                                               
VALO132  L     RE,APODNET                                                       
         CLC   =C'ABC',0(RE)                                                    
         BNE   VALO134                                                          
         LA    R1,10(R1)                                                        
         B     VALO144                                                          
*                                                                               
VALO134  CLC   =C'CBS',0(RE)                                                    
         BNE   VALO136                                                          
         LA    R1,20(R1)                                                        
         B     VALO144                                                          
*                                                                               
VALO136  CLC   =C'NBC',0(RE)                                                    
         BNE   VALO138                                                          
         LA    R1,30(R1)                                                        
         B     VALO144                                                          
*                                                                               
VALO138  CLC   =C'FOX',0(RE)                                                    
         BNE   VALO140                                                          
         LA    R1,40(R1)                                                        
         B     VALO144                                                          
*                                                                               
VALO140  CLC   =C'ZZZ',0(RE)                                                    
         BNE   BADPRGS                                                          
         LA    R1,50(R1)                                                        
         B     VALO144                                                          
*                                                                               
VALO142  ICM   R1,15,8(R4)         BINARY VALUE OF INDEX=???                    
*                                                                               
VALO144  STCM  R1,3,MPAINDEX                                                    
         B     OPTEND                                                           
*                                                                               
VALO146  CLC   =C'PAV',12(R4)      PAV=ALL, BEST, LAYOUT                        
         BNE   VALO150                                                          
         CLI   22(R4),C'A'                                                      
         BE    VALO148                                                          
         CLI   22(R4),C'B'                                                      
         BE    VALO148                                                          
         CLI   22(R4),C'L'                                                      
         BNE   BADOPT                                                           
*                                                                               
VALO148  MVC   PDBSTOPT,22(R4)     SET OPTION FOR DBBEST                        
         B     OPTEND                                                           
*                                                                               
VALO150  CLC   =C'DUR',12(R4)      DURATION = PROGRAM/TIME                      
         BNE   VALO158                                                          
         CLI   PDMETHOD,1                                                       
         BH    BADOPT                                                           
         MVI   PDMETHOD,1                                                       
         CLI   22(R4),C'M'         MINUTE WIEGHTING-START TIME FLTR             
         BE    VALO157                                                          
         CLI   22(R4),C'-'                                                      
         BE    VALO152                                                          
         CLI   23(R4),C'-'                                                      
         BE    VALO154                                                          
         CLI   22(R4),C'P'         PROGRAM = Y AND PROG CROSSING TIME           
         BE    VALO156                                                          
         CLI   22(R4),C'T'         TIME    = X'00' (QUATER HOUR) STF            
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
VALO152  MVI   PDTRNKST,C'Y'                                                    
         CLI   24(R4),C'-'                                                      
         BNE   VALO156                                                          
*                                                                               
VALO154  MVI   PDTRNKET,C'Y'                                                    
*                                                                               
VALO156  MVI   PDDUROPT,C'Y'       SET OPTION FOR PROGRAM DURATION              
         B     OPTEND                 START TIME CROSSES REQ. TIME              
*                                                                               
VALO157  MVI   PDDUROPT,C'M'       SET OPTION FOR MINUTE WEIGHTING              
         B     OPTEND                 START TIME WITHIN REQ. TIME               
*                                                                               
VALO158  CLC   =C'QH',12(R4)       QH=(1/2)                                     
         BNE   VALO160                                                          
         TM    3(R4),X'80'         HAS TO BE A NUMBER                           
         BO    *+8                                                              
         B     BADOPT                                                           
         MVC   PDNUMQH,11(R4)                                                   
*                                                                               
         L     RE,APODBKL                                                       
         CLC   =C'CNADT',1(RE)     SOURCE=CNADT OR CNAWT?                       
         BE    *+10                (CABLE NAD OR MVGO - TP)                     
         CLC   =C'CNAWT',1(RE)                                                  
         BNE   OPTEND                                                           
         TM    11(R4),X'01'        YES. DON'T ALLOW QH=ODD                      
         BO    BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
VALO160  CLC   =C'SID',12(R4)      NEW SID, SCHEME                              
         BNE   VALO168                                                          
         CLI   PDOVSYS,3         NET SYSTEM CAN'T USE SIDS                      
         BE    BADSYS                                                           
         CLI   PDSIDOPT,C'X'                                                    
         BE    BADSYS                                                           
         CLI   PDOVSYS,8                                                        
         BNE   VALO164                                                          
         CLI   OFFLINE,C'Y'                                                     
         BNE   VALO162                                                          
         ICM   RF,15,PDAUTL                                                     
         MVC   4(1,RF),PDSSENUM    OFFLINE SWITCH                               
         B     VALO164                                                          
*                                                                               
VALO162  GOTO1 SWITCH,DMCB,(0,=C'SPT'),0    SPOT SYS HAS SID RECS               
         CLI   4(R1),0                      HAVE TO BE ABLE TO SWITCH           
         BZ    VALO164                                                          
         DC    H'0'                                                             
*                                                                               
VALO164  MVC   PDSDSCHM,=C'ALL'                                                 
         CLI   1(R4),0                                                          
         BE    BADSID                                                           
         CLI   1(R4),2                                                          
         BL    BADSID                                                           
         CLI   1(R4),3                                                          
         BH    BADSID                                                           
         MVC   PDSDSCHM,22(R4)                                                  
* MAKE SURE SCHEME EXISTS                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'0C'                                                        
         MVC   KEY+1(1),PDBAGYMD                                                
         CLC   =C'ALL',PDSDSCHM                                                 
         BE    VALO166                                                          
         GOTO1 CLPACK,DMCB,PDSDSCHM,KEY+2                                       
*                                                                               
VALO166  GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BADSID                                                           
         MVC   PDSDDSKA,KEY+14     SAVE DISK ADDRESS                            
         B     OPTEND                                                           
*                                                                               
* VALIDATE SID PERIOD                                                           
VALO168  CLC   =C'PER',12(R4)                                                   
         BNE   VALO184                                                          
         OC    PDSDDSKA,PDSDDSKA   GOT TO HAVE SID                              
         BZ    BADSID                                                           
*                                                                               
         LA    R0,5                                                             
         LA    R1,22(R4)           POINT TO PERIOD                              
*                                                                               
VALO170  CLI   0(R1),C' '                                                       
         BE    VALO172                                                          
         CLI   0(R1),C'/'                                                       
         BE    VALO172                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,VALO170                                                       
         B     PERERR                                                           
*                                                                               
VALO172  LA    RE,5                                                             
         SR    RE,R0               GIVES NUM CHARS PRESENT FOR PERIOD           
         BZ    PERERR                                                           
         ST    RE,FULL                                                          
         CH    RE,=H'4'                                                         
         BH    PERERR                                                           
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PDSDPER(0),22(R4)                                                
         OC    PDSDPER,=CL4'    '                                               
*                                                                               
         CLI   0(R1),C'/'          TEST YEAR PRESENT                            
         BNE   VALO176                                                          
*                                                                               
         ZIC   R0,1(R4)                                                         
         S     R0,FULL                                                          
         BCTR  R0,0                ADJUST FOR /                                 
         LA    R1,1(R1)            POINT TO FIRST DIGIT OF YEAR                 
         ST    R1,FULL             SAVE ADDRESS                                 
         CH    R0,=H'2'                                                         
         BNE   PERERR                                                           
*                                                                               
VALO174  CLI   0(R1),C'0'                                                       
         BL    PERERR                                                           
         CLI   0(R1),C'9'                                                       
         BH    PERERR                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,VALO174                                                       
*                                                                               
         L     RE,FULL                                                          
         PACK  DUB,0(2,RE)                                                      
         CVB   R0,DUB                                                           
         STC   R0,PDSDYEAR                                                      
*                                                                               
* VALIDATE PERIOD NAME *                                                        
*                                                                               
VALO176  XC    KEY,KEY                                                          
         MVC   KEY+14(4),PDSDDSKA                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         L     R5,AIO                                                           
         MVI   ELCODE,EPNCODEQ     FIND PERIOD NAME ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   PERERR                                                           
*                                                                               
         USING EPNELEM,R5                                                       
         ZIC   R0,EPNLEN                                                        
         SH    R0,=H'2'                                                         
         SRDL  R0,32                                                            
         D     R0,=F'5'                                                         
         LR    R0,R1                                                            
         LA    R1,EPNDATA                                                       
*                                                                               
VALO178  CLC   PDSDPER,1(R1)                                                    
         BE    VALO180                                                          
         LA    R1,5(R1)                                                         
         BCT   R0,VALO178                                                       
         B     PERERR                                                           
*                                                                               
VALO180  MVI   PDSIDOPT,C'Y'                                                    
         CLI   PDOVSYS,8                                                        
         BNE   OPTEND                                                           
         CLI   OFFLINE,C'Y'                                                     
         BNE   VALO182                                                          
         ICM   RF,15,PDAUTL                                                     
         MVC   4(1,RF),PDSYSTEM                                                 
         B     OPTEND                                                           
*                                                                               
VALO182  GOTO1 SWITCH,DMCB,(PDSYSTEM,0),0                                       
         CLI   4(R1),0                                                          
         BZ    OPTEND                                                           
         DC    H'0'                                                             
*                                                                               
VALO184  CLC   12(4,R4),=C'BOOK'   BOOK FOR UPGRADE, WE TAKE 'BK' TOO           
         BNE   VALO188                                                          
*                                                                               
VALO186  XC    WORK,WORK                                                        
         MVC   WORK+5(1),1(R4)                                                  
         MVC   WORK+8(10),22(R4)                                                
         LA    R5,PDBKOPT                                                       
         GOTO1 BOOKVAL,DMCB,(PODBSRC,WORK),(1,(R5)),(C'S',SCANNER)              
         CLI   4(R1),1                                                          
         BNE   BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
VALO188  CLC   12(3,R4),=C'PUT'    PUT FOR UPGRADE                              
         BE    VALO190                                                          
         CLC   12(3,R4),=C'RTG'    RTG FOR UPGRADE                              
         BE    VALO190                                                          
         CLC   12(3,R4),=C'SHR'    RTG FOR UPGRADE                              
         BNE   VALO192                                                          
*                                                                               
VALO190  MVI   PDPUTOPT,C'N'                                                    
         CLC   8(4,R4),=F'1'                                                    
         BE    OPTEND                                                           
         MVI   PDPUTOPT,C'Y'                                                    
         CLC   8(4,R4),=F'2'                                                    
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
VALO192  CLC   12(2,R4),=C'UP..'   UPGRADE                                      
         BNE   VALO198                                                          
         L     RE,APODBKL                                                       
*                                                                               
VALO193  CLC   1(3,RE),=C'PAV'     UPGRADE INVALID FOR PAV                      
         BE    BADOPT7                                                          
         LA    RE,PODBLNQ(RE)                                                   
         CLI   0(RE),X'FF'                                                      
         BNE   VALO193                                                          
*                                                                               
         MVI   PDDPOPT,C'Y'        FORCE ON DAYPART OPTION                      
         MVI   PDUPFILE,C'T'       DEFAULT IS TP                                
         CLI   14(R4),C' '                                                      
         BE    *+10                                                             
         MVC   PDUPFILE,14(R4)                                                  
         MVC   PDUPOPT(3),22(R4)                                                
         CLC   PDUPOPT(3),=C'SID'  OPTION TO COME FROM SID                      
         BNE   VALO194                                                          
         MVI   PDSUPOPT,C'Y'                                                    
         MVI   PDSIDOPT,C'Y'                                                    
         B     VALO196                                                          
*                                                                               
VALO194  XC    WORK,WORK                                                        
         MVC   WORK+5(1),1(R4)                                                  
         MVC   WORK+8(20),22(R4)                                                
         GOTO1 UPVAL,DMCB,WORK,PDUPOPT,(C'/',ACOMFACS)                          
         CLI   0(R1),0                                                          
         BE    BADOPT                                                           
*                                                                               
VALO196  OC    PDAESTBK,PDAESTBK     MUST BE ESTIMATED BOOK                     
         BNZ   OPTEND                                                           
         B     BADEBK                                                           
*                                                                               
VALO198  CLC   =C'NOIUN',12(R4)    TURN IUN CALCULATION OFF                     
         BNE   VALO200                                                          
         MVI   PDIUNOPT,C'N'                                                    
         B     OPTEND                                                           
*                                                                               
VALO200  CLC   12(4,R4),=C'DETAIL'   SUPPRESS/ACTIVATE DETAIL LINES             
         BNE   VALO202                FOR SPOT/REP LOOKUPS                      
         MVC   PDDETOPT,22(R4)                                                  
         CLI   22(R4),C'N'                                                      
         BE    OPTEND                                                           
         CLI   22(R4),C'Y'                                                      
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
VALO202  CLC   12(3,R4),=C'NHTI'      OPTIONAL HISPANIC HUT SURVEY              
         BNE   VALO204                FOR GET HUT(NET ONLY)                     
         MVI   PDHUTTYP,C'H'                                                    
         B     OPTEND                                                           
*                                                                               
VALO204  CLC   12(5,R4),=C'-SPOR'                                               
         BNE   VALO206                                                          
         MVI   PDSPTOPT,C'N'                                                    
         B     OPTEND                                                           
*                                                                               
VALO206  CLC   12(5,R4),=C'SPORT'                                               
         BNE   VALO208                                                          
         MVI   PDSPTOPT,C'Y'                                                    
         B     OPTEND                                                           
*                                                                               
VALO208  CLC   =C'TELECAST',12(R4)  CABLE TELECAST OPTION                       
         BE    *+10                                                             
         CLC   =C'TCAST',12(R4)                                                 
         BNE   VALO212                                                          
         CLI   22(R4),C'Y'                                                      
         BE    VALO210                                                          
         B     BADOPT                                                           
*                                                                               
VALO210  MVI   PDBSTOPT,C'A'       SET OPTION FOR DBBEST                        
         B     OPTEND                                                           
*                                                                               
VALO212  CLC   =C'DMA',12(R4)      DMA RPS CALC OPTION                          
         BNE   VALO216                                                          
         CLI   22(R4),C'R'                                                      
         BE    VALO214                                                          
         CLI   22(R4),C'I'                                                      
         BE    VALO214                                                          
         B     BADOPT                                                           
*                                                                               
VALO214  MVC   DMAOPT,22(R4)       SET OPTION FOR RPS CALC                      
         B     OPTEND                                                           
*                                                                               
VALO216  CLC   =C'INSEQ',12(R4)                                                 
         BNE   VALO222                                                          
         CLI   22(R4),C'S'                                                      
         BE    VALO218                                                          
         CLI   22(R4),C'B'                                                      
         BE    VALO220                                                          
         B     BADOPT                                                           
*                                                                               
VALO218  OI    PDINSEQ,PDSEQSTA    SET SEQUENCING OPTIONS                       
         B     OPTEND                                                           
*                                                                               
VALO220  OI    PDINSEQ,PDSEQBK     SET SEQUENCING OPTIONS                       
         B     OPTEND                                                           
*                                                                               
VALO222  CLC   12(3,R4),=C'CALENDAR' SPECIAL CALENDAR ROUTINES                  
         BNE   VALO226                                                          
         CLC   22(3,R4),=C'NSI'                                                 
         BNE   VALO224                                                          
         MVI   PDCALOPT,PDCALNSI                                                
         B     OPTEND                                                           
*                                                                               
VALO224  CLC   22(3,R4),=C'NTI'                                                 
         BNE   BADOPT                                                           
         MVI   PDCALOPT,PDCALNTI                                                
         B     OPTEND                                                           
*                                    AVERAGE BASE RTG/IMP/BOOK                  
VALO226  CLC   12(4,R4),=C'BASE'                                                
         BNE   VALO228                                                          
         CLI   PDMETHOD,1                                                       
         BH    BADOPT                                                           
         MVI   PDMETHOD,1                                                       
         L     RE,APODBKL                                                       
         CLC   1(3,RE),=C'NTI'     ONLY FOR NTI                                 
         BE    VAL0226A                                                         
         CLC   1(4,RE),=C'OOH '    ONLY FOR NTI                                 
         BE    VAL0226A                                                         
         CLC   1(3,RE),=C'REN'     OR RENTRAK                                   
         BE    VAL0226A                                                         
         CLC   1(5,RE),=C'ACM  '   OR ACM                                       
         BE    VAL0226A                                                         
         CLC   1(5,RE),=C'OOHC '   OR ACM                                       
         BE    VAL0226A                                                         
         CLC   1(3,RE),=C'NHW'     OR WEEKLY HISPANIC                           
         BE    VAL0226A                                                         
         CLC   1(3,RE),=C'HPW'     OR WEEKLY HISPANIC FROM NATIONAL SMP         
         BE    VAL0226A                                                         
         CLC   1(3,RE),=C'NAW'     OR WEEKLY NAD                                
         BE    VAL0226A                                                         
         CLC   1(3,RE),=C'NHT'     OR NHT                                       
         BE    VAL0226A                                                         
         CLC   1(3,RE),=C'HPM'     OR HPM                                       
         BE    VAL0226A                                                         
         CLC   1(3,RE),=C'NAD'     OR NAD                                       
         BE    VAL0226A                                                         
         CLC   1(3,RE),=C'CNA'     OR CABLE NAD/MVGO                            
         BE    VAL0226A                                                         
         B     BADOPT                                                           
VAL0226A CLC   22(3,R4),=C'BOO'                                                 
         BNE   *+12                                                             
         MVI   PDBASE,C'B'                                                      
         B     OPTEND                                                           
         CLC   22(3,R4),=C'IMP'                                                 
         BNE   BADOPT                                                           
         MVI   PDBASE,C'I'                                                      
         B     OPTEND                                                           
*                                                                               
VALO228  CLC   12(4,R4),=C'MASTER' MASTER STATION FOR DAYS/TIMES                
         BNE   VALO230                                                          
         MVC   PDINVSTA,22(R4)                                                  
         OC    PDINVSTA(4),=C'    '                                             
         B     OPTEND                                                           
*                                                                               
VALO230  CLC   =C'MRKGRP',12(R4)   IS IT NON-SID MARKET GROUP?                  
         BNE   VALO236             NO, ERROR                                    
         OC    PDMGRP,PDMGRP       YES, ERROR IF ALREADY BEEN HERE              
         BNZ   BADOPT2                                                          
*        TM    23(R4),C'0'         2ND CHARACTER MUST BE NUMERIC                
*        BNO   BADOPT                                                           
*        CLI   22(R4),C'G'         1ST MUST BE G-Z                              
*        BL    BADOPT                                                           
*        CLI   22(R4),C'Z'                                                      
*        BH    BADOPT                                                           
         CLI   23(R4),C'0'         IS SECOND CHAR NUMERIC?                      
         BL    VALO231A            ALPHA. MRKGRP=AANNNN                         
         CLI   23(R4),C'9'                                                      
         BH    BADOPT                                                           
         B     VALO231B            NUMERIC. MRKGRP=ANNNN                        
*                                                                               
VALO231A TM    24(R4),C'0'         3RD CHARACTER MUST BE NUMERIC                
         BNO   BADOPT                                                           
         MVC   HALF,22(R4)         ALPHA + ALPHA                                
         LA    R5,24(R4)           START OF NUMERIC                             
         LHI   R0,2                LENGTH OF ALPHA                              
         B     VALO232                                                          
VALO231B MVC   HALF(1),22(R4)      2ND CHAR IS NUMERIC                          
         MVI   HALF+1,C' '         ALPHA + SPACE                                
         LA    R5,23(R4)           START OF NUMERIC                             
         LHI   R0,1                LENGTH OF ALPHA                              
*                                                                               
VALO232  L     RF,=A(SPMGRTAB)     LOOKUP ALPHA                                 
         A     RF,PGNR                                                          
         LHI   RE,(SPMGRTBX-SPMGRTAB)/L'SPMGRTAB                                
         CLC   HALF,0(RF)                                                       
         BE    *+16                                                             
         LA    RF,L'SPMGRTAB(RF)                                                
         BCT   RE,*-14                                                          
         B     BADOPT                                                           
*                               ALPHA IS VALID. NOW VALIDATE NUMERIC            
         IC    RE,1(R4)            GET LENGTH                                   
         SR    RE,R0               LESS LENGTH OF ALPHA                         
         LTR   RE,RE               RE=LENGTH OF NUMERIC                         
         BZ    VALO234                                                          
         CHI   RE,L'PDMGRP-1                                                    
         BH    BADOPT              NUMERIC TOO LONG                             
*                                                                               
         LR    R1,R5               POINTER TO FIRST NUMERIC                     
         TM    0(R1),C'0'          MUST BE NUMERIC                              
         BNO   BADOPT                                                           
         LA    R1,1(R1)                                                         
         BCT   RE,*-12                                                          
*                                                                               
VALO234  MVC   PDMGRP(1),2(RF)     INTERNAL CODE FROM TABLE                     
         MVC   PDMGRP+1(L'PDMGRP-1),0(R5)   NUMERIC PART                        
*                                                                               
* READ MGRDEF RECORD *                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),PDBAGYMD                                                
         MVC   KEY+8(1),PDMGRP     MGRPID                                       
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BNE   BADOPT                                                           
         CLC   KEY(13),KEYSAVE     TEST KEY FOUND                               
         BNE   BADOPT                                                           
*                                                                               
         B     OPTEND                                                           
*                                                                               
VALO236  CLC   12(5,R4),=C'AFFILIATE'   LOOK FOR AFFILATE                       
         BNE   VALO238                                                          
         CLI   1(R4),0                                                          
         BE    BADOPT                                                           
*                                                                               
         L     R5,APDUFFLT         CHECK UAFFILIATE                             
         CLI   0(R5),X'FF'                                                      
         BNE   BADOPT4             CAN'T HAVE BOTH                              
*                                                                               
         L     R5,APDAFFLT         ADDRESS AFFILATE LIST                        
         LR    RF,R5                                                            
         LA    RF,L'PDAFFLT(RF)    SAVE END ADDRESS                             
         B     VALO240                                                          
*                                                                               
VALO238  CLC   12(6,R4),=C'UAFFILIATE'                                          
         BNE   VALO246                                                          
         CLI   1(R4),0                                                          
         BE    BADOPT                                                           
*                                                                               
         L     R5,APDAFFLT         CHECK AFFILIATE                              
         CLI   0(R5),X'FF'                                                      
         BNE   BADOPT4             CAN'T HAVE BOTH                              
*                                                                               
         L     R5,APDUFFLT         ADDRESS UAFFILIATE LIST                      
         LR    RF,R5                                                            
         LA    RF,L'PDUFFLT(RF)    SAVE END ADDRESS                             
*                                                                               
VALO240  MVC   0(1,R4),1(R4)       SHIFT DATA FIRST TIME IN                     
         MVC   2(1,R4),3(R4)                                                    
         MVC   4(4,R4),8(R4)                                                    
         MVC   12(10,R4),22(R4)                                                 
*                                                                               
VALO242  MVC   0(5,R5),12(R4)      MOVE ENTRY TO TABLE                          
         LA    R5,5(R5)            GET NEXT SPOT IN LIST                        
         MVI   0(R5),X'FF'         MARK IN CASE LAST                            
*                                                                               
         CR    R5,RF                                                            
         BNL   BADOPT3             ERROR IF PAST END                            
*                                                                               
         ZIC   RE,OPTSCNLN                                                      
         AR    R4,RE                                                            
         AI    FIELDERR,1                                                       
         ZIC   RE,NUMLINES                                                      
         BCTR  RE,0                                                             
         STC   RE,NUMLINES                                                      
         LTR   RE,RE               ANY FIELDS LEFT?                             
         BZ    VALO244             NO, BACK IT UP                               
         CLI   1(R4),0             IS THERE A SECOND FIELD?                     
         BE    VALO242             NO, WE'RE STILL IN AFFL                      
*                                                                               
VALO244  ZIC   RE,NUMLINES                                                      
         LA    RE,1(RE)                                                         
         STC   RE,NUMLINES                                                      
         ZIC   RE,OPTSCNLN                                                      
         SR    R4,RE                                                            
         B     OPTEND                                                           
*                                                                               
VALO246  CLC   12(6,R4),=C'GSTACK'   GENDER STACKING OPTION                     
         BNE   VALO248                                                          
         CLI   PDSTACK,X'FF'       USING DEMO STACK?                            
         BE    BADOPT5             YES, ERROR                                   
         GOTO1 =A(OVFLRTN),DMCB,(7,DUB),(RC),RR=PGNR VALGSTCK                   
         BNE   BADOPT                                                           
         MVI   PDGSTACK,X'FF'      INDICATE TYPE OF STACKING                    
         BAS   RE,ADDGEN                                                        
         BE    OPTEND              OK                                           
         L     R1,ATWA                                                          
         USING T325FFD,R1                                                       
         LA    R2,SPLDEMH          ERROR, POINT TO DEMO FIELD                   
         B     BADDEM                                                           
*                                                                               
VALO248  CLC   12(6,R4),=C'PRGGRP'   PROGRAM GROUP OPTION                       
         BNE   VALO250                                                          
         CLI   1(R4),0                                                          
         BE    BADOPT                                                           
         L     RE,APDNTIFT                                                      
         CLI   0(RE),0             PROGS OPTION IS CONFLICTING                  
         BNE   BADOPT6                                                          
         GOTO1 =A(OVFLRTN),DMCB,(8,DUB),(RC),RR=PGNR VALPRGRP                   
         BNE   BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
VALO250  CLC   12(6,R4),=C'METHOD'   METHODOLOGY                                
*        BNE   BADOPT                                                           
         BNE   VALO255                                                          
         CLI   PDMETHOD,0                                                       
         BNE   BADOPT                                                           
         CLC   22(3,R4),=C'NET'                                                 
         BNE   *+12                                                             
         MVI   PDMETHOD,3                                                       
         B     OPTEND                                                           
*                                                                               
         L     RE,APODBKL                                                       
         CLC   1(3,RE),=C'NTI'     ONLY FOR NTI                                 
         BE    VALO250A                                                         
         CLC   1(4,RE),=C'OOH '    ONLY FOR NTI                                 
         BE    VALO250A                                                         
         CLC   1(3,RE),=C'REN'     OR RENTRAK                                   
         BE    VALO250A                                                         
         CLC   1(3,RE),=C'ACM'     OR COMMERCIAL AVERAGE / ACM WB1              
         BE    VALO250A                                                         
         CLC   1(4,RE),=C'OOHC'    OR COMMERCIAL AVERAGE / ACM WB1              
         BE    VALO250A                                                         
         CLC   1(3,RE),=C'MXM'     OR MINUTE BY MINUTE                          
         BE    VALO250A                                                         
         CLC   1(3,RE),=C'WB1'     OR COMMERCIAL AVERAGE / ACM WB1              
         BE    VALO250A                                                         
         CLC   1(3,RE),=C'NHW'     OR HISPANIC WEEKLY                           
         BE    VALO250A                                                         
         CLC   1(3,RE),=C'HPW'     OR HISPANIC WEEKLY FROM NATIONAL SMP         
         BE    VALO250A                                                         
         CLC   1(3,RE),=C'NAW'     OR NAD WEEKLY                                
         BE    VALO250A                                                         
         CLC   1(3,RE),=C'NHT'     OR NHT                                       
         BE    VALO250A                                                         
         CLC   1(3,RE),=C'HPM'     OR NHT FROM NPM                              
         BE    VALO250A                                                         
         CLC   1(3,RE),=C'NAD'     OR NAD                                       
         BE    VALO250A                                                         
         CLC   1(3,RE),=C'CNA'     OR CABLE NAD/MVGO                            
         BE    VALO250A                                                         
*                                                                               
         CLC   =C'NSI',1(RE)       TP                                           
         BE    VALO250B                                                         
         CLC   =C'TP',1(RE)        TP                                           
         BE    VALO250B                                                         
         CLC   =C'OTP',1(RE)       OVERNIGHT TP                                 
         BE    VALO250B                                                         
         CLC   =C'PAV',1(RE)       PAV                                          
         BE    VALO250B                                                         
         CLC   =C'OPA',1(RE)       OVERNIGHT PAV                                
         BE    VALO250B                                                         
         CLC   =C'FUS',1(RE)       FUSION                                       
         BE    VALO250B                                                         
         CLC   =C'CCV',1(RE)       COUNTY COVERAGE                              
         BE    VALO250B                                                         
         CLC   =C'T4',1(RE)        T4                                           
         BE    VALO250B                                                         
         B     BADOPT                                                           
VALO250A DS    0C                                                               
         CLC   22(2,R4),=C'AE'     METHOD=AUDIENCE ESTIMATOR                    
         BNE   *+12                                                             
         MVI   PDMETHOD,6                                                       
         B     VALO25A2                                                         
         CLC   22(3,R4),=C'NTI'                                                 
         BE    VALO25A1                                                         
         CLC   22(4,R4),=C'OOH '                                                
         BE    VALO25A1                                                         
         CLC   22(3,R4),=C'SOU'                                                 
         BNE   BADOPT                                                           
VALO25A1 MVI   PDMETHOD,4                                                       
VALO25A2 MVI   PDDUROPT,C'M'       MINUTE WIEGHTING/ ST.TM. EXCLUSION           
         MVI   PDBASE,C'I'         BASE IS IMPRESSIONS                          
         CLI   MINDUR,0                                                         
         BNE   *+8                                                              
         MVI   MINDUR,6            INCLUDE PROGRAMS 6MIN OR OVER                
         CLC   1(3,RE),=C'CNA'     CABLE NAD AND MVGO                           
         BE    OPTEND                                                           
         CLC   =C'NAD-D',1(RE)     NAD DAYPART SUMMARIES                        
         BE    *+10                                                             
         CLC   =C'NAD-T',1(RE)     NAD PROGRAM TYPE SUMMARIES                   
         BE    *+10                                                             
         CLC   =C'NHT-D',1(RE)     NHT DAYPART SUMMARIES                        
         BE    *+10                                                             
         CLC   =C'HPM-D',1(RE)     NHT DAYPART SUMMARIES FROM NPM               
         BE    *+10                                                             
         CLC   =C'NHT-T',1(RE)     NHT PROGRAM TYPE SUMMARIES                   
         BE    *+10                                                             
         CLC   =C'HPM-T',1(RE)     NHT PROGRAM TYPE SUMMARIES FROM NPM          
         BE    *+8                 ALLOW ALL DAYS                               
         MVI   DAYSOPT,C'I'        USE INDIVIDUAL DAY                           
         MVI   VAROPT,C'Y'         USE VARS AND INDIV DAY                       
         MVI   PDBSTOPT,C'A'       USE TELECAST FOR CABLE                       
         B     OPTEND                                                           
*                                                                               
VALO250B DS    0C                  METHOD=SOURCE FOR SPOT/REP                   
         CLC   22(3,R4),=C'NSI'                                                 
         BE    *+10                                                             
         CLC   22(3,R4),=C'SOU'                                                 
         BNE   BADOPT                                                           
         MVI   PDMETHOD,5                                                       
         B     OPTEND                                                           
*                                                                               
VALO255  CLC   =C'CNAD',12(R4)                                                  
         BNE   VALO260                                                          
         CLC   =C'PROG',22(R4)                                                  
         BNE   BADOPT                                                           
         OI    OPTIND,OPTCNADP                                                  
         B     OPTEND                                                           
*                                                                               
VALO260  CLC   =C'CONTINUE',12(R4)   CONTINUATION REPORT                        
         BNE   VALO265             (FOR EXTRA DEMOS AND COLUMNS)                
         OI    OPTIND,OPTCONT                                                   
         OI    DOWNOPT,DOWNON      FORCE DOWNLOAD                               
         GOTO1 =A(OVFLRTN),DMCB,(11,DUB),(RC),RR=PGNR VALRP2                    
         BNE   BADOPT                                                           
         ST    R2,ACONTOPT         SAVE POSITION OF CONTINUE OPTION             
         MVC   SVFLDER,FIELDERR                                                 
         B     OPTEND                                                           
*                                                                               
VALO265  CLC   =C'SYSCODE',12(R4)  SYSCODE FILTER                               
         BNE   VALO270                                                          
         CLI   1(R4),0                                                          
         BE    BADOPT                                                           
         OI    PDFLAG1,PDF1SYSC    SYSCODE REQUESTED                            
         OI    PDFLAG1,PDF1SCPV    SYSCODE LIST PROVIDED BY USER                
         GOTO1 =A(OVFLRTN),DMCB,(14,DUB),(RC),RR=PGNR VALSYSCD                  
         BNE   BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
VALO270  CLC   =C'VTYP',12(R4)     VIEWING TYPES                                
         BNE   VALO290                                                          
         GOTO1 =A(OVFLRTN),DMCB,(15,DUB),(RC),RR=PGNR VALVIEWT                  
         BNE   BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
VALO290  CLC   12(3,R4),=C'PIVAE'  OPTION FOR AE RECORDS                        
         BNE   VALO300                                                          
         CLI   22(R4),PIVAEY                                                    
         BE    *+8                                                              
         CLI   22(R4),PIVAEN                                                    
         BNE   BADOPT                                                           
         MVC   PIVAEOPT,22(R4)                                                  
         B     OPTEND                                                           
*                                                                               
VALO300  CLC   =C'ACMWGT',12(R4)   WGT=M FOR MINUTE WEIGHTING                   
         BNE   VALO310                                                          
         CLI   22(R4),C'M'                                                      
         BNE   BADOPT                                                           
         OI    OPTFLAG1,ACMMWTQ                                                 
         B     OPTEND                                                           
*                                                                               
VALO310  CLC   =C'COMM',12(R4)     COMMERCIAL MINUTE DEFINITION                 
         BNE   VALO320                                                          
         ZIC   R1,1(R4)            L'SECOND HALF                                
         ST    R1,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,(0,22(R4))                                          
         CLI   DMCB,X'FF'                                                       
         BE    BADOPT                                                           
         ICM   R1,15,DMCB+4                                                     
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         CHI   R1,59                                                            
         BH    BADOPT                                                           
         STC   R1,MINCOMM          MINIMUM COMMERCIAL DURATION                  
         B     OPTEND                                                           
*                                                                               
VALO320  CLC   =C'CMXMWGT',12(R4)                                               
         BNE   VALO340                                                          
*                                                                               
         CLI   OPTMINTY,0          MISSING MINTYP OPTION                        
         BE    MISMTYER                                                         
*                                                                               
         CLI   1(R4),2                                                          
         BNE   BADOPT                                                           
         CLC   22(2,R4),=C'CS'     WEIGHT BY COMMERCIAL SECONDS                 
         BNE   VALO322                                                          
         MVI   OPTSCWGT,OPTSWS                                                  
         B     OPTEND                                                           
VALO322  CLC   22(2,R4),=C'TS'     WEIGHT BY TOTAL COMM SECONDS                 
         BNE   VALO324                                                          
         MVI   OPTSCWGT,OPTSWTS                                                 
         B     OPTEND                                                           
VALO324  CLC   22(2,R4),=C'PD'     WEIGHT BY POD SECONDS                        
         BNE   BADOPT                                                           
         MVI   OPTSCWGT,OPTSWPD                                                 
         B     OPTEND                                                           
*                                                                               
VALO330  GOTO1 CASHVAL,DMCB,(0,22(R4))  'MINCOM' MIN COMMERCIAL SECONDS         
         CLI   DMCB,X'FF'                                                       
         BE    BADOPT                                                           
         ICM   R1,15,DMCB+4                                                     
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         CHI   R1,59                                                            
         BH    BADOPT                                                           
         STC   R1,OPTMINCM         MINIMUM COMMERCIAL SECONDS                   
         B     OPTEND                                                           
*                                                                               
VALO340  CLC   =C'SVT',12(R4)      SOURCE+VIEWING TYPE COMBOS                   
         BNE   VALO350                                                          
         GOTO1 =A(OVFLRTN),DMCB,(17,DUB),(RC),RR=PGNR VALSVT                    
         BNE   BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
VALO350  CLC   =C'OS',12(R4)       ORDERED SUSTAINER OPTION                     
         BNE   VALO360                                                          
         CLI   22(R4),C'N'                                                      
         BE    OPTEND                                                           
         CLI   22(R4),C'Y'                                                      
         BNE   BADOPT                                                           
         OI    OPTFLAG1,INCLOSQ                                                 
         B     OPTEND                                                           
*                                                                               
VALO360  DS    0X                                                               
         B     BADOPT                                                           
*                                                                               
OPTEND   ZIC   RE,OPTSCNLN                                                      
         AR    R4,RE                                                            
         AI    FIELDERR,1                                                       
         ZIC   RE,NUMLINES                                                      
         BCTR  RE,0                                                             
         STC   RE,NUMLINES                                                      
         LTR   RE,RE                                                            
         BNZ   VALO10                                                           
*                                                                               
OPTEND5  CLI   DEMOPT,DEMOTGT      TEST DEMO OPTION SET TO TGT/SEC              
         BE    *+12                                                             
         CLI   DEMOPT,DEMOSEC                                                   
         BNE   OPTX                                                             
         OC    PDQDEMOS,PDQDEMOS   YES - TEST DEMO MENU                         
         BZ    BADMEN                                                           
*                                                                               
* MUST HAVE A STATION BY NOW                                                    
OPTX     L     RE,APODNET                                                       
         CLI   0(RE),0             DO WE HAVE A STATION?                        
         BNE   OPTX2               YES                                          
         L     RE,APODMKTL                                                      
         CLI   0(RE),X'FF'         NO, DO WE HAVE A MARKET LIST?                
         BNE   OPTX2               YES                                          
         OC    PDMGRP,PDMGRP       NO, DO WE HAVE A MARKET GROUP?               
         BNZ   OPTX2               YES                                          
         CLI   SCNDTIME,C'Y'       NO, IS THIS 2ND PASS?                        
         BE    BADSTA              YES, ERROR                                   
*                                                                               
OPTX2    OC    PDAESTBK,PDAESTBK   ESTIMATE BOOK USED?                          
         BZ    OPTX4                                                            
         CLI   SCNDTIME,C'Y'       LET US VALIDATE BOTH OPTIONS                 
         BNE   OPTX4                                                            
         OC    PDUPOPT,PDUPOPT     EST BOOKS NEED UPGRADE                       
         BZ    BADUPG                                                           
*                                                                               
OPTX4    L     R5,APODMKTL                                                      
         CLI   0(R5),X'FF'         USING MARKET LIST?                           
         BE    OPTXIT              NO                                           
         OC    PDMGRP,PDMGRP       YES, USING MARKET GROUP ALSO?                
         BNZ   OPTCON2             YES, ERROR                                   
*                                                                               
OPTXIT   XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL VALDEMO ROUTINE                                   *         
***********************************************************************         
*                                                                               
*        CREATE NEW LIST OF DEMOS WITH STACKED GENDER                           
*                                                                               
ADDGEN   NTR1                                                                   
         XCEFL BLOCK,480                                                        
*                                                                               
         L     R5,PDADEMTB                                                      
         USING PDDMBUFF,R5                                                      
         LA    RF,DBLOCKA                                                       
         ST    RF,DMCB+8                                                        
         USING DBLOCKD,RF                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'PAV'                                                   
         MVC   DBSELMED,PODBMED                                                 
         MVI   DBSELMED,C'N'       DEFAULT NETWORK                              
*                                                                               
         CLC   =C'OPI',PODBEXT     EDIT OPI MODIFIERS                           
         BNE   *+14                                                             
         MVC   DBFILE,PODBEXT                                                   
         MVI   DBSELMED,X'00'                                                   
*                                                                               
         CLI   PDOVSYS,3           NET USES 4 BYTE DEMOS                        
         BNE   *+8                                                              
         MVI   DBDEMTYP,C'4'                                                    
         CLI   PDOVSYS,2                                                        
         BNE   ADDG02                                                           
         MVI   DBSELMED,C'T'                                                    
         MVI   DMCB+8,C'S'                                                      
         CLC   =C'CCV',PODBEXT     COUNTY COVER HAS OWN TABLE                   
         BE    ADDG01                                                           
         CLI   PODBMED,C'C'                                                     
         BNE   ADDG02                                                           
         MVI   DBSELMED,C'C'                                                    
         MVC   DBFILE,=C'TP '                                                   
         B     ADDG02                                                           
*                                                                               
ADDG01   MVC   DBFILE,=C'CUN'      COUNTY COVERAGE                              
         MVI   DBSELMED,C'U'                                                    
*                                                                               
ADDG02   GOTO1 DEMOCON,DMCB,(ACTUAL,PODDEMWK),(5,BLOCK),,0                      
         DROP  R5,RF                                                            
*                                                                               
         L     RE,APDSTSCN         CLEAR SCREEN BUILD AREA                      
         LA    RF,L'PDSTSCN                                                     
         XCEFL                                                                  
*                                                                               
         L     RE,APDSTDEM         CLEAR DEMOS SAVE AREA                        
         LA    RF,L'PDSTDEM                                                     
         XCEFL                                                                  
*                                                                               
         ZIC   R0,ACTUAL           NUMBER OF ENTRIES                            
*                                                                               
         L     RE,APDSTDEM         SAVE ADDRESS OF DEMO OUTPUT                  
         ST    RE,STDMPTR                                                       
*                                                                               
         L     R3,APDSTSCN         BUILD NEW SCREEN HERE                        
         LA    R3,8(R3)                                                         
*                                                                               
         LA    R4,BLOCK            OUTPUT FROM DEMCON                           
         MVI   FIELDERR,0                                                       
         SR    RF,RF               CLEAR DEMO COUNTER                           
*                                                                               
ADDG04   LA    R2,PDSTADEF         GET STACK OF GENDERS                         
         AI    FIELDERR,1                                                       
*                                                                               
ADDG06   CLI   0(R2),X'FF'         END OF LIST?                                 
         BE    ADDG08              YES, GET NEXT BLOCK ENTRY                    
         LA    R5,MODTAB           GET MODIFIER TABLE                           
*                                                                               
ADDG07   CLI   0(R5),X'FF'         END OF MODIFIER TABLE                        
         BE    ADDG07A                                                          
         CLC   0(5,R5),5(R4)       FIND ENTRY IN TABLE                          
         BE    *+12                                                             
         LA    R5,1(R5)                                                         
         B     ADDG07                                                           
         MVC   0(1,R3),5(R5)       FOUND IT, MOVE IN CODE                       
         CLI   0(R3),C' '          DID WE MOVE ANYTHING?                        
         BNH   *+8                 NO                                           
         LA    R3,1(R3)            BUMP                                         
*                                                                               
ADDG07A  MVC   0(5,R3),0(R4)       MOVE FROM BLOCK TO PDSTSCN                   
         MVC   0(1,R3),1(R2)       REPLACE GENDER                               
*                                                                               
         CLI   5(R5),C'V'          V=VPH                                        
         BNE   ADDG07B                                                          
         CLI   1(R2),C'W'          W=WOMEN                                      
         BNE   ADDG07B             BUT VW=VIEWERS.                              
         MVC   DUB(4),1(R3)                                                     
         MVC   2(4,R3),DUB         TO AVOID CONFUSION REPLACE WITH VWM          
         MVI   1(R3),C'M'                                                       
         LA    R1,6-1(R3)                                                       
         B     *+8                                                              
*                                                                               
ADDG07B  LA    R1,5-1(R3)          GET TO LAST BYTE                             
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         SR    R1,R3               GET LENGTH OF DATA                           
         LA    R1,1(R1)                                                         
         LA    R3,0(R1,R3)         BUMP TO END                                  
         MVI   0(R3),C','          PUT IN A COMMA                               
         LA    R3,1(R3)            NEXT IN PDSTSCN                              
         LA    R2,2(R2)            NEXT IN PDSTADEF                             
         LA    RF,1(RF)            INCREASE DEMO COUNT                          
*                                                                               
         CHI   RF,24               24 IS MAX DEMOVAL CAN HANDLE                 
         BNE   ADDG06                                                           
*                                                                               
         ST    R2,SAVER2           SAVE PLACE IN PDSTADEF                       
         ST    R4,SAVER4           SAVE PLACE IN BLOCK                          
         ST    R0,SAVER0           SAVE REMAINING DEMOS                         
         B     ADDG09                                                           
*                                                                               
ADDG08   LA    R4,10(R4)                                                        
         BCTR  R0,0                                                             
         LTR   R0,R0               ANY LINES LEFT?                              
         BNZ   ADDG04              YES                                          
*                                                                               
ADDG09   BCTR  R3,0                NO, BACK UP PDSTSCN                          
         MVI   0(R3),X'00'         REMOVE THE LAST COMMA                        
*                                                                               
         L     R2,APDSTSCN                                                      
         LA    R2,8(R2)                                                         
         SR    R3,R2               LENGTH OF STRING                             
*                                                                               
         AHI   R2,-8               BACK UP TO START                             
         STC   R3,5(R2)            FUDGE LENGTH OF DATA                         
         AHI   R3,8                                                             
         STC   R3,0(R2)            AND LENGTH OF ENTRY                          
*                                                                               
         STC   RF,NFLD#                                                         
*                                                                               
         L     R5,PDADEMTB                                                      
         USING PDDMBUFF,R5         DEMO BUFFER                                  
         LA    RF,DBLOCKA                                                       
         USING DBLOCKD,RF                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         CLI   PODBMED,C'C'                                                     
         BNE   *+8                                                              
         MVI   DBSELMED,C'C'                                                    
         CLI   PDOVSYS,3           FOR NET                                      
         BNE   *+8                                                              
         MVI   DBSELMED,C'N'       SET SELMED TO N                              
         MVC   DBFILE,PODBFIL                                                   
*                                                                               
         ST    RF,DMCB+8                                                        
         CLI   PDOVSYS,2                                                        
         BNE   *+8                                                              
         MVI   DMCB+8,C'S'        SPOTPAK CALL                                  
         CLI   PDOVSYS,3           ONLY NET CAN USE 4 BYTE DEMOS                
         BNE   *+8                                                              
         MVI   DBDEMTYP,C'4'                                                    
*                                                                               
         GOTO1 DEMOVAL,DMCB,(NFLDS,(R2)),STDMPTR,,0,,APDNTDMS                   
         CLI   4(R1),0                                                          
         BE    ADDGNEX                                                          
*                                                                               
         CLI   NFLD#,24            DID WE HAVE 24 DEMOS?                        
         BNE   ADDG09A             NO, DONE                                     
*                                                                               
         ZIC   RF,4(R1)            SAVE NUMBER OF DEMOS                         
         LA    R1,4                                                             
         CLI   PDOVSYS,3           IS THIS NET?                                 
         BE    *+8                 YES                                          
         LA    R1,3                NO                                           
         MR    RE,R1               NUMBER OF DEMOS X LENGTH                     
*                                                                               
         L     RE,STDMPTR          GET CURRENT OUT ADDRESS                      
         LA    RE,0(RE,RF)         BUMP TO END                                  
         ST    RE,STDMPTR          SAVE FOR NEXT TIME                           
*                                                                               
         L     R2,SAVER2           RESTORE PLACE IN PDSTADEF                    
         L     R4,SAVER4           RESTORE PLACE IN BLOCK                       
         L     R0,SAVER0           RESTORE DEMO COUNT                           
*                                                                               
         L     RE,APDSTSCN         CLEAR SCREEN BUILD AREA AGAIN                
         LA    RF,L'PDSTSCN                                                     
         XCEFL                                                                  
*                                                                               
         L     R3,APDSTSCN                                                      
         LA    R3,8(R3)                                                         
*                                                                               
         SR    RF,RF               CLEAR COUNTER                                
         B     ADDG06                                                           
*                                                                               
ADDG09A  L     R5,PDADEMTB                                                      
         USING PDDMBUFF,R5                                                      
         LA    R4,PODDEMWK         DEMO WORK AREA                               
         LA    R3,PODDEMO          DEMO HOLD AREA                               
         LA    RF,PODDEWPL         PERSONAL LANGUAGE WORK AREA                  
         LA    R2,PODDEMPL         PERSONAL LANGUAGE HOLD AREA                  
         LA    R1,PDSTADEF         STACK DEFINITION                             
         L     RE,APDSTDEM         STACK DEMOS                                  
         MVI   PODDMNUM,0          TOTAL NUMBER OF DEMOS                        
         MVI   PODDMENT,0          NUMBER OF ENTRIES IN EACH DEMO GROUP         
*                                                                               
ADDG10   CLI   0(R4),X'FF'         ANY MORE DEMOS?                              
         BE    ADDGEQX             NO                                           
         MVI   PODDMENT,0                                                       
         CLI   PDOVSYS,3           IS IT NET?                                   
         BNE   ADDG12              NO                                           
         MVC   0(4,R3),0(R4)       YES, MOVE 4 BYTES TO PODDEMO                 
         LA    R3,4(R3)                                                         
         B     ADDG14                                                           
*                                                                               
ADDG12   MVC   0(3,R3),0(R4)       NOT NET, MOVE 3 BYTES                        
         LA    R3,3(R3)                                                         
*                                                                               
ADDG14   MVC   0(PODPLLQ,R2),0(RF)  MOVE PERSONAL LANGUAGE ATTRIBUTE            
         LA    R2,PODPLLQ(R2)                                                   
*                                                                               
         ZIC   R0,PODDMNUM         ADD 1 TO COUNTERS                            
         AHI   R0,1                                                             
         STC   R0,PODDMNUM                                                      
         ZIC   R0,PODDMENT                                                      
         AHI   R0,1                                                             
         STC   R0,PODDMENT                                                      
*                                                                               
ADDG16   CLI   1(R1),0             END OF STACK DEFINITIONS                     
         BNE   ADDG18              NO                                           
         AH    R4,LNDEMCOD         YES, GET TO NEXT DEMO                        
         LA    RF,PODPLLQ(RF)      NEXT PERSONAL ATTRIBUTE IN WORK AREA         
         LA    R1,PDSTADEF         GET TO TOP OF STACK                          
         B     ADDG10                                                           
*                                                                               
ADDG18   MVC   0(3,R3),0(RE)                                                    
         CLI   PDOVSYS,3           NET SYSTEM?                                  
         BNE   *+10                                                             
         MVC   0(4,R3),0(RE)       USE 4 BYTE DEMOS                             
*                                                                               
         CLI   0(R3),0                                                          
         BNE   *+8                                                              
         MVI   0(R3),X'01'                                                      
*                                                                               
         LA    R3,3(R3)            PODDEMO                                      
         LA    RE,3(RE)            PDSTSCN                                      
*                                                                               
         CLI   PDOVSYS,3           NET SYSTEM?                                  
         BNE   *+12                                                             
         LA    R3,1(R3)            PODDEMO                                      
         LA    RE,1(RE)            PDSTSCN                                      
*                                                                               
         MVC   0(PODPLLQ,R2),0(RF)  MOVE PERSONAL LANGUAGE ATTRIBUTE            
         LA    R2,PODPLLQ(R2)                                                   
*                                                                               
         LA    R1,2(R1)            PDSTADEF                                     
         ZIC   R0,PODDMNUM         COUNTER                                      
         AHI   R0,1                                                             
         STC   R0,PODDMNUM                                                      
         ZIC   R0,PODDMENT                                                      
         AHI   R0,1                                                             
         STC   R0,PODDMENT                                                      
         B     ADDG16                                                           
*                                                                               
ADDGEQX  MVI   0(R3),X'FF'         END OF DEMO MARK                             
         CR    RB,RB                                                            
         B     ADDGX                                                            
*                                                                               
ADDGNEX  LTR   RB,RB                                                            
         B     ADDGX                                                            
*                                                                               
ADDGX    XIT1                                                                   
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
* CHECK IF UNIVERSE RECORD EXISTS                                               
*                                                                               
*                                                                               
CKUNIV   NTR1                                                                   
         LA    R2,BLOCK                                                         
         USING GUVD,R2             FILL BLOCK FOR GETNUN                        
         XC    GUVBLOCK,GUVBLOCK                                                
         MVC   GUVAREC,AIO                                                      
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT,X'31'                                                    
         MVI   ELEMENT+1,179                                                    
         MVI   ELEMENT+2,X'44'                                                  
         LA    R1,ELEMENT+3                                                     
         ST    R1,GUVAOUT                                                       
*                                                                               
         MVC   GUVCMFCS,ACOMFACS                                                
         MVC   GUVAGY,AGENCY                                                    
         L     R1,UNIVOPT          NEED CODE IN PWOS                            
         CVD   R1,DUB                                                           
         L     R1,DUB+4                                                         
         SRL   R1,4                                                             
         STH   R1,DUB                                                           
         MVC   GUVCODE,DUB                                                      
         XC    GUVDATE,GUVDATE                                                  
         GOTO1 GETNUN,DMCB,GUVBLOCK                                             
         CLI   GUVERROR,0          SET CONDITION CODE                           
CKUNIVX  B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
BADOPT   MVC   PERROR,=AL2(INVOPT)                                              
         B     OPTERR                                                           
*                                                                               
BADOPT2  MVC   PERROR,=AL2(DUPOPT)                                              
         B     OPTERR                                                           
*                                                                               
BADOPT3  MVC   PERROR,=AL2(TOOMANY)                                             
         B     OPTERR                                                           
*                                                                               
BADOPT4  MVC   PERROR,=AL2(AFFCON)                                              
         B     OPTERR                                                           
*                                                                               
BADOPT5  MVC   PERROR,=AL2(STKCON)                                              
         B     OPTERR                                                           
*                                                                               
BADOPT6  MVC   PERROR,=AL2(PRGCON)                                              
         B     OPTERR                                                           
*                                                                               
BADOPT7  MVC   PERROR,=AL2(PAVUPG)                                              
         B     OPTERR                                                           
*                                                                               
MINERR   MVC   PERROR,=AL2(PREMIN)                                              
         B     OPTERR                                                           
*                                                                               
BADPRGS  MVC   PERROR,=AL2(INVPRGS)                                             
         B     OPTERR                                                           
*                                                                               
BADSID   MVC   PERROR,=AL2(INVSID)                                              
         B     OPTERR                                                           
*                                                                               
PERERR   MVC   PERROR,=AL2(INVPER)                                              
         B     OPTERR                                                           
*                                                                               
BADEBK   MVC   PERROR,=AL2(INVEST)                                              
         B     OPTERR                                                           
*                                                                               
BADSYS   MVC   PERROR,=AL2(NOSIDS)                                              
         B     OPTERR                                                           
*                                                                               
BADUPG   MVC   PERROR,=AL2(NEEDUP)                                              
         B     OPTERR                                                           
*                                                                               
BADMEN   MVC   PERROR,=AL2(MENUOPT)                                             
         B     OPTERR                                                           
*                                                                               
BADSTA   MVC   PERROR,=AL2(MISSTA)                                              
         B     OPTERR                                                           
*                                                                               
MISMTYER MVC   PERROR,=AL2(MISSMTYP)                                            
         B     OPTERR                                                           
*                                                                               
OPTERR   MVI   PMSGTYPE,C'E'                                                    
         GOTO1 VPDSERR                                                          
         EJECT                                                                  
         LTORG                                                                  
***********************************************************************         
*              LIST OF VALID MODIFIERS                                *         
***********************************************************************         
*                                                                               
MODTAB   DS    0CL6                                                             
         DC    C'*VWR*',C'A'                                                    
         DC    C'*PUT)',C'B'                                                    
         DC    C'HIMPS',C'C'                                                    
         DC    C'*N/A*',C'D'                                                    
         DC    C'*HUT)',C'H'                                                    
         DC    C'(IMP)',C' '                                                    
         DC    C'(PUT)',C'P'                                                    
         DC    C'(TOT)',C'Q'                                                    
         DC    C'(RTG)',C'R'                                                    
         DC    C'(SHR)',C'S'                                                    
         DC    C'(TSH)',C'T'                                                    
         DC    C'(UNV)',C'U'                                                    
         DC    C'(VPH)',C'V'                                                    
         DC    C'*000)',C'X'                                                    
         DC    X'FF'                                                            
                                                                                
         EJECT                                                                  
***********************************************************************         
*         INITIALIZE DRIVER                                           *         
***********************************************************************         
*                                                                               
INTDRIV  NMOD1 0,**INTDRV**                                                     
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   DRIX                                                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         GOTO1 CALLOV,DMCB,X'B1000000',0,0  LOAD T325B1(GLOBAL STORAGE)         
         L     R4,DMCB                     FOR DRIVER                           
         ST    R4,AGLOBAL                                                       
*                                  THIS ALSO CONTAINS BUFFERS                   
         LA    R2,16(R4)                                                        
         L     R1,0(R2)                                                         
         LA    R2,4+8(R1,R2)                                                    
*                                                                               
         GOTO1 COVAIL,DMCB,C'GET',90000,90000    GET OFFLINE BUFFER             
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,PDAOFFBF         A(OFFLINE BUFFER)                            
         MVC   0(4,RE),8(R1)       L'BUFFER                                     
*                                                                               
         GOTO1 COVAIL,DMCB,C'GET',300000,300000  GET NAME POOL BUFFER           
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,8(R1)                                                         
         ST    RE,ANAMPOOL         A(OFFLINE BUFFER)                            
         ST    RF,LNAMPOOL                                                      
         LA    R1,8(RE)                                                         
         ST    R1,0(RE)            A(FIRST AVAILABLE ENTRY)                     
         ST    RF,4(RE)                                                         
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A9E'   T00A9E (SPOT CABLE STANS)            
         L     R2,DMCB                                                          
         ST    R2,VSCBLLST                                                      
*                                                                               
         USING GLOBALD,R4                                                       
         GOTO1 CALLOV,DMCB,0,X'D9000A3A'   LOAD T00A3A (DRIVER)                 
         L     R2,DMCB                                                          
         ST    R2,DRIVER                                                        
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A53'   LOAD T00A53 (RESEARCH DRIVE)         
         L     R2,DMCB                                                          
         ST    R2,GLASYSDR                                                      
         ST    RC,GLAWORKD                                                      
         MVC   GLAPROG,ADPGPROG                                                 
         MVI   GLTWORKD,GLTSPOOL                                                
         MVC   GLFHEADL,MYFIRSTH                                                
         MVC   GLSPACE,SPACOPT      PASS THRU SPACING OPT                       
         MVC   GLBOXOPT,BOXOPT                BOX OPTION                        
         MVC   GLLFTOPT,LEFTOPT           AND LEFT OPTION                       
*                                                                               
         TM    DOWNOPT,DOWNON       OPTION TO DOWNLOAD                          
         BZ    *+14                                                             
         OC    GLDOWNLD,DOWNOPT                                                 
         OI    GLDOWNLD,GLDLACTV                                                
*                                                                               
         TM    REQIND,REQITRN      TEST RECORD=TRANSMIT                         
         BZ    *+8                                                              
*        OI    GLDOWNLD,GLDLACTV+GLDLNOTR+GLDLALPH+GLDLNOHD+GLDLCUT             
         OI    GLDOWNLD,GLDLACTV+GLDLNOTR+GLDLALPH+GLDLNOHD                     
*                                                                               
         OC    GLDWNLD2(1),DOWNOPT2  OTHER DOWNLOAD OPTIONS                     
         CLI   DOWNCHAR,0                                                       
         BE    *+10                                                             
         MVC   GLDLCHAR,DOWNCHAR                                                
*                                                                               
         MVC   GLINDS2,WKINDS2                                                  
         CLI   TRACEOPT,C'Y'        OPTION TO TRACE                             
         BNE   *+8                                                              
         MVI   GLTRACE,C'Y'                                                     
*                                                                               
         XC    GLOPTS,GLOPTS       SET GLOBAL USER OPTIONS                      
*                                                                               
DRI4     B     DRI6                                                             
         EJECT                                                                  
* INITIALIZATION OF PRINT RELATED FIELDS                                        
*                                                                               
DRI6     L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         CLI   WIDEOPT,C'Y'                                                     
         BE    DRIWIDE                                                          
         MVC   BOXWIDTH,=F'132'                                                 
         MVI   BOXFONT,0                                                        
         LA    R1,H1               PRINT ADDRESSES FOR STANDARD                 
         ST    R1,AH1                                                           
         LA    R1,H3                                                            
         ST    R1,AH3                                                           
         LA    R1,H4                                                            
         ST    R1,AH4                                                           
         LA    R1,H5                                                            
         ST    R1,AH5                                                           
         LA    R1,P                                                             
         ST    R1,AP1                                                           
         MVC   PWIDTH,=F'132'                                                   
         LA    R1,REGSPECS                                                      
         ST    R1,SPECS                                                         
         CLI   NARROPT,C'Y'                                                     
         BNE   DRIX                                                             
         LA    R1,NARSPECS                                                      
         ST    R1,SPECS                                                         
         B     DRIX                                                             
*                                                                               
DRIWIDE  MVC   BOXWIDTH,=F'165'                                                 
         MVI   BOXFONT,1                                                        
         L     R4,BOXAWIDE                                                      
         USING WIDED,R4                                                         
         LA    R1,XHEAD1                                                        
         ST    R1,AH1                                                           
         LA    R1,XHEAD3                                                        
         ST    R1,AH3                                                           
         LA    R1,XHEAD4                                                        
         ST    R1,AH4                                                           
         LA    R1,XHEAD5                                                        
         ST    R1,AH5                                                           
         LA    R1,XP                                                            
         ST    R1,AP1                                                           
         MVC   PWIDTH,=F'198'                                                   
         LA    R1,WIDSPECS                                                      
         ST    R1,SPECS                                                         
         B     DRIX                                                             
*                                                                               
DRIX     XMOD1                                                                  
         EJECT                                                                  
REGSPECS DS    0C                                                               
         SSPEC H1,2,RUN            SPECS FOR REGULAR PRINTING                   
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,REPORT                                                     
         SSPEC H4,110,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
NARSPECS DS    0C                                                               
         SSPEC H1,60,RUN           SPECS FOR NARROW PRINTING                    
         SSPEC H2,60,REQUESTOR                                                  
         SSPEC H4,60,REPORT                                                     
         SSPEC H4,73,PAGE                                                       
         DC    X'00'                                                            
*                                                                               
WIDSPECS DS    0C                                                               
         WSPEC H1,2,RUN            SPECS FOR WIDE PRINTING                      
         WSPEC H2,2,REQUESTOR                                                   
         WSPEC H1,129,AGYNAME                                                   
         WSPEC H2,129,AGYADD                                                    
         WSPEC H4,129,REPORT                                                    
         WSPEC H4,142,PAGE                                                      
         DC    X'00'                                                            
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*--OVERFLOW ROUTINES                                                            
*                                                                               
         DS    0F                                                               
         DROP  R6,R7,RA,RB                                                      
OVFLRTN  NMOD1 0,**A54OV*                                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING OVFLRTN+4096,RA                                                  
         L     RC,4(R1)                                                         
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         LA    RE,RELO2C                                                        
         S     RE,RELO2C                                                        
         ST    RE,PGNR2                                                         
*                                                                               
         ZIC   RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     OVBRANCH(RF)                                                     
*                                                                               
OVBRANCH B     VALSTACK                                                         
         B     VALSTDEM                                                         
*        B     VGENERIC                                                         
         J     *+2                                                              
         B     VFLOWTAB                                                         
         B     VPEREXPR                                                         
         B     VALNTINM                                                         
         B     VSFTBOOK                                                         
         B     VALGSTCK                                                         
         B     VALPRGRP                                                         
         B     ADJDEMO                                                          
         B     SETERROR                                                         
         B     VALRP2                                                           
         B     VLASTWKS         12                                              
         B     VCOUNTBK         13                                              
         B     VALSYSCD         14                                              
         B     VALVIEWT         15                                              
         B     GETDPT           16                                              
         B     VALSVT           17                                              
         EJECT                                                                  
*              VALIDATE A STACK EXPRESSION                                      
*                                                                               
*              INPUT               R4=A(SCANNER BLOCK)                          
*                                                                               
VALSTACK ZIC   R1,1(R4)            LENGTH OF EXPRESSIONS                        
         LTR   R1,R1                                                            
         BZ    OVNEXIT                                                          
         LA    R1,22(R1,R4)                                                     
         MVI   0(R1),C'/'          DELIMIT LAST WITH A SLASH                    
         LA    R4,22(R4)           (BUMP TO EXPRESSION)                         
         LA    R3,PDSTADEF                                                      
         LA    R0,8                (MAX 8 TERMS)                                
*                                                                               
VSTK2    CLI   0(R4),C'T'          PREFIX OF T=TOTAL ONLY                       
         BNE   VSTK2A                                                           
         OI    0(R3),X'40'                                                      
*                                                                               
VSTK2A   L     RF,=A(STKTABLE)                                                  
         A     RF,PGNR2                                                         
         CLC   PODBEXT(3),=C'BBM'                                               
         BNE   VSTK2B                                                           
         L     RF,=A(STKTABC)                                                   
         A     RF,PGNR2                                                         
*                                                                               
VSTK2B   LR    R2,RF               R2-->STKTABLE                                
         LA    R8,7                7 BYTE EXPRESSIONS                           
*                                                                               
VSTK4    L     R6,0(R2)            GET ADDRESS FROM TABLE                       
         A     R6,PGNR2                                                         
         LR    R5,R8                                                            
         BCTR  R5,0                                                             
*                                                                               
VSTK4A   DS    0H                                                               
         CLI   0(R6),X'FF'         END OF TABLE?                                
         BE    VSTK5                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),1(R6)                                                    
         BE    VSTKFND                                                          
         AR    R6,R8               BUMP TO NEXT TABLE ELEMENT                   
         LA    R6,1(R6)                                                         
         B     VSTK4A                                                           
*                                                                               
VSTK5    BCTR  R8,0                                                             
         LTR   R8,R8                                                            
         BZ    OVNEXIT             CAN'T FIND IT, ERROR                         
         LA    R2,4(R2)            NEXT LENGTH IN STACK TABLE                   
         B     VSTK4                                                            
*                                                                               
VSTKFND  MVC   1(1,R3),0(R6)       SAVE EXPRESSION NUMBER                       
*                                                                               
VSTKFND2 CLI   0(R6),250                                                        
         BNE   VSTKAVLX                                                         
         MVI   1(R3),240           DEMO                                         
         MVI   3(R3),241           SHARE                                        
         MVI   5(R3),242           HPT                                          
         LA    R3,4(R3)                                                         
VSTKAVLX LA    R3,2(R3)                                                         
         AR    R4,R8                                                            
         CLI   0(R4),C'/'          NEED A /                                     
         BNE   OVNEXIT                                                          
         LA    R4,1(R4)                                                         
         CLI   0(R4),C' '                                                       
         BE    OVEQXIT                                                          
         BCT   R0,VSTK2                                                         
         B     OVNEXIT                                                          
         EJECT                                                                  
*-CHECK IF EXTRA DEMO'S CATEGORIES NEEDED TO                                    
*-COVER THE STACKS DEMO NEEDS.                                                  
*                                                                               
VALSTDEM L     R5,PDADEMTB                                                      
         USING PDDMBUFF,R5                                                      
         LA    R4,PODDEMWK         DEMO WORK AREA                               
         LA    R3,PODDEMO          DEMO HOLD AREA                               
         LA    RF,PODDEWPL         PERSONAL LANGUAGE WORK AREA                  
         LA    R2,PODDEMPL         PERSONAL LANGUAGE HOLD AREA                  
         LA    R1,PDSTADEF         STACK DEFINITION                             
         MVI   PODDMNUM,0          TOTAL NUMBER OF DEMOS                        
         MVI   PODDMENT,0          NUMBER OF ENTRIES IN EACH DEMO GROUP         
*                                                                               
VSTD20   CLI   0(R4),X'FF'                                                      
         BE    VSTD80                                                           
         MVI   PODDMENT,0                                                       
         CLI   PDOVSYS,3                                                        
         BNE   VSTD25                                                           
         MVC   0(4,R3),0(R4)                                                    
         LA    R3,4(R3)                                                         
         B     VSTD27                                                           
VSTD25   MVC   0(3,R3),0(R4)                                                    
         LA    R3,3(R3)                                                         
*                                                                               
VSTD27   MVC   0(PODPLLQ,R2),0(RF)  MOVE PERSONAL LANGUAGE ATTRIBUTE            
         LA    R2,PODPLLQ(R2)                                                   
*                                                                               
         ZIC   R0,PODDMNUM                                                      
         AHI   R0,1                                                             
         STC   R0,PODDMNUM                                                      
         ZIC   R0,PODDMENT                                                      
         AHI   R0,1                                                             
         STC   R0,PODDMENT                                                      
*                                                                               
VSTD30   CLI   1(R1),0             END OF STACK DEFINITIONS                     
         BNE   VSTD40                                                           
         AH    R4,LNDEMCOD         BUMP TO NEXT DEMO                            
         LA    RF,PODPLLQ(RF)      NEXT PERSONAL ATTRIBUTE IN WORK AREA         
         LA    R1,PDSTADEF         RE-START STACK LIST                          
         B     VSTD20                                                           
VSTD40   CLI   1(R1),C'A'                                                       
         BNL   VSTD50                                                           
         B     VSTD60                                                           
*                                                                               
VSTD50   MVC   0(3,R3),0(R4)                                                    
*                                                                               
         STM   RE,RF,DMCB                                                       
         CLI   1(R1),240           ABOVE 240 ARE SPECIAL                        
         BL    *+12                                                             
         BAS   RE,MODSTK           MODIFY THE STACK MODIFIERS                   
         B     *+10                AND BYPASS CONSTANT TYPE SET                 
*                                                                               
         MVC   1(1,R3),1(R1)       MOVE NEW TYPE IN                             
         LM    RE,RF,DMCB                                                       
VSTD52   CLI   PDOVSYS,3           NET SYSTEM?                                  
         BNE   *+8                                                              
         BAS   RE,HNDLNET          USE 4 BYTE DEMOS                             
         LA    R3,3(R3)                                                         
*                                                                               
         MVC   0(PODPLLQ,R2),0(RF)  MOVE PERSONAL LANGUAGE ATTRIBUTE            
         LA    R2,PODPLLQ(R2)                                                   
*                                                                               
         LA    R1,2(R1)                                                         
         ZIC   R0,PODDMNUM                                                      
         AHI   R0,1                                                             
         STC   R0,PODDMNUM                                                      
         ZIC   R0,PODDMENT                                                      
         AHI   R0,1                                                             
         STC   R0,PODDMENT                                                      
         B     VSTD30                                                           
*                                                                               
VSTD60   MVC   0(3,R3),0(R4)                                                    
         MVC   0(1,R3),1(R1)       MOVE NAD CATEGORY IN                         
         CLI   PDOVSYS,3           NET SYSTEM?                                  
         BNE   *+8                                                              
         BAS   RE,HNDLNAD          USE 4 BYTE DEMOS                             
         LA    R3,3(R3)                                                         
*                                                                               
         MVC   0(PODPLLQ,R2),0(RF)  MOVE PERSONAL LANGUAGE ATTRIBUTE            
         LA    R2,PODPLLQ(R2)                                                   
*                                                                               
         LA    R1,2(R1)                                                         
         ZIC   R0,PODDMNUM                                                      
         AHI   R0,1                                                             
         STC   R0,PODDMNUM                                                      
         ZIC   R0,PODDMENT                                                      
         AHI   R0,1                                                             
         STC   R0,PODDMENT                                                      
         B     VSTD30                                                           
*                                                                               
HNDLNET  MVC   0(4,R3),0(R4)       USE 4 BYTES FOR DEMOS                        
         MVC   1(1,R3),1(R1)                                                    
         LA    R3,1(R3)                                                         
         BR    RE                                                               
*                                                                               
HNDLNAD  MVC   0(4,R3),0(R4)       USE 4 BYTES FOR DEMOS                        
         CLI   1(R1),C'A'                                                       
         BL    *+14                                                             
         MVC   1(1,R3),1(R1)                                                    
         B     HNDLNAD2                                                         
         MVC   0(1,R3),1(R1)                                                    
HNDLNAD2 LA    R3,1(R3)                                                         
         BR    RE                                                               
*                                                                               
VSTD80   MVI   0(R3),X'FF'         END OF DEMO MARK                             
         B     OVEQXIT                                                          
*                                                                               
MODSTK   NTR1                                                                   
         CLI   1(R1),240           DEMO KEYWORK GETS NO MODIFICATION            
         BE    MODSTKX                                                          
         SR    RF,RF                                                            
         CLI   1(R1),241           SHARES                                       
         BNE   *+8                                                              
         LA    RF,KSHREQ                                                        
         CLI   1(R1),242           HPT                                          
         BNE   *+8                                                              
         LA    RF,KHPTEQ                                                        
         LTR   RF,RF               ERROR - JUST GET OUT                         
         BZ    MODSTKX                                                          
MODSTK2  CLI   0(RF),X'FF'         SCAN FOR MODIFIER EQUATES                    
         BE    MODSTKX                                                          
         CLC   1(1,R3),0(RF)                                                    
         BE    *+12                                                             
         LA    RF,2(RF)                                                         
         B     MODSTK2                                                          
         MVC   1(1,R3),1(RF)       SET NEW MODIFIER                             
MODSTKX  XIT1                                                                   
*                                                                               
KSHREQ   DC    C'RS',C' X',C'IX',C'TX',X'FF'                                    
KHPTEQ   DC    C'RP',C' Q',C'IQ',C'PP',X'FF'                                    
         DS    0H                                                               
         EJECT                                                                  
*              BUILD THE FLOWCHART DATE TABLE                                   
         SPACE 3                                                                
*              INPUT               R4=A(SCANNER BLOCK)                          
         SPACE 1                                                                
VFLOWTAB L     R2,PDAFLBUF         LENGTH OF EXPRESSIONS                        
         XCEFL 0(R2),300                                                        
         USING PDFLBUFF,R2                                                      
*                                                                               
*--SET WEEK TAB                                                                 
         LA    RF,VFLT100                                                       
         BAS   RE,FLTMCHK                                                       
         LA    RE,104                                                           
         MVC   WEEKLIST(2),PODBBKS                                              
         LA    R3,WEEKLIST                                                      
*                                                                               
VFLT20   MVC   2(2,R3),0(R3)                                                    
         ZIC   RF,3(R3)            BUMP THE WEEK                                
         LA    RF,1(RF)                                                         
         STC   RF,3(R3)                                                         
*                                                                               
         CLI   2(R3),X'64'         CHECK FOR 53 WEEK YEAR                       
         BNE   *+16                                                             
         CH    RF,=H'53'                                                        
         BNH   VFLT40                                                           
         B     *+12                                                             
*                                                                               
         CH    RF,=H'52'           CHECK FOR YEAR BREAK (MOST HAVE 52)          
         BNH   VFLT40                                                           
         ZIC   RF,2(R3)                                                         
         LA    RF,1(RF)            BUMP THE YEAR                                
         STC   RF,2(R3)                                                         
         MVI   3(R3),1             SET WEEK TO 1                                
VFLT40   LA    R3,2(R3)                                                         
         BCT   RE,VFLT20                                                        
*                                                                               
*--SET MONTH TAB                                                                
VFLT100  LA    R3,MNTHLIST                                                      
         MVC   0(2,R3),PODBBKS                                                  
         LA    RF,VFLT120                                                       
         BAS   RE,FLTMCHK                                                       
*--CONVERT WEEK FORMAT TO MONTH FORMAT                                          
         GOTO1 NETUNBK,DMCB,(C'M',0(R3)),DUB,GETDAY,ADDAY,GETBROAD              
         PACK  DUB(8),DUB+2(2)                                                  
         CVB   R1,DUB                                                           
         STC   R1,1(R3)                                                         
*                                                                               
VFLT120  LA    RE,24                                                            
*                                                                               
VFLT140  MVC   2(2,R3),0(R3)                                                    
         ZIC   RF,3(R3)            BUMP THE MONTH                               
         LA    RF,1(RF)                                                         
         STC   RF,3(R3)                                                         
         CH    RF,=H'12'           CHECK FOR YEAR BREAK                         
         BNH   VFLT160                                                          
         ZIC   RF,2(R3)                                                         
         LA    RF,1(RF)            BUMP THE YEAR                                
         STC   RF,2(R3)                                                         
         MVI   3(R3),1             SET MONTH TO ONE                             
VFLT160  LA    R3,2(R3)                                                         
         BCT   RE,VFLT140                                                       
*                                                                               
*--SET QUARTER TAB                                                              
         LA    R3,QURTLIST                                                      
         MVC   0(2,R3),PODBBKS                                                  
         LA    RF,VFLT200                                                       
         BAS   RE,FLTMCHK                                                       
*--CONVERT WEEK FORMAT TO QUARTER FORMAT (NTI,PIV)                              
         GOTO1 NETUNBK,DMCB,(C'Q',0(R3)),DUB,GETDAY,ADDAY,GETBROAD              
         MVC   1(1,R3),DUB+1       TAKE THE QUARTER NUMBER                      
         NI    1(R3),X'0F'         REMOVE THE ZONE                              
         B     VFLT220                                                          
*--CONVERT WEEK FORMAT TO QUARTER FORMAT (NAD, MPA)                             
VFLT200  L     R4,=A(FLWNAD)                                                    
         A     R4,PGNR2                                                         
         BAS   RE,GETMNQT          CONV FROM WEEK TO MONTH/QUARTER              
*                                                                               
VFLT220  LA    RE,7                                                             
*                                                                               
VFLT240  MVC   2(2,R3),0(R3)                                                    
         ZIC   RF,3(R3)            BUMP THE QUARTER                             
         LA    RF,1(RF)                                                         
         STC   RF,3(R3)                                                         
         CH    RF,=H'4'            CHECK FOR YEAR BREAK                         
         BNH   VFLT260                                                          
         ZIC   RF,2(R3)                                                         
         LA    RF,1(RF)            BUMP THE YEAR                                
         STC   RF,2(R3)                                                         
         MVI   3(R3),1             SET QUARTER TO ONE                           
VFLT260  LA    R3,2(R3)                                                         
         BCT   RE,VFLT240                                                       
*                                                                               
*--SET YEAR TAB                                                                 
         LA    RE,3                                                             
         MVC   YEARLIST(1),PODBBKS                                              
         LA    R3,YEARLIST                                                      
*                                                                               
VFLT300  MVC   2(2,R3),0(R3)                                                    
         ZIC   RF,2(R3)            BUMP THE YEAR                                
         LA    RF,1(RF)                                                         
         STC   RF,2(R3)                                                         
         LA    R3,2(R3)                                                         
         BCT   RE,VFLT300                                                       
*                                                                               
         B     OVXIT                                                            
         DROP  R2                                                               
*                                                                               
FLTMCHK  CLC   PODBEXT(3),=CL3'NAW'       NAW IS WEEKLY                         
         BER   RE                                                               
         CLC   PODBEXT(4),=CL4'CNAW'                                            
         BER   RE                                                               
         CLC   PODBFIL,=CL3'NAD'          COVERS NAD AND NHI                    
         BER   RF                                                               
         CLC   PODBEXT(3),=CL3'MPA'                                             
         BER   RF                                                               
         CLC   PODBEXT(3),=CL3'CSI'        CSI IS WEEKLY SOMETIMES              
         BE    FLTM02                                                           
         CLC   PODBEXT(3),=CL3'WTP'        NSI WTP OVERRIDE                     
         BE    FLTM02                                                           
         CLC   PODBEXT(3),=CL3'BBM'        BBM IS WEEKLY SOMETIMES              
         BE    FLTM06                                                           
         CLI   PODBMED,C'O'                                                     
         BER   RE                          OVERNIGHT TP IS WEEKLY               
         CLC   PODBFIL(3),=CL3'TP'         COVERS TP, T4 AND DPT                
         BER   RF                                                               
*                                                                               
FLTM02   CLC   PODBEXT(3),=CL3'PAV'                                             
         BER   RF                                                               
         CLC   PODBEXT(3),=CL3'IUN'                                             
         BER   RF                                                               
         CLI   PODBBKS,96                                                       
         BNL   FLTM04                                                           
         CLC   PODBEXT(3),=CL3'CSI'                                             
         BER   RF                                                               
*                                                                               
FLTM04   CLC   PODBEXT(3),=CL3'SRC'                                             
         BER   RF                                                               
         BR    RE                                                               
*                                                                               
FLTM06   CLI   SVBBMWK,C'Y'        YES, IS THIS WEEKLY?                         
         BER   RE                  YES                                          
         BR    RF                  NO                                           
         EJECT                                                                  
*--CONVERT WEEK TO MONTH OR QUARTER                                             
*--R3=BOOK DATE                                                                 
*--R4=CONVERSION TABLE                                                          
GETMNQT  NTR1                                                                   
GETMQ20  ZIC   RF,0(R4)                                                         
         LA    R4,1(R4)                                                         
         TM    1(R3),X'80'         ESTIMATED BOOK?                              
         BNO   GETMQ40                                                          
         NI    1(R3),X'7F'         EXTRACT MONTH                                
         SR    RE,RE                                                            
GETMQ40  CLC   1(1,R3),0(R4)                                                    
         BE    GETMQ100                                                         
         LA    R4,1(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   GETMQ40                                                          
         LA    R4,1(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     GETMQ20                                                          
*                                                                               
GETMQ100 STC   RF,1(R3)                                                         
         LTR   RE,RE               IF ZERO, IT WAS ESTIMATED BOOK               
         BNZ   OVXIT                                                            
         OI    1(R3),X'80'                                                      
         B     OVXIT                                                            
         EJECT                                                                  
*              ROUTINE TO VALIDATE PERIOD EXPRESSIONS                           
*                                                                               
*              INPUT               R4=A(SCANNER ENTRY)                          
*              OUTPUT              RETURN IN EXPLIST                            
*                                  X'00' NOT VALID                              
*                                  X'FF' VALID BUT DATE MISSED                  
*                                  X'NN00' PERIOD NUMBER                        
*                                  OR LIST OF PERIOD NUMBERS                    
*                                                                               
*              VALID               W1 THRU W105   SINGLE WEEK                   
*                                  M1 THRU M25           MONTH                  
*                                  Q1 THRU Q8            QUARTER                
*                                  Y1 THRU Y3            YEARS                  
*                                  WN-N           FLOW   WEEK                   
*                                  MN-N                  MONTH                  
*                                  QN-N                  QUARTER                
*                                  YN-N                  YEARS                  
*                                                                               
*                                                                               
VPEREXPR L     R5,PDAFLBUF                                                      
         USING PDFLBUFF,R5                                                      
         CLI   EXPLIST,0           IF THERE IS ONE THERE ALREADY                
         BNE   OVXIT                  WE ARE THROUGH                            
         SR    R2,R2                                                            
         LA    R3,105                                                           
         CLI   12(R4),C'W'         EXPRESSIONS MUST START W/M/Q/Y               
         BE    PEX2                                                             
         LA    R2,105                                                           
         LA    R3,25                                                            
         CLI   12(R4),C'M'                                                      
         BE    PEX2                                                             
         LA    R2,130                                                           
         LA    R3,8                                                             
         CLI   12(R4),C'Q'                                                      
         BE    PEX2                                                             
         LA    R2,138                                                           
         LA    R3,3                                                             
         CLI   12(R4),C'Y'                                                      
         BE    PEX2                                                             
         B     OVXIT                                                            
         SPACE 1                                                                
PEX2     LA    R4,13(R4)           R4=A(FIRST POTENTIAL NUMBER)                 
         BAS   RE,PEXNUM                                                        
         LTR   R0,R0               DID WE GET A VALID NUMBER                    
         BZ    OVXIT                                                            
         CR    R0,R3               THAT WASN'T TOO BIG?                         
         BH    OVXIT                                                            
         AR    R0,R2                                                            
         STC   R0,EXPLIST          THEN SAVE THAT NUMBER                        
         CLI   0(R4),C'-'          IS THERE A RANGE SPECIFIED?                  
         BNE   PEX6                                                             
         LA    R4,1(R4)                                                         
         BAS   RE,PEXNUM           WAS THE SECOND NUMBER VALID?                 
         LTR   R0,R0                                                            
         BZ    PEXNO                                                            
         CR    R0,R3                                                            
         BH    PEXNO                                                            
         AR    R0,R2                                                            
         ZIC   R1,EXPLIST                                                       
         CR    R0,R1                                                            
         BNH   PEXNO               SECOND MUST BE HIGHER THAN FIRST             
         SR    R1,R0                                                            
         LCR   R1,R1                                                            
         CH    R1,=H'15'           AND NOT MORE THAT 15 MORE                    
         BH    PEXNO                                                            
         ZIC   R1,EXPLIST                                                       
         LA    R1,1(R1)                                                         
         LA    R2,EXPLIST+1                                                     
         SPACE 1                                                                
PEX4     STC   R1,0(R2)            GENERATE A LIST OF NUMBERS                   
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         CR    R1,R0                                                            
         BNH   PEX4                                                             
         SPACE 1                                                                
PEX6     LA    R2,EXPLIST          NOW HAVE A LIST OF NUMBERS                   
         LA    R3,16               IN EXPLIST                                   
         SPACE 1                                                                
PEX8     CLI   0(R2),0                                                          
         BE    OVXIT                                                            
         ZIC   R1,0(R2)            CHECK THAT EACH NUMBER                       
         BCTR  R1,0                                                             
         SLL   R1,1                                                             
         AR    R1,R5                                                            
         OC    0(2,R1),0(R1)       PRODUCES A VALID DATE RANGE                  
         BZ    PEXMISS                                                          
         LA    R2,1(R2)                                                         
         BCT   R3,PEX8                                                          
         B     OVXIT                                                            
         SPACE 1                                                                
PEXMISS  MVI   EXPLIST,X'FF'       PASS BACK X'FF'                              
         B     OVXIT                                                            
         SPACE 1                                                                
PEXNO    XC    EXPLIST,EXPLIST                                                  
         B     OVXIT                                                            
         DROP  R5                                                               
         SPACE 1                                                                
PEXNUM   SR    R0,R0               VALIDATE FOR NUMERIC                         
*                                  R4=A(POTENTIAL EXPRESSION)                   
*                                  PASS BACK NUMBER IN R0                       
         SPACE 1                                                                
PEXNUM2  CLI   0(R4),C' '          DELIMITED BY SPACE OR DASH                   
         BER   RE                                                               
         CLI   0(R4),C'-'                                                       
         BER   RE                                                               
         CLI   0(R4),C'0'          CHECK FOR NUMERIC DIGIT                      
         BL    PEXNUMNO                                                         
         CLI   0(R4),C'9'                                                       
         BH    PEXNUMNO                                                         
         MH    R0,=H'10'           MULTIPLY PREVIOUS BY 10                      
         ZIC   R1,0(R4)            AND ADD THIS DIGIT                           
         SLL   R1,28               (STRIP TOP BITS)                             
         SRL   R1,28                                                            
         AR    R0,R1                                                            
         LA    R4,1(R4)                                                         
         B     PEXNUM2                                                          
         SPACE 1                                                                
PEXNUMNO SR    R0,R0               NO GOOD - RETURN ZERO                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*-SET NTI FILTER TABLE                                                          
*                                                                               
VALNTINM L     RE,APDNTIFT                                                      
         CLI   0(RE),0                                                          
         BNE   OVNEXIT             ALREADY INPUTTED ERROR                       
         CLI   22(R4),X'40'                                                     
         BNH   OVNEXIT             NO INPUT ERROR                               
         LA    R1,22(R4)                                                        
         L     R5,APDNTIFT                                                      
         LA    RF,10                                                            
*                                                                               
         CLI   0(R1),C'-'          MINUS SIGN MEANS EXCLUDE                     
         BNE   VNTI10                                                           
*                                                                               
         L     RE,APODBKL          EXCLUDE IS INVALID FOR MPA                   
VNTI03   CLI   0(RE),X'FF'                                                      
         BE    VNTI05              NOT MPA SOURCE. CONTINUE                     
         CLC   =C'MPA',PODBEXT-PODBD(RE)                                        
         BE    OVNEXIT             INVALID                                      
         CLI   PODBRNG-PODBD(RE),0                                              
         BE    *+8                                                              
         LA    RE,PODBLNQ(RE)                                                   
         LA    RE,PODBLNQ(RE)                                                   
         B     VNTI03                                                           
*                                                                               
VNTI05   OI    OPTFLAG1,EXCLNTI    EXCLUDE NTI NUMBERS IN THE LIST              
         LA    R1,1(R1)            GO PAST THE MINUS SIGN                       
*                                                                               
VNTI10   MVC   0(5,R5),0(R1)       NTI HOLD AREA                                
         LA    R1,5(R1)                                                         
         CLI   0(R1),X'40'                                                      
         BNH   OVEQXIT             NO MORE EXIT                                 
         CLI   0(R1),C'/'                                                       
         BNE   OVNEXIT                                                          
         LA    R1,1(R1)                                                         
         LA    R5,21(R5)                                                        
         BCT   RF,VNTI10                                                        
         BCTR  R1,0                                                             
         CLI   0(R1),C'/'                                                       
         BE    OVNEXIT             TOO MANY ENTRIES                             
         B     OVEQXIT             NO MORE EXIT                                 
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO CHECK FOR SOFT HUT EXPRESSIONS                    *         
*        ALLOWABLE           Q1/85      Q4/85                         *         
*                            Q1/85,86   Q4/85,86                      *         
*                            M1/85      M12/85                        *         
*                            M1/85,86   M12/85,86                     *         
***********************************************************************         
*                                                                               
VSFTBOOK DS    0H                                                               
         XC    SOFTRNGE,SOFTRNGE                                                
*        CLC   0(3,R5),=C'OPI'     OPI IS QUARTERLY                             
*        BE    VQRTBK                                                           
         CLC   7(3,R5),=C'NAD'     NOT APPLICABLE FOR NAD AND NHI               
         BE    SOFTNO                                                           
         CLC   0(3,R5),=C'MPA'     NOT APPLICABLE FOR MPA                       
         BE    SOFTNO                                                           
         CLC   7(3,R5),=C'CTP'     NOT APPLICABLE FOR COUNTY COVG               
         BE    SOFTNO                                                           
         CLC   PODBEXT(3),=CL3'OTP'   NSI TP OVERNIGHTS                         
         BE    *+10                                                             
         CLC   PODBEXT(3),=CL3'WTP'   NSI WTP OVERRIDE                          
         BE    *+14                                                             
         CLC   7(3,R5),=C'TP '     NOT APPLICABLE FOR TP, T4 AND DPT            
         BE    SOFTNO                                                           
         CLC   PODBEXT(3),=CL3'OPA'   NSI PAV OVERNIGHTS                        
         BE    *+14                                                             
         CLC   0(3,R5),=C'PAV'     NOT APPLICABLE FOR PAV                       
         BE    SOFTNO                                                           
         CLC   0(3,R5),=C'IUN'     NOT APPLICABLE FOR PAV                       
         BE    SOFTNO                                                           
         CLC   0(3,R5),=C'CSI'     NOT APPLICABLE FOR CSI                       
         BE    SOFTNO                                                           
         CLC   0(3,R5),=C'BBM'     NOT APPLICABLE FOR BBM                       
         BE    SOFTNO                                                           
         CLC   0(3,R5),=C'SRC'     NOT APPLICABLE FOR SRC                       
         BE    SOFTNO                                                           
         CLC   0(4,R5),=C'SQAD'    NOT APPLICABLE FOR SQAD                      
         BE    SOFTNO                                                           
         CLI   MAX,1               MUST BE REQUESTING MULTIPLE BOOKS            
         BE    SOFTNO                                                           
                                                                                
*                                  CHECK FOR A MATCH ON PHRASE                  
         L     R1,=A(HUTTABLE)                                                  
         A     R1,PGNR2                                                         
         LA    R2,12(R3)           POINT TO DATE                                
*                                                                               
VSB10    ZIC   RE,0(R1)            LENGTH OF PHRASE                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),1(R1)                                                    
         BE    VSB20                                                            
         LA    R1,13(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         BE    SOFTNO                                                           
         B     VSB10                                                            
*                                                                               
VSB20    MVI   0(R4),X'01'         SOFT EXPRESSION IS A RANGE                   
         MVC   1(11,R4),0(R5)                                                   
         MVC   PODBLNQ+PODBEXT-PODBD(11,R4),0(R5)                               
         CLI   1(R1),C'Q'          CHECK FOR QUARTERS                           
         BE    VSB40                                                            
*                                                                               
*--MONTH HANDLING                                                               
         XC    WORK,WORK                                                        
         MVC   DUB+2(4),5(R1)                                                   
         LA    R2,1(RE,R2)         SPACE TO YEAR                                
         CLI   0(R2),C'/'          GET PAST /                                   
         BNE   *+8                                                              
         LA    R2,1(R2)                                                         
         CLI   0(R2),C'0'          MUST BE NUMERIC                              
         BL    SOFTNO                                                           
         CLI   1(R2),C'0'                                                       
         BL    SOFTNO                                                           
*        CLI   2(R2),C' '          AND EXPRESSION MUST END THERE                
*        BH    SOFTNO                                                           
         CLI   2(R2),C' '                                                       
         BNH   VSB22                                                            
         BAS   RE,CKBTYPE                                                       
         BNE   SOFTNO                                                           
VSB22    MVC   DUB(2),0(R2)                                                     
         GOTO1 DATCON,PARAS,(0,DUB),(3,FULL)                                    
         GOTO1 DATCON,PARAS,(3,FULL),(0,DUB)                                    
*                                                                               
         GOTO1 GETBROAD,PARAS,(1,DUB),WORK,GETDAY,ADDAY                         
*                                                                               
         CLI   PDCALOPT,PDCALNTI                                                
         BE    *+16                                                             
         CLI   SOFTRNGE,1                                                       
         BNE   VSB23                                                            
         B     VSB30                                                            
         GOTO1 DATCON,PARAS,(0,DUB),(3,FULL)                                    
*                                                                               
         L     RF,ACOMFACS         GET A(MONTH/DATES TABLE)                     
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,MTHDATES                                               
         ICM   RE,15,0(R1)         A(MONTH/DATES TABLE)                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)            LENGTH OF TABLE ENTRY                        
         ST    RE,AMTHTAB                                                       
         ST    RF,MTHTABL                                                       
*                                                                               
         USING MTHDATD,RE                                                       
         CLC   FULL(2),MDNLBOOK                                                 
         BNH   *+10                                                             
         AR    RE,RF               TRY NEXT ENTRY                               
         B     *-12                                                             
         CLC   FULL(2),MDNLBOOK                                                 
         BL    VSB23                                                            
         DROP  RE                                                               
*                                                                               
         ST    RE,PARAS+16                                                      
         GOTO1 DATCON,PARAS,(3,(RE)),(0,WORK)                                   
         L     RE,PARAS+16                                                      
         GOTO1 DATCON,PARAS,(3,3(RE)),(0,WORK+6)                                
         CLI   SOFTRNGE,1                                                       
         BE    VSB30                                                            
*                                                                               
*GET START RANGE                                                                
VSB23    GOTO1 NETWEEK,PARAS,WORK,GETDAY,ADDAY                                  
         ZIC   RE,PARAS+4                                                       
         CHI   RE,84                YEAR < 1984 IS INVALID                      
         BL    SOFTNO                                                           
         MVC   12(1,R4),PARAS+4                                                 
         MVC   13(1,R4),PARAS+12                                                
*                                                                               
VSB25    CLI   1(R3),0             IS THERE A SECOND EXPRESSION                 
         BE    VSB30                                                            
         L     R1,=A(HUTTABLE)                                                  
         A     R1,PGNR2                                                         
         LA    R2,24(R3)                                                        
         MVI   SOFTRNGE,1                                                       
         B     VSB10                                                            
*                                                                               
*        ZIC   R0,11(R3)           YES - IT MUST BE ANOTHER YEAR                
*        LTR   R0,R0                                                            
*        BZ    SOFTNO                                                           
*        CLC   22(2,R3),DUB        CHECK YEARS IN SEQ                           
*        BL    SOFTNO                                                           
*        MVC   DUB(2),22(R3)                                                    
*                                                                               
*        GOTO1 DATCON,PARAS,(0,DUB),(3,FULL)                                    
*        GOTO1 DATCON,PARAS,(3,FULL),(0,DUB)                                    
*        GOTO1 GETBROAD,PARAS,(1,DUB),WORK,GETDAY,ADDAY                         
*                                                                               
*        CLI   PDCALOPT,PDCALNTI                                                
*        BNE   VSB30                                                            
*        GOTO1 DATCON,PARAS,(0,DUB),(3,FULL)                                    
*        L     RE,AMTHTAB          SAVED A(MONTH/WEEK TABLE)                    
*        L     RF,MTHTABL          SAVED LENGTH OF TABLE                        
*        USING MTHDATD,RE                                                       
*        CLC   FULL(2),MDNLBOOK                                                 
*        BNH   *+10                                                             
*        AR    RE,RF               NEXT ENTRY                                   
*        B     *-12                                                             
*        CLC   FULL(2),MDNLBOOK                                                 
*        BL    VSB30                                                            
*        GOTO1 DATCON,PARAS,(3,3(RE)),(0,WORK+6)                                
*        DROP  RE                                                               
*                                                                               
*GET END RANGE                                                                  
VSB30    GOTO1 NETWEEK,PARAS,WORK+6,GETDAY,ADDAY                                
         MVC   PODBLNQ+PODBBKS-PODBD(1,R4),PARAS+4                              
         MVC   PODBLNQ+PODBBKS+1-PODBD(1,R4),PARAS+12                           
         B     SOFTYES                                                          
*--QUARTER HANDLING                                                             
VSB40    XC    WORK,WORK                                                        
         MVC   DUB+2(4),5(R1)                                                   
         MVC   WORK+30(4),9(R1)    END DATE FOR QUARTER                         
         ST    R2,FULL                                                          
         LA    R2,1(RE,R2)         SPACE TO YEAR                                
         CLI   0(R2),C'/'          GET PAST /                                   
         BNE   *+8                                                              
         LA    R2,1(R2)                                                         
         CLI   0(R2),C'0'          MUST BE NUMERIC                              
         BL    SOFTNO                                                           
         CLI   1(R2),C'0'                                                       
         BL    SOFTNO                                                           
*        CLI   2(R2),C' '          AND EXPRESSION MUST END THERE                
*        BH    SOFTNO                                                           
         CLI   2(R2),C' '                                                       
         BNH   VSB42                                                            
         BAS   RE,CKBTYPE                                                       
         BNE   SOFTNO                                                           
VSB42    MVC   DUB(2),0(R2)                                                     
         CLI   PDCALOPT,PDCALNTI   SPECIAL NTI CALENDAR REQUESTED               
         BNE   VSB45                                                            
*                                                                               
         L     RF,ACOMFACS         GET A(MONTH/DATES TABLE)                     
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,NENTIQRT                                               
         ICM   RE,15,0(R1)         A(MONTH/DATES TABLE)                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING NEQTRD,RE                                                        
         L     RF,FULL                                                          
*                                                                               
VSBNTI   CLI   0(RE),X'FF'                                                      
         BE    VSB45                                                            
         CLC   DUB(2),NEQTRYR       MATCH YEAR                                  
         BNE   *+10                                                             
         CLC   0(2,RF),NEQTRQT      MATCH MONTH                                 
         BE    *+12                                                             
         LA    RE,16(RE)                                                        
         B     VSBNTI                                                           
*                                                                               
         MVC   WORK(6),NEQTRFRS     FIRST DAY OF MONTH                          
         MVC   WORK+30(4),NEQTRLST  YYMM                                        
         MVC   WORK+6(6),NEQTRLST   LAST DAY OF MONTH                           
         GOTO1 NETWEEK,PARAS,WORK,GETDAY,ADDAY                                  
         ZIC   RE,PARAS+4          YEAR < 1984 IS INVALID                       
         CHI   RE,84                                                            
         BL    SOFTNO                                                           
         MVC   12(1,R4),PARAS+4                                                 
         MVC   13(1,R4),PARAS+12                                                
*                                                                               
         GOTO1 NETWEEK,PARAS,WORK+6,GETDAY,ADDAY                                
         MVC   PODBLNQ+PODBBKS-PODBD(1,R4),PARAS+4                              
         MVC   PODBLNQ+PODBBKS+1-PODBD(1,R4),PARAS+12                           
         B     SOFTYES                                                          
*                                                                               
VSB45    CLI   13(R3),C'4'         CHECK 4TH QTR                                
         BNE   VSB50                                                            
         GOTO1 NETWEEK,PARAS,DUB,GETDAY,ADDAY                                   
         ZIC   RE,PARAS+4                                                       
         CHI   RE,84                YEAR < 1984 IS INVALID                      
         BL    SOFTNO                                                           
         MVC   12(1,R4),PARAS+4                                                 
         ZIC   RF,PARAS+12                                                      
         LA    RF,1(RF)                                                         
         STC   RF,13(R4)                                                        
         B     VSB60                                                            
*                                                                               
VSB50    GOTO1 DATCON,PARAS,(0,DUB),(3,FULL)                                    
         GOTO1 DATCON,PARAS,(3,FULL),(0,DUB)                                    
         GOTO1 GETBROAD,PARAS,(1,DUB),WORK,GETDAY,ADDAY                         
*GET START RANGE                                                                
         GOTO1 NETWEEK,PARAS,WORK,GETDAY,ADDAY                                  
         ZIC   RE,PARAS+4                                                       
         CHI   RE,84                YEAR < 1984 IS INVALID                      
         BL    SOFTNO                                                           
         MVC   12(1,R4),PARAS+4                                                 
         MVC   13(1,R4),PARAS+12                                                
*                                                                               
VSB60    MVC   DUB+2(4),WORK+30    GET END RANGE FOR QUARTER                    
         CLI   1(R3),0             IS THERE A SECOND EXPRESSION                 
         BE    VSB65                                                            
         ZIC   R0,11(R3)           YES - IT MUST BE ANOTHER YEAR                
         LTR   R0,R0                                                            
         BZ    SOFTNO                                                           
         CLC   22(2,R3),DUB        CHECK YEARS IN SEQ                           
         BL    SOFTNO                                                           
         MVC   DUB(2),22(R3)                                                    
*                                                                               
VSB65    CLI   13(R3),C'3'         CHECK 3RD QTR                                
         BNE   VSB70                                                            
         GOTO1 NETWEEK,PARAS,DUB,GETDAY,ADDAY                                   
         MVC   PODBLNQ+PODBBKS-PODBD(1,R4),PARAS+4                              
         MVC   PODBLNQ+PODBBKS+1-PODBD(1,R4),PARAS+12                           
         B     SOFTYES                                                          
*                                                                               
VSB70    DS    0H                                                               
         GOTO1 DATCON,PARAS,(0,DUB),(3,FULL)                                    
         GOTO1 DATCON,PARAS,(3,FULL),(0,DUB)                                    
         GOTO1 GETBROAD,PARAS,(1,DUB),WORK,GETDAY,ADDAY                         
*GET END RANGE                                                                  
         GOTO1 NETWEEK,PARAS,WORK+6,GETDAY,ADDAY                                
         MVC   PODBLNQ+PODBBKS-PODBD(1,R4),PARAS+4                              
         MVC   PODBLNQ+PODBBKS+1-PODBD(1,R4),PARAS+12                           
         B     SOFTYES                                                          
*                                                                               
SOFTYES  SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     OVXIT                                                            
         SPACE 1                                                                
*--IF ERROR CLEAR BOOK TABLE OF ALL ENTRIES CREATED IN THIS PASS                
SOFTNO   XC    0(PODBLNQ,R4),0(R4)                                              
         LA    R4,PODBLNQ(R4)                                                   
         CLI   1(R4),0                                                          
         BNE   SOFTNO                                                           
*                                                                               
         LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     OVXIT                                                            
         EJECT                                                                  
***********************************************************************         
*              SEE IF HUT EXPRESSION FOLLOWED BY BOOK TYPE            *         
*                    INPUT  1(R2) -> END OF HUT EXPRESSION            *         
***********************************************************************         
CKBTYPE  NTR1                                                                   
         CLI    2(R2),C'('     BOOKTYPES MUST BE BETWEEN PARENTHESIS            
         BNE    CKBTYNO                                                         
         CLI    4(R2),C')'                                                      
         BE     *+12                                                            
         CLI    5(R2),C')'                                                      
         BNE    CKBTYNO                                                         
         LA     RF,3(R2)                                                        
         BRAS   RE,VSPTBTYP                                                     
         BNE    CKBTYNO                                                         
         STC    RF,14(R4)      SET BOOK TYPE                                    
         B      CKBTYYES                                                        
*                                                                               
CKBTYYES CR     R0,R0                                                           
         B      CKBTYX                                                          
CKBTYNO  CHI    RE,0                                                            
         B      CKBTYX                                                          
*                                                                               
CKBTYX   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*              ALLOW EXPRESSIONS "LAST1"-"LAST12"                     *         
*              EX: 'LAST5' MEANS RETURN THE LAST 5 WEEKS              *         
***********************************************************************         
VLASTWKS DS    0H                                                               
         CLC   0(3,R5),=C'NTI'     WEEKLY NETWORK SOURCES                       
         BE    VLW1                                                             
         CLC   0(4,R5),=C'OOH '    WEEKLY NETWORK SOURCES                       
         BE    VLW1                                                             
         CLC   0(5,R5),=C'ACM  '                                                
         BE    VLW1                                                             
         CLC   0(5,R5),=C'OOHC '                                                
         BE    VLW1                                                             
         CLC   0(5,R5),=C'MXM  '                                                
         BE    VLW1                                                             
         CLC   0(3,R5),=C'NHW'                                                  
         BE    VLW1                                                             
         CLC   0(3,R5),=C'HPW'                                                  
         BE    VLW1                                                             
         CLC   0(3,R5),=C'NAW'                                                  
         BE    VLW1                                                             
         CLC   0(3,R5),=C'NHC'                                                  
         BE    VLW1                                                             
         CLC   0(3,R5),=C'HPC'                                                  
         BE    VLW1                                                             
         CLC   0(4,R5),=C'CNAW'                                                 
         BNE   LASTNO                                                           
*                                                                               
VLW1     LA    R2,12(R3)           POINT TO DATE                                
         CLC   =C'LAST',0(R2)                                                   
         BNE   LASTNO                                                           
         ZIC   RF,0(R3)            LENGTH OF FIELD                              
         LA    RE,0(R2,RF)         END OF FIELD                                 
         LA    R2,4(R2)                                                         
         SR    R1,R1                                                            
*                                                                               
VLW5     TM    0(R2),X'F0'                                                      
         BNO   LASTNO                                                           
         LA    R2,1(R2)            ADVANCE ONE POSITION                         
         LA    R1,1(R1)            INCREASE COUNTER                             
         CHI   R1,2                MAX 2 DIGITS                                 
         BH    LASTNO                                                           
         CR    R2,RE                                                            
         BNL   VLW20                                                            
         CLI   0(R2),C' '                                                       
         BE    VLW20                                                            
         CLI   0(R2),C'('          FOLLOWED BY BOOKTYPE                         
         BE    VLW10                                                            
         B     VLW5                                                             
*                                                                               
VLW10    SHI   R2,2                                                             
         BAS   RE,CKBTYPE          POSITION MYSELF FOR CKBTYPE                  
         BNE   LASTNO                                                           
*                                                                               
VLW20    XC    DUB,DUB                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,16(0,R3)                                                     
         CVB   R1,DUB              R1 = N (NUMBER OF WEEKS)                     
         CHI   R1,1                                                             
         BL    LASTNO                                                           
         CHI   R1,12                                                            
         BH    LASTNO                                                           
         BCTR  R1,0                NO OF WEEKS TO BACK OUT                      
         MHI   R1,7                WEEKS*7 = DAYS                               
         SR    R2,R2                                                            
         SR    R2,R1               WITH NEGATIVE SIGN                           
         GOTO1 DATCON,PARAS,(5,DUB),(0,DUB)     TODAY'S DATE                    
         GOTO1 ADDAY,PARAS,(C'D',DUB),(0,WORK),(R2)  MINUS N WEEKS              
*                                                                               
*NOW: WORK = START DATE, DUB = END DATE                                         
         GOTO1 NETWEEK,PARAS,WORK,GETDAY,ADDAY                                  
         ZIC   RE,PARAS+4                                                       
         CHI   RE,84                YEAR < 1984 IS INVALID                      
         BL    LASTNO                                                           
         MVC   12(1,R4),PARAS+4     START BOOK                                  
         MVC   13(1,R4),PARAS+12                                                
         MVI   0(R4),X'01'          RANGE INDICATOR                             
         MVC   1(11,R4),0(R5)                                                   
*                                                                               
         GOTO1 NETWEEK,PARAS,DUB,GETDAY,ADDAY                                   
         LA    R4,PODBLNQ(R4)                                                   
         MVC   12(1,R4),PARAS+4     END BOOK                                    
         MVC   13(1,R4),PARAS+12                                                
         MVC   1(11,R4),0(R5)                                                   
*                                                                               
LASTYES  SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     OVXIT                                                            
         SPACE 1                                                                
*--IF ERROR CLEAR BOOK TABLE OF ALL ENTRIES CREATED IN THIS PASS                
LASTNO   XC    0(PODBLNQ,R4),0(R4)                                              
         LA    R4,PODBLNQ(R4)                                                   
         CLI   1(R4),0                                                          
         BNE   LASTNO                                                           
         LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     OVXIT                                                            
         EJECT                                                                  
***********************************************************************         
*              VALIDATE A GENDER STACK EXPRESSION                     *         
*                    INPUT  R4=A(SCANNER BLOCK)                       *         
***********************************************************************         
*                                                                               
VALGSTCK ZIC   R1,1(R4)            LENGTH OF EXPRESSIONS                        
         LTR   R1,R1                                                            
         BZ    VALGNEX             NOTHING INPUT, ERROR                         
         LA    R1,22(R1,R4)                                                     
         MVI   0(R1),C'/'          DELIMIT LAST WITH A SLASH                    
         LA    R4,22(R4)           (BUMP TO EXPRESSION)                         
*                                                                               
         LA    R3,PDSTADEF                                                      
         LA    R0,8                (MAX 8 TERMS)                                
*                                                                               
*ALG00   LA    RF,GENTAB           GET GEN TABLE                                
VALG00   L     RF,=A(GENTAB)       GET GEN TABLE                                
         A     RF,PGNR2                                                         
*                                                                               
VALG02   CLI   0(RF),X'FF'         END OF TABLE?                                
         BE    VALGNEX                                                          
         CLC   0(1,R4),0(RF)                                                    
         BE    VALG04                                                           
         LA    RF,1(RF)                                                         
         B     VALG02                                                           
*                                                                               
VALG04   MVC   1(1,R3),0(RF)       SAVE IN LIST                                 
         LA    R3,2(R3)                                                         
         LA    R4,1(R4)                                                         
         CLI   0(R4),C'/'          NEED A /                                     
         BNE   VALGNEX                                                          
         LA    R4,1(R4)                                                         
         CLI   0(R4),C' '          ANYTHING LEFT?                               
         BE    VALGEQX             NO, DONE                                     
         BCT   R0,VALG00                                                        
*                                                                               
VALGNEX  LTR   RB,RB                                                            
         B     OVXIT                                                            
*                                                                               
VALGEQX  MVI   0(R3),X'FF'                                                      
         CR    RB,RB                                                            
         B     OVXIT                                                            
         EJECT                                                                  
***********************************************************************         
*              VALIDATE A PROGRAM GROUP OR LIST OF PROGRAM GROUPS     *         
*              STORE LIST OF PROGRAMS IN PRODNTIFT                    *         
*                    INPUT  R4=A(SCANNER BLOCK)                       *         
***********************************************************************         
*                                                                               
VALPRGRP ZIC   R1,1(R4)            LENGTH OF EXPRESSIONS                        
         LTR   R1,R1                                                            
         BZ    VALPNEX             NOTHING INPUT, ERROR                         
*                                                                               
         LA    R1,22(R1,R4)                                                     
         MVI   0(R1),C'/'          DELIMIT LAST WITH A SLASH                    
         LA    R4,22(R4)           (BUMP TO EXPRESSION)                         
*                                                                               
         L     R3,APODPRGL                                                      
         LA    R0,10               MAXIMUM NUMBER OF GROUPS ALLOWED             
*                                                                               
VALP02   LR    R1,R4               GET LENGTH OF THIS EXPRESSION                
*                                                                               
VALP04   CLI   0(R1),C'/'                                                       
         BE    VALP06                                                           
         LA    R1,1(R1)                                                         
         B     VALP04                                                           
*                                                                               
VALP06   SR    R1,R4                                                            
         BZ    VALPNEX                                                          
         CHI   R1,8                                                             
         BH    VALPNEX                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R4)                                                    
         OC    0(8,R3),SPACES                                                   
         LA    R4,2(R4,R1)         GET TO NEXT PROGRAM GROUP                    
*                                                                               
         LA    R5,KEY              BUILD KEY                                    
         USING NPRGRECD,R5                                                      
         XC    NPRGKEY,NPRGKEY                                                  
         MVC   NPRGKTYP,=X'0D3C'                                                
         MVC   NPRGKAGM,PDBAGYMD                                                
         MVC   NPRGKCOD,0(R3)      GET PROGRAM GROUP CODE                       
*                                                                               
         MVC   DATADISP,=H'24'                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     MAKE SURE WE FOUND THE GROUP                 
         BNE   VALPNEX                                                          
*                                                                               
         LA    R3,8(R3)            ALLOW FOR MAX LENGTH                         
         MVI   0(R3),X'FF'         MARK END OF TABLE                            
         CLI   0(R4),C' '          ANY MORE?                                    
         BNH   VALPEQX             NO                                           
         BCT   R0,VALP02                                                        
*                                                                               
VALPEQX  B     OVEQXIT                                                          
*                                                                               
VALPNEX  B     OVNEXIT                                                          
         EJECT                                                                  
***********************************************************************         
*          ADJUST DEMO NUMBER FOR CONTINUATION SCREENS                *         
***********************************************************************         
ADJDEMO  DS    0H                                                               
         CLI   12(R4),C'D'        DEMOS                                         
         BNE   ADJD5                                                            
         LA    RE,13(R4)                                                        
         LA    R3,1                                                             
         B     ADJD10                                                           
*                                                                               
ADJD5    CLC   12(3,R4),=C'STD'    STACKED DEMOS                                
         BNE   ADJDX                                                            
         LA    RE,15(R4)                                                        
         LA    R3,3                                                             
*                                                                               
ADJD10   ZIC   R1,0(R4)           LENGTH OF COLUMN ENTRY                        
         SR    R1,R3              LESS LENGTH OF LITERAL                        
         LR    RF,RE              SAVE ADDRESS OF NUMERIC                       
         LR    R2,R1              SAVE LENGTH OF NUMERIC(1 OR 2)                
ADJD11   TM    0(RE),X'F0'        VALID NUMBER                                  
         BNO   ADJDX                                                            
         LA    RE,1(RE)                                                         
         BCT   R1,ADJD11                                                        
*                                                                               
         XC    DUB,DUB                                                          
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RF)                                                      
         CVB   R1,DUB                                                           
*                                                                               
         ZIC   RE,NDEMS1           NO OF DEMOS ON 1ST SCREEN                    
         AR    R1,RE                                                            
*                                                                               
         EDIT  (R1),(2,DUB)                                                     
         CLI   DUB,C' '            NO IS ONE DIGIT                              
         BNE   ADJD20                                                           
         MVC   0(1,RF),DUB+1       MOVE NEW NUMBER IN                           
         B     ADJDX               AND LENGTH STAYS THE SAME                    
*                                                                               
ADJD20   MVC   0(2,RF),DUB                                                      
         AHI   R3,2               ADD TO LENGTH OF LITERAL                      
         STC   R3,0(R4)           ADJUST LENGTH. IT MAY HAVE CHANGED            
*                                                                               
ADJDX    B     OVXIT                                                            
         EJECT                                                                  
***********************************************************************         
*SET ERROR MESSAGE FOR CONTINUATION SCREEN                                      
*RESTORE ORIGINAL SCREEN                                                        
***********************************************************************         
SETERROR L     RE,ATIA             ORIGINAL SCREEN (REPORT 1)                   
         L     R0,ATWA                                                          
         LHI   R1,SPLWORK-T325FFD                                               
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVC   GTMSGNO,=Y(BADCONT)  THIS IS USED IF NOT COLWIDE                 
*                                                                               
         CLI   ERROR,X'FE'          CUSTOM MESSAGE                              
         BNE   SETERX                                                           
         MVI   ERROR,X'FF'          ALWAYS USE BADCONT MESSAGE                  
         OI    GENSTAT2,USGETTXT    NEED EXTRA SETUP                            
         MVC   GTMTYP,PMSGTYPE                                                  
         MVI   GTMSYS,24                                                        
*                                                                               
SETERX   DS    0H                                                               
         B     OVXIT                                                            
         EJECT                                                                  
* VALRP2: VALIDATE CONTINUE OPTION                                              
*  PASS R4 = A(SCAN BLOCK ENTRY)                                                
*                                                                               
VALRP2   DS    0H                                                               
         SR    RE,RE                                                            
         ICM   RE,1,1(R4)          GET L'REPORT NAME                            
         JZ    XITNEQ                                                           
         CHI   RE,8                MAKE SURE IT'S NOT TOO LONG                  
         JH    XITNEQ                                                           
*                                                                               
         XC    KEY,KEY             READ REPORT KEY TO VERIFIFY                  
         LA    R2,KEY                                                           
         USING CT01RECD,R2         ESTABLISH PROGRAM RECORD                     
         MVI   CT01TYPE,CT01TYPQ   SET RECORD TYPE                              
         MVC   CT01AGID,AGENCY     SET AGENCY ID                                
         MVI   CT01SYS,3           SET FOR NETWORK                              
         MVI   CT01PRG,37          SET FOR NETWORK RES WRITER                   
         MVI   CT01PHAS,1          SET FOR PHASE 1                              
         MVC   CT01NAME,22(R4)     SECOND REPORT NAME                           
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'CTFILE' SET FILENAME                               
         MVI   USEIO,C'Y'                                                       
         GOTO1 HIGH                READ IN PROGRAM RECORD                       
*                                                                               
         XC    FILENAME,FILENAME   RESET FILENAME                               
         MVI   USEIO,C'N'                                                       
*                                                                               
         CLC   CT01KEY,KEYSAVE     MUST FIND IT                                 
         BNE   XITNEQ                                                           
         MVC   RPT2ID,CT01NAME     SAVE SECOND REPORT ID                        
         BE    XITEQU                                                           
*                                                                               
XITEQU   B     OVEQXIT                                                          
*                                                                               
XITNEQ   B     OVNEXIT                                                          
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*              COUNT NUMBER OF BOOKS REQUESTED FOR A SOURCE           *         
*              8(R1) HAS A(SOURCE)                                    *         
***********************************************************************         
VCOUNTBK DS    0H                                                               
         SR    R2,R2                 START COUNTER                              
*                                                                               
         L     RE,APODBKL                                                       
         USING PODBD,RE                                                         
VCNTB10  CLI   0(RE),X'FF'                                                      
         BE    VCOUNTBX                                                         
         ICM   RF,15,8(R1)                                                      
         CLC   PODBEXT,0(RF)         CHECK ON SOURCE                            
         BNE   VCOUNTNX                                                         
*                                                                               
         CLI   PODBRNG,0             JUST ONE BOOK                              
         BNE   *+12                                                             
         AHI   R2,1                                                             
         B     VCOUNTNX                                                         
*                                    RANGE OF BOOKS                             
         LA    RF,PODBLNQ(RE)        RF->END BOOK ENTRY                         
         MVC   HALF,PODBBKS          RE->START BOOK ENTRY                       
VCNTB20  AHI   R2,1                                                             
         CLC   HALF,PODBBKS-PODBD(RF)                                           
         BNL   VCOUNTNX                                                         
         ZIC   R0,HALF+1                                                        
         AHI   R0,1                                                             
         STC   R0,HALF+1                                                        
*                                                                               
         CLC   =C'CNAW',PODBEXT                                                 
         BE    VCNTB30                                                          
         CHI   R0,12                 START OF NEW YEAR                          
         BNH   VCNTB20                FOR MONTHLY SOURCES                       
         B     VCNTB35                                                          
*                                                                               
VCNTB30  CHI   R0,53                 START OF NEW YEAR                          
         BNH   VCNTB20                FOR WEEKLY SOURCES                        
*                                                                               
VCNTB35  MVI   HALF+1,1                                                         
         ZIC   R0,HALF                                                          
         AHI   R0,1                                                             
         STC   R0,HALF                                                          
         B     VCNTB20                                                          
*                                                                               
VCOUNTNX ZIC   R0,PODBRNG            NEXT BOOK OR RANGE                         
         LA    RE,PODBLNQ(RE)        SKIP START BOOK                            
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         LA    RE,PODBLNQ(RE)        RANGE, SKIP END BOOK                       
         B     VCNTB10                                                          
*                                                                               
VCOUNTBX ST    R2,0(R1)              NUMBER OF BOOKS                            
         B     OVEQXIT                                                          
         DROP  RE                                                               
         EJECT                                                                  
*                                                                               
*-SET SYSCODE FILTER TABLE                                                      
*                                                                               
VALSYSCD L     RE,APDSYSC                                                       
         CLC   =X'FFFF',0(RE)                                                   
         BNE   OVNEXIT             ALREADY INPUTTED, ERROR                      
         CLI   22(R4),X'40'                                                     
         BNH   OVNEXIT             NO INPUT ERROR                               
         LA    R1,22(R4)                                                        
         L     R5,APDSYSC                                                       
         LA    RF,MAXSYSC          MAXIMUM NO OF SYSCODES                       
         TM    PDFLAG1,PDF1ALST    ALL-STATION/SYSCODE COMBINATION              
         BNO   *+8                                                              
         LA    RF,5                MAXIMUM 5 SYSCODES IN THIS CASE              
*                                                                               
VSYS01   SR    RE,RE                                                            
         LR    R2,R1               BEGINNING OF SYSCODE                         
VSYS05   CLI   0(R1),X'40'                                                      
         BNH   VSYS10              NO MORE EXIT                                 
         CLI   0(R1),C'/'                                                       
         BE    VSYS10              END OF SYSCODE                               
         TM    0(R1),X'F0'                                                      
         BNO   OVNEXIT             HAS TO BE NUMERIC                            
         LA    R1,1(R1)                                                         
         AHI   RE,1                                                             
         B     VSYS05                                                           
*                                                                               
VSYS10   CHI   RE,MXSCLQ           MAX LENGTH OF A SYSCODE                      
         BH    OVNEXIT                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8              STORE BINARY SYSCODES                        
         B     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,3,0(R5)                                                       
         LA    R5,2(R5)                                                         
         CLI   0(R1),C'/'                                                       
         BE    *+14                                                             
         MVC   0(2,R5),=X'FFFF'    END OF SYSCODE TABLE                         
         B     OVEQXIT                                                          
         LA    R1,1(R1)                                                         
         BCT   RF,VSYS01                                                        
         B     OVNEXIT             TOO MANY SYSCODES                            
*                                                                               
MXSCLQ   EQU   4                                                                
         EJECT                                                                  
*                                                                               
*                                                                               
*-SET VIEWING TYPES                                                             
*                                                                               
VALVIEWT OC    PDSVTS,PDSVTS       OPTIONS VIEWTYP AND SVT ARE                  
         BNZ   OVNEXIT             MUTUALLY EXCLUSIVE                           
*                                                                               
         LA    R1,22(R4)                                                        
*                                                                               
         SR    RF,RF               LENGTH OF VIEWING TYPE                       
         LR    R2,R1                                                            
VALVW10  CLI   0(R1),C' '                                                       
         BNH   VALVW20             END OF EXPRESSION                            
         CLI   0(R1),C'/'                                                       
         BE    VALVW20             END OF ONE VIEWING TYPE                      
         LA    RF,1(RF)                                                         
         LA    R1,1(R1)                                                         
         B     VALVW10                                                          
*                                                                               
VALVW20  L     RE,=A(VOPTVTYP)                                                  
         A     RE,PGNR2                                                         
         USING VIEWTYPD,RE                                                      
         BCTR  RF,0                LENGTH-1                                     
VALVW25  CLI   0(RE),X'FF'                                                      
         BE    OVNEXIT             INVALID VIEWING TYPE REQUESTED               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   VIEWTYP(0),0(R2)    COMPARE TO VALID VIEWING TYPES               
         BNE   VALVW27                                                          
*                                                                               
         LA    R5,PDVIEWS          APPEND TO LIST OF REQUESTED VTYPS            
         OC    PDVIEWS,PDVIEWS                                                  
         BZ    VALVWP10            LIST IS EMPTY. JUST ADD TO IT                
VALVWP05 CLI   0(R5),X'FF'         END OF EXISTENT LIST?                        
         BE    VALVWP10            YES. GO ADD NEW VTYP                         
         CLC   VIEWINT,0(R5)                                                    
         BE    VALVW30             THIS VTYP IS ALREADY ON THE LIST             
         LA    R5,1(R5)                                                         
         B     VALVWP05                                                         
VALVWP10 LA    R0,PDVIEWS+L'PDVIEWS                                             
         CR    R5,R0                                                            
         BL    *+6                                                              
         DC    H'0'                PDVIEWS OVERFLOW                             
         MVC   0(1,R5),VIEWINT     SAVE INTERNAL VIEWING TYPE                   
         MVI   1(R5),X'FF'         ADD END OF LIST INDICATOR                    
         B     VALVW30                                                          
*                                                                               
VALVW27  LA    RE,VIEWTYPL(RE)                                                  
         B     VALVW25                                                          
         DROP  RE                                                               
*                                                                               
VALVW30  CLI   0(R1),C'/'                                                       
         BNE   VALVW40             FINISHED ALL VIEWING TYPES                   
VALVW35  LA    R1,1(R1)                                                         
         LR    R2,R1                                                            
         SR    RF,RF                                                            
         B     VALVW10                                                          
*                                                                               
VALVW40  DS    0H                                                               
                                                                                
         B     OVEQXIT                                                          
         EJECT                                                                  
*                                                                               
*                                                                               
*-GET DAYPART CODE (FOR PIV FILTERING)                                          
* RETURN ONE BYTE DAYPART CODE IN DPTOPT                                        
* THIS SUPPORTS 2-CHAR DAYPART CODES                                            
* EVERY 2-CHAR ALPHA DAYPART HAS A 1 BYTE EQUIVALENT RETURNED IN DPTOPT         
*                                                                               
GETDPT   XC    DPTOPT,DPTOPT                                                    
*                                                                               
         CLI   1(R4),1             1 OR 2 CHAR DAYPART                          
         BE    GETDP05                                                          
         CLI   1(R4),2                                                          
         BE    GETDP30                                                          
         B     OVNEXIT                                                          
*                                                                               
GETDP05  L     RE,=A(DAYPARTS)     USE TABLE                                    
         A     RE,PGNR2            TO VALIDATE 1-CHAR DAYPART                   
GETDP10  CLI   0(RE),X'FF'                                                      
         BE    GETDP30                                                          
         CLC   0(1,RE),22(R4)                                                   
         BNE   GETDP20                                                          
         MVC   DPTOPT,0(RE)                                                     
         B     OVEQXIT                                                          
GETDP20  LA    RE,10(RE)                                                        
         B     GETDP10                                                          
*                                                                               
GETDP30  LA    R3,KEY              USE SFM DAYPART RECORDS                      
         USING NDPTHDR,R3          TO VALIDATE 2-CHAR DAYPARTS                  
         XC    KEY,KEY                                                          
         MVC   NDPTKTYP,=XL2'0D07'                                              
         MVC   NDPTAGM,PDBAGYMD                                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=CL8'DMRDHI'),=C'UNTDIR  ',KEY,KEY,0             
         B     GETDPT50                                                         
*                                                                               
GETDPT40 MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=CL8'DMRSEQ'),=C'UNTDIR  ',KEY,KEY,0             
*                                                                               
GETDPT50 CLC   KEY(5),KEYSAVE      AGY LEVEL/CLIENT LEVEL                       
         BNE   OVNEXIT                                                          
*                                                                               
         MVC   WORK(2),22(R4)                                                   
         OC    WORK(2),=C'  '                                                   
         CLC   NDPTDPTA,WORK       MATCH ON CODE?                               
         BNE   GETDPT40                                                         
         MVC   DPTOPT,NDPTDPTE     PASS BACK DAYPART EQUATE                     
         B     OVEQXIT                                                          
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*-SET SVT'S (SOURCE + VIEWING TYPE COMBO)                                       
*                                                                               
VALSVT   DS    0X                                                               
         OC    PDSVTS,PDSVTS       DON'T REJECT IF VALIDATING SVT A             
         BNZ   *+14                 SECOND TIME                                 
         OC    PDVIEWS,PDVIEWS     OPTIONS VIEWTYP AND SVT ARE                  
         BNZ   OVNEXIT              MUTUALLY EXCLUSIVE                          
*                                                                               
         LA    R1,22(R4)                                                        
*                                                                               
         SR    RF,RF               LENGTH OF SVT                                
         LR    R2,R1                                                            
VALSV10  CLI   0(R1),C' '                                                       
         BNH   VALSV20             END OF EXPRESSION                            
         CLI   0(R1),C'/'                                                       
         BE    VALSV20             END OF ONE SVT                               
         LA    RF,1(RF)                                                         
         LA    R1,1(R1)                                                         
         B     VALSV10                                                          
*                                                                               
VALSV20  L     RE,=A(VCOLSVT)                                                   
         A     RE,PGNR2                                                         
         USING VCOLSVTD,RE                                                      
         BCTR  RF,0                LENGTH-1                                     
VALSV25  CLI   0(RE),X'FF'                                                      
         BE    OVNEXIT             INVALID SVT REQUESTED                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   VCSVTIN(0),0(R2)    COMPARE TO VALID SVT'S                       
         BE    VALSV30                                                          
         LA    RE,VCOLSVTL(RE)                                                  
         B     VALSV25                                                          
*                                                                               
VALSV30  LA    R5,PDSVTS           APPEND TO LIST OF REQUESTED SVTS             
         OC    PDSVTS,PDSVTS                                                    
         BZ    VALSV40             LIST IS EMPTY. JUST ADD TO IT                
VALSV35  CLI   0(R5),X'FF'         END OF EXISTENT LIST?                        
         BE    VALSV40             YES. GO ADD NEW VTYP                         
         CLC   VCSVTSRC,0(R5)      CK IF SOURCE+VIEWING TYPE COMBO              
         BNE   *+14                IS ALREADY IN THE LIST                       
         CLC   VCSVTVTI,5(R5)                                                   
         BE    VALSV50                                                          
         LA    R5,6(R5)                                                         
         B     VALSV35                                                          
VALSV40  LA    R0,PDSVTS+L'PDSVTS-1                                             
         CR    R5,R0                                                            
         BNL   OVNEXIT             TOO MANY SVT'S                               
         MVC   0(5,R5),VCSVTSRC    SAVE SOURCE TO LIST                          
         MVC   5(1,R5),VCSVTVTI    SAVE INTERNAL VIEWING TYPE TO LIST           
         MVI   6(R5),X'FF'         ADD END OF LIST INDICATOR                    
*                                                                               
VALSV50  LA    R5,PDVIEWS          APPEND TO LIST OF REQUESTED                  
         OC    PDVIEWS,PDVIEWS     VIEIWNG TYPES                                
         BZ    VALSV60             LIST IS EMPTY. JUST ADD TO IT                
VALSV55  CLI   0(R5),X'FF'         END OF EXISTENT LIST?                        
         BE    VALSV60             YES. GO ADD NEW VTYP                         
         CLC   VCSVTVTI,0(R5)                                                   
         BE    VALSV80             ALREADY IN LIST                              
         LA    R5,1(R5)                                                         
         B     VALSV55                                                          
VALSV60  LA    R0,PDVIEWS+L'PDVIEWS-1                                           
         CR    R5,R0                                                            
         BNL   OVNEXIT             TOO MANY VIEIWNG TYPES                       
         MVC   0(1,R5),VCSVTVTI    SAVE INTERNAL VIEWING TYPE TO LIST           
         MVI   1(R5),X'FF'         ADD END OF LIST INDICATOR                    
*                                                                               
         DROP  RE                                                               
*                                                                               
VALSV80  CLI   0(R1),C'/'                                                       
         BNE   VALSV90             FINISHED ALL SVT'S                           
VALSV85  LA    R1,1(R1)                                                         
         LR    R2,R1                                                            
         SR    RF,RF                                                            
         B     VALSV10                                                          
*                                                                               
VALSV90  DS    0H                                                               
                                                                                
         B     OVEQXIT                                                          
         EJECT                                                                  
*                                                                               
*                                                                               
OVNEXIT  LTR   RB,RB                                                            
         B     OVXIT                                                            
*                                                                               
OVEQXIT  CR    RB,RB                                                            
         B     OVXIT                                                            
*                                                                               
OVXIT    XMOD1 1                                                                
*                                                                               
*RELO2    DS    A                                                               
RELO2C   DC    A(*)                                                             
*                                                                               
         DROP  R8,R9,RA,RB,RC                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
VOPTVTYP DC    C'L  ',AL1(DBXLLQ)                                               
         DC    C'SD ',AL1(DBXLL1Q)                                              
         DC    C'7+ ',AL1(DBXLL7Q)                                              
         DC    C'1+ ',AL1(DBXLLF1Q)                                             
         DC    C'2+ ',AL1(DBXLL2Q)                                              
         DC    C'3+ ',AL1(DBXLL3Q)                                              
         DC    C'AL ',AL1(DBXLALVQ)                                             
         DC    C'AS ',AL1(DBXLALSQ)                                             
         DC    C'A7 ',AL1(DBXLAL7Q)                                             
         DC    C'N3 ',AL1(DBXLNL3)                                              
         DC    C'RL ',AL1(DBXLCLQ)                                              
         DC    C'RC ',AL1(DBXLCLCQ)                                             
         DC    C'R3 ',AL1(DBXLCL3Q)                                             
         DC    C'R7 ',AL1(DBXLCL7Q)                                             
         DC    C'OL ',AL1(DBXLOLQ)                                              
         DC    C'OS ',AL1(DBXLOSQ)                                              
         DC    C'O3 ',AL1(DBXLO3Q)                                              
         DC    C'O7 ',AL1(DBXLO7Q)                                              
         DC    C'OCL',AL1(DBXLOCLQ)                                             
         DC    C'OCS',AL1(DBXLOCSQ)                                             
         DC    C'OC3',AL1(DBXLOC3Q)                                             
         DC    C'OC7',AL1(DBXLOC7Q)                                             
         DC    X'FF'                                                            
                                                                                
VPROG    DS    0H                                                               
         DC    CL(L'ETENTRY)'P'                                                 
         DC    CL(L'ETENTRY)'PROG'                                              
         DC    CL(L'ETENTRY)'LP'                                                
         DC    CL(L'ETENTRY)'LPROGRAM'                                          
         DC    CL(L'ETENTRY)'PROGRAM'                                           
         DC    CL(L'ETENTRY)'PROGNAM'                                           
         DC    X'FF'                                                            
         DC    CL(L'ETENTRY)'CPROG'     OTHER PROGRAM IDENTIFIERS               
         DC    CL(L'ETENTRY)'NTI'                                               
         DC    CL(L'ETENTRY)'NTILONG'                                           
         DC    CL(L'ETENTRY)'EP'                                                
         DC    CL(L'ETENTRY)'EPISODE'                                           
         DC    CL(L'ETENTRY)'LTRACK'                                            
         DC    CL(L'ETENTRY)'TLNUM'                                             
         DC    CL(L'ETENTRY)'TRACK'                                             
         DC    CL(L'ETENTRY)'TRNUM'                                             
         DC    X'FFFF'                                                          
*                                                                               
VNETLST  DS    0H                                                               
         DC    CL(L'ETENTRY)'N'                                                 
         DC    CL(L'ETENTRY)'NET'                                               
         DC    X'FF'                                                            
*                                                                               
VCOLF    DS    0H                                                               
         DC    X'01',C'***O'                                                    
         DC    X'01',C'ORIG'                                                    
         DC    X'02',C'***R'                                                    
         DC    X'02',C'REPE'                                                    
         DC    X'FF'                                                            
VCOLNTTB DS    0H                                                               
         DC    X'01',C'N1',C'S1'                                                
         DC    X'02',C'N2',C'S2'                                                
         DC    X'03',C'N3',C'S3'                                                
         DC    X'04',C'N4',C'S4'                                                
         DC    X'05',C'N5',C'S5'                                                
         DC    X'06',C'N6',C'S6'                                                
         DC    X'07',C'N7',C'S7'                                                
         DC    X'08',C'N8',C'S8'                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
VCOLBKTB DS    0H                                                               
         DC    X'01',C'B1'                                                      
         DC    X'02',C'B2'                                                      
         DC    X'03',C'B3'                                                      
         DC    X'04',C'B4'                                                      
         DC    X'05',C'B5'                                                      
         DC    X'06',C'B6'                                                      
         DC    X'07',C'B7'                                                      
         DC    X'08',C'B8'                                                      
         DC    X'FF'                                                            
         SPACE 2                                                                
VCOLDYTB DS    0H                                                               
         DC    X'00',C'M-F'                                                     
         DC    X'01',C'MON'                                                     
         DC    X'02',C'TUE'                                                     
         DC    X'03',C'WED'                                                     
         DC    X'04',C'THU'                                                     
         DC    X'05',C'FRI'                                                     
         DC    X'06',C'SAT'                                                     
         DC    X'07',C'SUN'                                                     
         DC    X'08',C'M-S'                                                     
         DC    X'FF'                                                            
         SPACE 2                                                                
VCOLVTTB DS    0CL3                                                             
         DC    AL1(SRCLIVE),C'L '     LIVE                                      
         DC    AL1(SRCLIVSD),C'SD'    LIVE+SD                                   
         DC    AL1(SRCLIVE7),C'7+'    LIVE+7                                    
         DC    AL1(SRCLIVE1),C'1+'    LIVE+1                                    
         DC    AL1(SRCLIVE2),C'2+'    LIVE+2                                    
         DC    AL1(SRCLIVE3),C'3+'    LIVE+3                                    
         DC    AL1(SRCALV),C'AL'      LIVE    FROM ACM TAPES                    
         DC    AL1(SRCALS),C'AS'      LIVE+SD FROM ACM TAPES                    
         DC    AL1(SRCAL7),C'A7'      LIVE+7  FROM ACM TAPES                    
         DC    AL1(SRCNTIL3),C'N3'    LIVE+3 NTI FILE                           
         DC    AL1(SRCLCLQ),C'RL'     LIVE (COMSCORE)                           
         DC    AL1(SRCLCLCQ),C'RC'    LIVE COMMERCIAL (COMSCORE)                
         DC    AL1(SRCLCL3Q),C'R3'    LIVE+3 COMMERCIAL (COMSCORE)              
         DC    AL1(SRCLCL7Q),C'R7'    LIVE+7 COMMERCIAL (COMSCORE)              
         DC    X'FF'                                                            
         SPACE 2                                                                
VCOLMTTB DS    0CL2                   MINUTE TYPE TABLE                         
         DC    AL1(OPTMCOMQ),C'C'     COMMERCIAL                                
         DC    AL1(OPTMPROQ),C'R'     PROMO                                     
         DC    AL1(OPTMPSAQ),C'S'     PSA                                       
         DC    AL1(OPTMCOMQ+OPTMPROQ+OPTMPSAQ),C'Z'  COMM+PROMO+PSA             
         DC    X'FF'                                                            
         SPACE 2                                                                
VCOLSVT  DC    C'PL',X'04',CL5'NTI',AL1(SRCLIVE),AL1(DBXLLQ)                    
         DC    C'PS',X'04',CL5'NTI',AL1(SRCLIVSD),AL1(DBXLL1Q)                  
         DC    C'P7',X'04',CL5'NTI',AL1(SRCLIVE7),AL1(DBXLL7Q)                  
         DC    C'AL',X'04',CL5'NTI',AL1(SRCALV),AL1(DBXLALVQ)                   
         DC    C'AS',X'04',CL5'NTI',AL1(SRCALS),AL1(DBXLALSQ)                   
         DC    C'P1',X'04',CL5'NTI',AL1(SRCLIVE1),AL1(DBXLLF1Q)                 
         DC    C'P2',X'04',CL5'NTI',AL1(SRCLIVE2),AL1(DBXLL2Q)                  
         DC    C'P3',X'04',CL5'NTI',AL1(SRCLIVE3),AL1(DBXLL3Q)                  
         DC    C'A7',X'04',CL5'NTI',AL1(SRCAL7),AL1(DBXLAL7Q)                   
         DC    C'N3',X'04',CL5'NTI',AL1(SRCNTIL3),AL1(DBXLNL3)                  
         DC    C'CL',X'37',CL5'ACM',AL1(SRCLIVE),AL1(DBXLLQ)                    
         DC    C'CS',X'37',CL5'ACM',AL1(SRCLIVSD),AL1(DBXLL1Q)                  
         DC    C'C1',X'37',CL5'ACM',AL1(SRCLIVE1),AL1(DBXLLF1Q)                 
         DC    C'C2',X'37',CL5'ACM',AL1(SRCLIVE2),AL1(DBXLL2Q)                  
         DC    C'C3',X'37',CL5'ACM',AL1(SRCLIVE3),AL1(DBXLL3Q)                  
         DC    C'C7',X'37',CL5'ACM',AL1(SRCLIVE7),AL1(DBXLL7Q)                  
         DC    C'ML',X'44',CL5'MXM',AL1(SRCLIVE),AL1(DBXLLQ)                    
         DC    C'MS',X'44',CL5'MXM',AL1(SRCLIVSD),AL1(DBXLL1Q)                  
         DC    C'M1',X'44',CL5'MXM',AL1(SRCLIVE1),AL1(DBXLLF1Q)                 
         DC    C'M2',X'44',CL5'MXM',AL1(SRCLIVE2),AL1(DBXLL2Q)                  
         DC    C'M3',X'44',CL5'MXM',AL1(SRCLIVE3),AL1(DBXLL3Q)                  
         DC    C'M7',X'44',CL5'MXM',AL1(SRCLIVE7),AL1(DBXLL7Q)                  
         DC    C'RL',X'45',CL5'COM',AL1(SRCLCLQ),AL1(DBXLCLQ)                   
         DC    C'RC',X'45',CL5'COM',AL1(SRCLCLCQ),AL1(DBXLCLCQ)                 
         DC    C'R3',X'45',CL5'COM',AL1(SRCLCL3Q),AL1(DBXLCL3Q)                 
         DC    C'R7',X'45',CL5'COM',AL1(SRCLCL7Q),AL1(DBXLCL7Q)                 
*                                                                               
         DC    C'OL',X'04',CL5'OOH',AL1(SRCLOLQ),AL1(DBXLLQ)                    
         DC    C'OS',X'04',CL5'OOH',AL1(SRCLOSQ),AL1(DBXLL1Q)                   
         DC    C'O3',X'04',CL5'OOH',AL1(SRCLO3Q),AL1(DBXLL3Q)                   
         DC    C'O7',X'04',CL5'OOH',AL1(SRCLO7Q),AL1(DBXLL7Q)                   
         DC    C'OCL',X'37',CL5'OOHC',AL1(SRCLOCLQ),AL1(DBXLLQ)                 
         DC    C'OCS',X'37',CL5'OOHC',AL1(SRCLOCSQ),AL1(DBXLL1Q)                
         DC    C'OC3',X'37',CL5'OOHC',AL1(SRCLOC3Q),AL1(DBXLL3Q)                
         DC    C'OC7',X'37',CL5'OOHC',AL1(SRCLOC7Q),AL1(DBXLL7Q)                
         DC    X'FF'                                                            
VCOLDPTB DS    0H                                                               
         DC    C'E'                EARLY                                        
         DC    C'D'                DAY                                          
         DC    C'P'                PRIME                                        
         DC    C'L'                LATE                                         
         DC    C'S'                SATURDAY AM                                  
         DC    C'W'                WEEKEND                                      
         DC    C'N'                NEWS                                         
         DC    X'FF'                                                            
         SPACE 2                                                                
VCOLSCTB DS    0H                                                               
         DC    X'01',C'NAD  '                                                   
         DC    X'02',C'NAD-T'                                                   
         DC    X'03',C'NAD-D'                                                   
         DC    X'04',C'NTI  '                                                   
         DC    X'04',C'NTI-N'                                                   
         DC    X'05',C'NTI-T'                                                   
         DC    X'06',C'PIV  '                                                   
         DC    X'07',C'NSI-W'                                                   
         DC    X'08',C'MPA  '                                                   
         DC    X'08',C'MPA-N'                                                   
         DC    X'09',C'EMI  '                                                   
         DC    X'0A',C'NMI  '                                                   
         DC    X'0B',C'TP   '                                                   
         DC    X'0B',C'TP-N '                                                   
         DC    X'0C',C'PAV  '                                                   
         DC    X'0C',C'PAV-N'                                                   
         DC    X'0D',C'DPT  '                                                   
         DC    X'0D',C'DPT-N'                                                   
         DC    X'0E',C'MPA-A'                                                   
         DC    X'0F',C'TP-A '                                                   
         DC    X'10',C'PAV-A'                                                   
         DC    X'11',C'DPT-A'                                                   
         DC    X'12',C'CSI  '                                                   
         DC    X'13',C'BBM  '                                                   
         DC    X'14',C'T4   '                                                   
         DC    X'14',C'T4-N '                                                   
         DC    X'15',C'T4-A '                                                   
         DC    X'16',C'SRC  '                                                   
         DC    X'17',C'NHI-T'                                                   
         DC    X'17',C'NHT-H'                                                   
         DC    X'18',C'NHT  '                                                   
         DC    X'19',C'NHT-D'                                                   
         DC    X'1A',C'NHT-T'                                                   
         DC    X'1B',C'NSI  '                                                   
         DC    X'1C',C'ARB  '                                                   
         DC    X'1D',C'NHW-H'                                                   
         DC    X'1E',C'NHW  '                                                   
         DC    X'1F',C'T3   '                                                   
         DC    X'1F',C'T3-C '                                                   
         DC    X'20',C'T3-B '                                                   
         DC    X'21',C'IUN  '                                                   
         DC    X'22',C'TP-M '                                                   
         DC    X'22',C'MFX  '                                                   
         DC    X'23',C'PAV-M'                                                   
         DC    X'24',C'T4 -M'                                                   
         DC    X'25',C'IUN-S'                                                   
         DC    X'26',C'IUN-M'                                                   
         DC    X'27',C'NAW  '                                                   
         DC    X'28',C'NAW-D'                                                   
         DC    X'29',C'NAW-T'                                                   
         DC    X'2A',C'NHW-D'                                                   
         DC    X'2B',C'HWH-T'                                                   
         DC    X'2C',C'SQAD '                                                   
         DC    X'2C',C'SQADH'                                                   
         DC    X'2C',C'SQADR'                                                   
         DC    X'2D',C'TCAR '                                                   
         DC    X'2D',C'WB1  '                                                   
         DC    X'2E',C'RADAR'                                                   
         DC    X'2F',C'NHC-T'                                                   
         DC    X'2F',C'NHC-H'                                                   
         DC    X'30',C'NHC  '                                                   
         DC    X'31',C'NCOS '                                                   
         DC    X'32',C'OPI  '                                                   
         DC    X'33',C'CNAD '                                                   
         DC    X'34',C'CNADT'                                                   
         DC    X'35',C'CNAW '                                                   
         DC    X'36',C'CNAWT'                                                   
         DC    X'37',C'ACM  '                                                   
         DC    X'38',C'HPM  '                                                   
         DC    X'39',C'HPM-D'                                                   
         DC    X'3A',C'HPM-H'                                                   
         DC    X'3B',C'HPM-T'                                                   
         DC    X'3C',C'HPW  '                                                   
         DC    X'3D',C'HPW-D'                                                   
         DC    X'3E',C'HPW-H'                                                   
         DC    X'3F',C'HPW-T'                                                   
         DC    X'40',C'HPC  '                                                   
         DC    X'41',C'HPC-H'                                                   
         DC    X'42',C'HPC-T'                                                   
         DC    X'43',C'ACMWB'                                                   
         DC    X'44',C'MXM  '                                                   
         DC    X'45',C'COM  '                                                   
         DC    X'04',C'OOH  '                                                   
         DC    X'37',C'OOHC '                                                   
         DC    X'FF'                                                            
         EJECT                                                                  
FLIST    DS    0F                                                               
         DC    CL8'USPTFILE'                                                    
         DC    CL8'NSPTDIR'                                                     
         DC    CL8'NSTAFILE'                                                    
         DC    CL8'NCTFILE'                                                     
         DC    CL8'NDEMDIRA'                                                    
         DC    CL8'NL=DEMFA'                                                    
         DC    CL8'NDEMDIRR'                                                    
         DC    CL8'NL=DEMFR'                                                    
         DC    CL8'NDEMDIRN'                                                    
         DC    CL8'NL=DEMFN'                                                    
         DC    CL8'NPAVDIR '                                                    
         DC    CL8'NL=PAVFL'                                                    
         DC    CL8'X'                                                           
         SPACE 2                                                                
RFLIST   DS    0F                                                               
         DC    CL8'NPAVDIR '                                                    
         DC    CL8'NL=PAVFL'                                                    
         DC    CL8'X'                                                           
         SPACE 2                                                                
FLWNAD   DC    X'01',AL1(1,2,3,0)       NAD QUARTERS                            
         DC    X'02',AL1(4,5,6,0)                                               
         DC    X'03',AL1(7,8,9,0)                                               
         DC    X'04',AL1(10,11,12,0)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
*   CONSTANTS FOR DAY EDIT                                                      
*   BE CAREFUL SEQUENCE IS IMPORTANT BECAUSE WE ALLOW INPUT                     
*   TRUNCATION.  M=MONDAY NOT MTWTF                                             
*                                                                               
DAYLIST  DC    C'YYYYYYYYY',X'00'       DUMMY ENTRY                             
         DC    C'MONDAY   ',X'40'         DALL FOR WHOLE WEEK                   
         DC    C'MTWTF    ',X'FC'       INDIVIDUAL WEEKDAYS, USE                
         DC    C'TUESDAY  ',X'20'                                               
         DC    C'WEDNESDAY',X'10'                                               
         DC    C'THURSDAY ',X'08'                                               
         DC    C'FRIDAY   ',X'04'                                               
         DC    C'SATURDAY ',X'02'                                               
         DC    C'SUNDAY   ',X'01'                                               
         DC    C'YYYYYYYYY',X'00'       DUMMY ENTRY                             
         DC    C'VARIABLE ',X'FF'                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
*                                                                               
DYTMMAC  DC    C'EARLY ',X'7C02BC03E80000000000',X'00'  M-F,7-10A               
         DC    C'DAY   ',X'7C03E8065E0000000000',X'00'  M-F,10A-430P            
         DC    C'PRIME ',X'FF07D008FC01076C07D0',X'05'  M-S,8-11P               
*                                                       SUN,7-8P                
         DC    C'LATE  ',X'7C091A0A8C0000000000',X'00'  M-F,1130P-3A            
         DC    C'SATAM ',X'02032005140000000000',X'00'  SAT,8A-1P               
         DC    C'WKEND ',X'0202BC076C0102BC076C',X'05'  SAT,7A-7P               
*                                                       SUN,7A-7P               
         DC    C'NEWS  ',X'FF0708076C0000000000',X'00'  ALL,6-7P                
         DC    X'FF'                                                            
*                                                                               
* THIS TABLE CONTAINS END TIMES AT THE MINUTE LEVEL AND IS NEEDED               
* FOR SOURCE MXM. FOR EXAMPLE IF THE END TIME OF 11:00 PM WAS 2300              
* IN TABLE DYTMMAC, IT IS 2259 IN THIS TABLE. THIS CHANGE WAS NEEDED            
* SO WE COULD EXCLUDE THE 11:00 PM MINUTE FOR MXM.                              
*                                                                               
DYTMMACM DC    C'EARLY ',X'7C02BC03BF0000000000',X'00'  M-F,7-959A              
         DC    C'DAY   ',X'7C03E8065D0000000000',X'00'  M-F,10A-429P            
         DC    C'PRIME ',X'FF07D008D301076C07A7',X'05'  M-S,8-1059P             
*                                                       SUN,7-759P              
         DC    C'LATE  ',X'7C091A0A630000000000',X'00'  M-F,1130P-259A          
         DC    C'SATAM ',X'02032004EB0000000000',X'00'  SAT,8A-1259P            
         DC    C'WKEND ',X'0202BC07430102BC0743',X'05'  SAT,7A-659P             
*                                                       SUN,7A-659P             
         DC    C'NEWS  ',X'FF070807430000000000',X'00'  ALL,6-659P              
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        BOOKTAB                                                      *         
*                  BYTE 0-4 = EXTERNAL SOURCE CODE                    *         
*                  BYTE 5   = INTERNAL SOURCE                         *         
*                             N=NIELSEN, ARBITRON, MEDIAFAX           *         
*                  BYTE 6   = INTERNAL MEDIA                          *         
*                  BYTE 7-9 = FILE                                    *         
*                  BYTE 10  = MEDIA TYPE                              *         
*                  BYTE 11  = ROUTINE DISPLACEMENT                    *         
***********************************************************************         
*                                                                               
BOOKTAB  DC    CL5'NSI  ',CL1'N',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
BOOKTABL EQU   *-BOOKTAB                                                        
         DC    CL5'OTP  ',CL1'N',CL1'O',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'ARB  ',CL1'A',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'MFX  ',CL1'M',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'CSI  ',CL1'N',CL1'C',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'BBM  ',CL1'A',CL1'C',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'T3   ',CL1'N',CL1'C',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'T3 -C',CL1'N',CL1'C',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'T3 -B',CL1'A',CL1'C',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'CCV  ',CL1'N',CL1'U',CL3'CTP',CL1'T',XL1'00'                 
         DC    CL5'FUS  ',CL1'F',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'RADAR',CL1'R',CL1'R',CL3'TP ',CL1'R',XL1'00'                 
         DC    CL5'SRC  ',CL1'S',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'NSI-W',CL1'N',CL1'W',CL3'WTP',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'CNAD ',CL1'N',CL1'N',CL3'NAD',CL1'N',XL1'00'                 
         DC    CL5'CNADT',CL1'N',CL1'N',CL3'NAD',CL1'N',XL1'00'                 
*                                                                               
         DC    CL5'CNAW ',CL1'N',CL1'N',CL3'NAD',CL1'N',XL1'00'                 
         DC    CL5'CNAWT',CL1'N',CL1'N',CL3'NAD',CL1'N',XL1'00'                 
*                                                                               
         DC    CL5'NAD  ',CL1'N',CL1'N',CL3'NAD',CL1'N',XL1'00'                 
         DC    CL5'NAD-T',CL1'N',CL1'N',CL3'NAD',CL1'N',XL1'00'                 
         DC    CL5'NAD-D',CL1'N',CL1'N',CL3'NAD',CL1'N',XL1'00'                 
*                                                                               
         DC    CL5'NAW  ',CL1'N',CL1'W',CL3'NAD',CL1'N',XL1'00'                 
         DC    CL5'NAW-T',CL1'N',CL1'W',CL3'NAD',CL1'N',XL1'00'                 
         DC    CL5'NAW-D',CL1'N',CL1'W',CL3'NAD',CL1'N',XL1'00'                 
*                                                                               
         DC    CL5'TCAR ',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'WB1  ',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'ACMWB',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'NCOS ',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'OPI  ',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'NTI  ',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'NTI-N',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'NTI-T',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'IAGE ',CL1'N',CL1'N',CL3'NTI',CL1'I',XL1'00'                 
         DC    CL5'IAGA ',CL1'N',CL1'N',CL3'NAD',CL1'I',XL1'00'                 
*                                                                               
         DC    CL5'ACM  ',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'MXM  ',CL1'N',CL1'N',CL3'RLD',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'PIV  ',CL1'N',CL1'V',CL3'EVN',CL1'N',XL1'00'                 
         DC    CL5'SQAD ',CL1'N',CL1'T',CL3'TP ',CL1'N',XL1'00'                 
         DC    CL5'SQADH',CL1'N',CL1'T',CL3'TP ',CL1'N',XL1'00'                 
         DC    CL5'SQADR',CL1'S',CL1'R',CL3'TP ',CL1'N',XL1'00'                 
*                                                                               
BOOKMPA  DC    CL5'MPA  ',CL1'N',CL1'P',CL3'MPA',CL1'T',XL1'00'                 
         DC    CL5'MPA-N',CL1'N',CL1'P',CL3'MPA',CL1'T',XL1'00'                 
*        DC    CL5'MPA-A',CL1'A',CL1'P',CL3'MPA',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'EMI  ',CL1'N',CL1'V',CL3'EVN',CL1'N',XL1'01'                 
         DC    CL5'NMI  ',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'01'                 
*        DC    CL5'EMI-A',CL1'N',CL1'V',CL3'EVN',CL1'N',XL1'01'                 
*        DC    CL5'NMI-A',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'01'                 
*                                                                               
         DC    CL5'TP   ',CL1'N',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'TP -N',CL1'N',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'TP -A',CL1'A',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'TP -M',CL1'M',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'T4   ',CL1'N',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'T4 -N',CL1'N',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'T4 -A',CL1'A',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'T4 -M',CL1'M',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'INV  ',CL1'I',CL1'U',CL3'IUN',CL1'T',XL1'00'                 
         DC    CL5'PAV  ',CL1'N',CL1'T',CL3'PAV',CL1'T',XL1'00'                 
         DC    CL5'PAV-N',CL1'N',CL1'T',CL3'PAV',CL1'T',XL1'00'                 
*        DC    CL5'PAV-A',CL1'A',CL1'T',CL3'PAV',CL1'T',XL1'00'                 
         DC    CL5'PAV-M',CL1'M',CL1'T',CL3'PAV',CL1'T',XL1'00'                 
         DC    CL5'OPA  ',CL1'N',CL1'O',CL3'PAV',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'DPT  ',CL1'N',CL1'D',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'DPT-N',CL1'N',CL1'D',CL3'TP ',CL1'T',XL1'00'                 
*        DC    CL5'DPT-A',CL1'A',CL1'D',CL3'TP ',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'NHI-T',CL1'N',CL1'N',CL3'NAD',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'NHT  ',CL1'N',CL1'N',CL3'NAD',CL1'N',XL1'00'                 
         DC    CL5'NHT-D',CL1'N',CL1'N',CL3'NAD',CL1'N',XL1'00'                 
         DC    CL5'NHT-H',CL1'N',CL1'N',CL3'NAD',CL1'T',XL1'00'                 
         DC    CL5'NHT-T',CL1'N',CL1'N',CL3'NAD',CL1'N',XL1'00'                 
*                                                                               
* SAME AS NHT, BUT FOR DATA NPM-BASED STARTING SEP/07                           
         DC    CL5'HPM  ',CL1'N',CL1'N',CL3'NAD',CL1'N',XL1'00'                 
         DC    CL5'HPM-D',CL1'N',CL1'N',CL3'NAD',CL1'N',XL1'00'                 
         DC    CL5'HPM-H',CL1'N',CL1'N',CL3'NAD',CL1'T',XL1'00'                 
         DC    CL5'HPM-T',CL1'N',CL1'N',CL3'NAD',CL1'N',XL1'00'                 
*                                                                               
         DC    CL5'NHW  ',CL1'N',CL1'W',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'NHW-D',CL1'N',CL1'W',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'NHW-H',CL1'N',CL1'W',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'NHW-T',CL1'N',CL1'W',CL3'NTI',CL1'T',XL1'00'                 
*                                                                               
* SAME AS NHW, BUT FOR DATA NPM-BASED STARTING AUG27/07                         
         DC    CL5'HPW  ',CL1'N',CL1'W',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'HPW-D',CL1'N',CL1'W',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'HPW-H',CL1'N',CL1'W',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'HPW-T',CL1'N',CL1'W',CL3'NTI',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'NHC  ',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'NHC-H',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'NHC-T',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'00'                 
*                                                                               
* SAME AS NHC, BUT FOR DATA NPM-BASED STARTING AUG27/07                         
         DC    CL5'HPC  ',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'HPC-H',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'HPC-T',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'IUN  ',CL1'N',CL1'U',CL3'IUN',CL1'T',XL1'00'                 
         DC    CL5'IUN-S',CL1'N',CL1'U',CL3'IUN',CL1'T',XL1'00'                 
         DC    CL5'IUN-M',CL1'N',CL1'U',CL3'IUN',CL1'T',XL1'00'                 
* NOTE: NHI-T VALIDATES AS NAD BUT READ NTI                                     
* RENTRAK                                                                       
         DC    CL5'REN-T',CL1'R',CL1'T',CL3'NTI',CL1'T',XL1'00'                 
* COMSCORE                                                                      
         DC    CL5'COM  ',CL1'N',CL1'T',CL3'PAV',CL1'N',XL1'00'                 
*                                                                               
         DC    CL5'OOH  ',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'OOHC ',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'00'                 
         DC    X'FF'                                                            
         EJECT                                                                  
* STACK TABLES AND VALUES                                                       
STKTABC  DC    A(STKTAB7),A(STKTAB6),A(STKTAB5),A(STKTAB4)                      
         DC    A(STKTAB3),A(STKTAB2),A(STKTABC1)                                
*                                                                               
STKTABLE DC    A(STKTAB7),A(STKTAB6),A(STKTAB5),A(STKTAB4)                      
         DC    A(STKTAB3),A(STKTAB2),A(STKTAB1)                                 
*                                                                               
STKTAB7  DC    AL1(15),C'PACIFIC',AL1(31),C'ANY-CBL',AL1(32),C'PAY-CBL'         
         DC    AL1(33),C'BAS-CBL',AL1(55),C'ANY6-11',AL1(56),C'ANY1217'         
         DC    AL1(71),C'$30+-NA',AL1(72),C'$30+POM',AL1(73),C'$30+COL'         
         DC    AL1(74),C'$40+-NA',AL1(75),C'$40+POM',AL1(76),C'$40+COL'         
         DC    AL1(05),C'SPANISH',AL1(06),C'ENGLISH',AL1(35),C'PAYCBL+'         
         DC    AL1(36),C'BASCBL+',AL1(38),C'ANYCBL+'                            
         DC    AL1(86),C'$50+/NA',AL1(87),C'$50+POM'                            
         DC    AL1(88),C'$50+COL',AL1(78),C'$50DUAL'                            
         DC    AL1(146),C'OHLMENG',AL1(147),C'OHLOENG'                          
         DC    AL1(174),C'OHCNMEX',AL1(108),C'OR/HISS'                          
         DC    AL1(109),C'OR/HISB',AL1(110),C'OR/HISE'                          
         DC    AL1(89),C'OR/HISP',AL1(90),C'OR/NHSP'                            
         DC    AL1(29),C'PAYWCBL',AL1(30),C'BASWCBL'                            
         DC    X'FF'                                                            
STKTAB6  DC    AL1(11),C'N-EAST',AL1(12),C'E-CENT',AL1(13),C'W-CENT'            
         DC    AL1(123),C'CS-C+D',AL1(34),C'NO-CBL',AL1(43),C'HHS-3+'           
         DC    AL1(44),C'HHS-4+',AL1(51),C'ANY18-',AL1(52),C'ANY12-'            
         DC    AL1(07),C'BILING',AL1(37),C'BRDCST',AL1(84),C'$60-75'            
         DC    AL1(135),C'OHTCEN',AL1(138),C'OHTPAC',AL1(143),C'OHLSE='         
         DC    AL1(144),C'OHLOSP',AL1(145),C'OHLMSP',AL1(173),C'OHCMEX'         
         DC    AL1(60),C'$125K+',AL1(148),C'RENT/H'                             
         DC    X'FF'                                                            
STKTAB5  DC    AL1(234),C'SPACE',AL1(235),C'INDEX',AL1(14),C'SOUTH'             
         DC    AL1(41),C'HHS-1',AL1(42),C'HHS-2',AL1(53),C'ANY6-'               
         DC    AL1(54),C'ANY3-',AL1(61),C'$15M+',AL1(62),C'$20M+'               
         DC    AL1(63),C'$30M+',AL1(64),C'$40M+',AL1(65),C'$50M+'               
         DC    AL1(66),C'$60M+',AL1(67),C'$30M-',AL1(85),C'$75M+'               
         DC    AL1(241),C'SHARE',AL1(250),C'AVAIL',C'E',C'E-RTG'                
         DC    C'C',C'C-RTG'                                                    
         DC    AL1(134),C'OHTNE',AL1(136),C'OHTSE',AL1(137),C'OHTSW'            
         DC    AL1(149),C'OWN/H'                                                
         DC    X'FF'                                                            
STKTAB4  DC    AL1(236),C'DIFF',AL1(3),C'DIF/',AL1(21),C'CS-A'                  
         DC    AL1(22),C'CS-B',AL1(240),C'DEMO'                                 
         DC    X'FF'                                                            
STKTAB3  DC    AL1(1),C'USA',C'T',C'IMP',C'P',C'PUT'                            
         DC    C'R',C'RTG',C'S',C'SHR',C'V',C'VPH',C'E',C'ERT'                  
         DC    AL1(107),C'VGO'                                                  
         DC    AL1(101),C'101',AL1(102),C'102',AL1(103),C'103'                  
         DC    AL1(104),C'104',AL1(105),C'105',AL1(106),C'106'                  
         DC    AL1(107),C'107',AL1(108),C'108',AL1(109),C'109'                  
         DC    AL1(110),C'110',AL1(111),C'111',AL1(112),C'112'                  
         DC    AL1(113),C'113',AL1(114),C'114',AL1(115),C'115'                  
         DC    AL1(121),C'121'                                                  
         DC    AL1(122),C'122',AL1(123),C'123',AL1(124),C'124'                  
         DC    AL1(125),C'125',AL1(131),C'131',AL1(132),C'132'                  
         DC    AL1(133),C'133',AL1(134),C'134',AL1(135),C'135'                  
         DC    AL1(136),C'136',AL1(137),C'137',AL1(138),C'138'                  
         DC    AL1(141),C'141',AL1(142),C'142',AL1(143),C'143'                  
         DC    AL1(144),C'144',AL1(145),C'145',AL1(146),C'146'                  
         DC    AL1(147),C'147',AL1(148),C'148',AL1(149),C'149'                  
         DC    AL1(151),C'151',AL1(152),C'152',AL1(153),C'153'                  
         DC    AL1(154),C'154',AL1(156),C'156',AL1(157),C'157'                  
         DC    AL1(158),C'158',AL1(159),C'159',AL1(161),C'161'                  
         DC    AL1(162),C'162',AL1(163),C'163',AL1(164),C'164'                  
         DC    AL1(165),C'165',AL1(166),C'166',AL1(167),C'167'                  
         DC    AL1(168),C'168',AL1(169),C'169',AL1(167),C'PMG'                  
         DC    AL1(168),C'FMG',AL1(169),C'AMG',AL1(242),C'HPT'                  
         DC    AL1(173),C'173',AL1(174),C'174'                                  
         DC    X'FF'                                                            
STKTAB2  DC    AL1(11),C'11',AL1(12),C'12',AL1(13),C'13'                        
         DC    AL1(14),C'14',AL1(15),C'15',AL1(21),C'21'                        
         DC    AL1(22),C'22',AL1(23),C'23'                                      
         DC    AL1(29),C'29',AL1(30),C'30',AL1(31),C'31'                        
         DC    AL1(32),C'32',AL1(33),C'33',AL1(34),C'34'                        
         DC    AL1(35),C'35',AL1(36),C'36',AL1(37),C'37'                        
         DC    AL1(38),C'38',AL1(40),C'40'                                      
         DC    AL1(41),C'41',AL1(42),C'42',AL1(43),C'43'                        
         DC    AL1(44),C'44',AL1(51),C'51',AL1(52),C'52'                        
         DC    AL1(53),C'53',AL1(54),C'54',AL1(55),C'55'                        
         DC    AL1(56),C'56',AL1(60),C'60'                                      
         DC    AL1(61),C'61',AL1(62),C'62'                                      
         DC    AL1(63),C'63',AL1(64),C'64',AL1(65),C'65'                        
         DC    AL1(66),C'66',AL1(67),C'67',AL1(71),C'71'                        
         DC    AL1(72),C'72',AL1(73),C'73',AL1(74),C'74'                        
         DC    AL1(75),C'75',AL1(76),C'76',AL1(77),C'77'                        
         DC    AL1(78),C'78'                                                    
         DC    AL1(80),C'80',AL1(81),C'81',AL1(82),C'82'                        
         DC    AL1(83),C'83',AL1(85),C'85',AL1(87),C'87'                        
         DC    AL1(88),C'88',AL1(89),C'89',AL1(90),C'90'                        
         DC    AL1(91),C'91',AL1(92),C'92',AL1(93),C'93'                        
*                                                                               
         DC    AL1(16),C'16',AL1(17),C'17',AL1(45),C'45'                        
         DC    X'FF'                                                            
STKTAB1  DC    C'TI',C'PP',C'RR',C'SS',C'VV',C'DD',C'UU',C'CC'                  
         DC    C'QQ',C'OO',C'LL',C'MM',C'NN',C'AA',C'XT',C'EE'                  
         DC    C'YY',C'ZZ'                                                      
         DC    X'01',C'1',X'05',C'5',X'06',C'6',X'07',C'7'                      
         DC    X'FF'                                                            
*        SPECIAL TABLE FOR CANADA                                               
STKTABC1 DC    C'II',C'PP',C'RR',C'SS',C'VV',C'DD',C'UU'                        
         DC    C'QQ',C'OO',C'LL',C'MM',C'NN',C'AA',C'TT',C'EE'                  
         DC    X'01',C'1',X'05',C'5',X'06',C'6',X'07',C'7'                      
         DC    X'FF'                                                            
         EJECT                                                                  
HUTTABLE DC    AL1(3),C'Q1/ ',C'01150316'        QUARTERS                       
         DC    AL1(3),C'Q2/ ',C'04150616'                                       
         DC    AL1(3),C'Q3/ ',C'07150911'                                       
         DC    AL1(3),C'Q4/ ',C'09111216'                                       
*                                                                               
         DC    AL1(3),C'M1/ ',C'0115    '        MONTHS                         
         DC    AL1(3),C'M2/ ',C'0215    '                                       
         DC    AL1(3),C'M3/ ',C'0315    '                                       
         DC    AL1(3),C'M4/ ',C'0415    '                                       
         DC    AL1(3),C'M5/ ',C'0515    '                                       
         DC    AL1(3),C'M6/ ',C'0615    '                                       
         DC    AL1(3),C'M7/ ',C'0715    '                                       
         DC    AL1(3),C'M8/ ',C'0815    '                                       
         DC    AL1(3),C'M9/ ',C'0915    '                                       
         DC    AL1(4),C'M10/',C'1015    '                                       
         DC    AL1(4),C'M11/',C'1115    '                                       
         DC    AL1(4),C'M12/',C'1215    '                                       
*                                                                               
         DC    AL1(3),C'JAN ',C'0115    '        MONTHS                         
         DC    AL1(3),C'FEB ',C'0215    '                                       
         DC    AL1(3),C'MAR ',C'0315    '                                       
         DC    AL1(3),C'APR ',C'0415    '                                       
         DC    AL1(3),C'MAY ',C'0515    '                                       
         DC    AL1(3),C'JUN ',C'0615    '                                       
         DC    AL1(3),C'JUL ',C'0715    '                                       
         DC    AL1(3),C'AUG ',C'0815    '                                       
         DC    AL1(3),C'SEP ',C'0915    '                                       
         DC    AL1(3),C'OCT ',C'1015    '                                       
         DC    AL1(3),C'NOV ',C'1115    '                                       
         DC    AL1(3),C'DEC ',C'1215    '                                       
*                                                                               
         DC    AL1(3),C'JAN/',C'0115    '        MONTHS                         
         DC    AL1(3),C'FEB/',C'0215    '                                       
         DC    AL1(3),C'MAR/',C'0315    '                                       
         DC    AL1(3),C'APR/',C'0415    '                                       
         DC    AL1(3),C'MAY/',C'0515    '                                       
         DC    AL1(3),C'JUN/',C'0615    '                                       
         DC    AL1(3),C'JUL/',C'0715    '                                       
         DC    AL1(3),C'AUG/',C'0815    '                                       
         DC    AL1(3),C'SEP/',C'0915    '                                       
         DC    AL1(3),C'OCT/',C'1015    '                                       
         DC    AL1(3),C'NOV/',C'1115    '                                       
         DC    AL1(3),C'DEC/',C'1215    '                                       
*                                                                               
         DC    AL1(2),C'1/  ',C'0115    '        MONTHS                         
         DC    AL1(2),C'2/  ',C'0215    '                                       
         DC    AL1(2),C'3/  ',C'0315    '                                       
         DC    AL1(2),C'4/  ',C'0415    '                                       
         DC    AL1(2),C'5/  ',C'0515    '                                       
         DC    AL1(2),C'6/  ',C'0615    '                                       
         DC    AL1(2),C'7/  ',C'0715    '                                       
         DC    AL1(2),C'8/  ',C'0815    '                                       
         DC    AL1(2),C'9/  ',C'0915    '                                       
*                                                                               
         DC    AL1(3),C'01/ ',C'0115    '        MONTHS                         
         DC    AL1(3),C'02/ ',C'0215    '                                       
         DC    AL1(3),C'03/ ',C'0315    '                                       
         DC    AL1(3),C'04/ ',C'0415    '                                       
         DC    AL1(3),C'05/ ',C'0515    '                                       
         DC    AL1(3),C'06/ ',C'0615    '                                       
         DC    AL1(3),C'07/ ',C'0715    '                                       
         DC    AL1(3),C'08/ ',C'0815    '                                       
         DC    AL1(3),C'09/ ',C'0915    '                                       
         DC    AL1(3),C'10/ ',C'1015    '                                       
         DC    AL1(3),C'11/ ',C'1115    '                                       
         DC    AL1(3),C'12/ ',C'1215    '                                       
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*              LIST OF VALID EXPRESSIONS FOR GENDER STACK             *         
***********************************************************************         
*                                                                               
GENTAB   DS    0CL1                                                             
         DC    C'L'                LISTEN                                       
         DC    C'G'                GIRLS                                        
         DC    C'F'                FEMALE                                       
         DC    C'W'                WOMEN                                        
         DC    C'B'                BOYS                                         
         DC    C'M'                MEN                                          
         DC    C'V'                VIEWER                                       
         DC    C'A'                ADULTS                                       
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE NEPODPREC                                                      
         EJECT                                                                  
       ++INCLUDE DENTIBKEQU                                                     
         EJECT                                                                  
       ++INCLUDE SPMGRTAB          MARKET GROUPS ALPHA LOOKUP                   
         EJECT                                                                  
       ++INCLUDE DESTACODE                                                      
         EJECT                                                                  
NEWTAB   DS    0H                  PUT NEW TABLES HERE                          
DBFUNTAB DC    CL5'NAD  '                                                       
         DC    CL5'NAD-D'                                                       
         DC    CL5'NAD-T'                                                       
*                                                                               
         DC    CL5'CNAD '          CABLE NAD                                    
         DC    CL5'CNADT'                                                       
*                                                                               
         DC    CL5'NAW  '                                                       
         DC    CL5'NAW-D'                                                       
         DC    CL5'NAW-T'                                                       
*                                                                               
         DC    CL5'CNAW '          CABLE MOVIE GOER AND NAD WKLY                
         DC    CL5'CNAWT'                                                       
*                                                                               
         DC    CL5'NCOS '                                                       
         DC    CL5'OPI  '                                                       
         DC    CL5'TCAR '                                                       
         DC    CL5'WB1  '                                                       
         DC    CL5'ACMWB'                                                       
         DC    CL5'NTI  '                                                       
         DC    CL5'NTI-N'                                                       
         DC    CL5'NTI-T'                                                       
         DC    CL5'ACM  '                                                       
         DC    CL5'MXM  '                                                       
*                                                                               
         DC    CL5'MPA  '                                                       
*                                                                               
         DC    CL5'NMI  '                                                       
*                                                                               
         DC    CL5'NHI-T'                                                       
*                                                                               
         DC    CL5'NHT  '                                                       
         DC    CL5'NHT-D'                                                       
         DC    CL5'NHT-H'                                                       
         DC    CL5'NHT-T'                                                       
         DC    CL5'HPM  '          NHT FROM NATIONAL SAMPLE                     
         DC    CL5'HPM-D'                                                       
         DC    CL5'HPM-H'                                                       
         DC    CL5'HPM-T'                                                       
* NHT WEEKLY                                                                    
         DC    CL5'NHW  '                                                       
         DC    CL5'NHW-D'                                                       
         DC    CL5'NHW-H'                                                       
         DC    CL5'NHW-T'                                                       
         DC    CL5'HPW  '          NHW FROM NATIONAL SAMPLE                     
         DC    CL5'HPW-D'                                                       
         DC    CL5'HPW-H'                                                       
         DC    CL5'HPW-T'                                                       
* NHT CABLE                                                                     
         DC    CL5'NHC  '                                                       
         DC    CL5'NHC-H'                                                       
         DC    CL5'NHC-T'                                                       
         DC    CL5'HPC  '          NHC FROM NATIONAL SAMPLE                     
         DC    CL5'HPC-H'                                                       
         DC    CL5'HPC-T'                                                       
* RENTRAK TIME PERIOD                                                           
         DC    CL5'REN-T'                                                       
* COMSCORE PROGRAM AVERAGE                                                      
         DC    CL5'COM  '                                                       
*                                                                               
         DC    CL5'OOH  '                                                       
         DC    CL5'OOHC '                                                       
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
DBNADTAB DC    CL7'NAD  PN'                                                     
         DC    CL7'NAD-DDN'                                                     
         DC    CL7'NAD-TTN'                                                     
*                                                                               
         DC    CL7'CNAD PN'                                                     
*                                                                               
         DC    CL7'NAW  PN'                                                     
         DC    CL7'NAW-DDN'                                                     
         DC    CL7'NAW-TTN'                                                     
*                                                                               
         DC    CL7'NHT  PN'                                                     
         DC    CL7'NHT-DDN'                                                     
         DC    CL7'NHT-TTN'                                                     
*                                                                               
         DC    CL7'HPM  PN'                                                     
         DC    CL7'HPM-DDN'                                                     
         DC    CL7'HPM-TTN'                                                     
*                                                                               
         DC    CL7'NHW  PN'                                                     
         DC    CL7'NHW-DDN'                                                     
         DC    CL7'NHW-TTN'                                                     
*                                                                               
         DC    CL7'HPW  PN'                                                     
         DC    CL7'HPW-DDN'                                                     
         DC    CL7'HPW-TTN'                                                     
*                                                                               
         DC    X'FF'                                                            
*                                                                               
* LIST OF VALID PVS FIELDS                                                      
* THE PVS FIELD IS USED TO FILTER ON THE VIEWING TYPES MARKED ON THE            
* PROGRAM RECORDS.                                                              
PVSTAB   DS    0CL6                                                             
         DC    AL1(NPG2VLV)                                                     
         DC    AL1(NPG2VSD)                                                     
         DC    AL1(NPG2VL1)                                                     
         DC    AL1(NPG2VL2)                                                     
         DC    AL1(NPG2VL3)                                                     
         DC    AL1(NPG2VL7)                                                     
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE NEDPTTAB                                                       
         EJECT                                                                  
       ++INCLUDE NEPODWORK                                                      
         EJECT                                                                  
       ++INCLUDE NEPODBOOK                                                      
         EJECT                                                                  
*DDCOREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE NEPODFFD                                                       
         ORG CONTAGH                                                            
       ++INCLUDE NEPODE0D                                                       
         EJECT                                                                  
         PRINT OFF                                                              
*DDGENTWA                                                                       
       ++INCLUDE DDGENTWA                                                       
RSETD    DSECT                                                                  
       ++INCLUDE REGENSET                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENPRG                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENMKG                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
SPEDICTD DSECT                                                                  
       ++INCLUDE SPEDICT                                                        
*CTGENFILE                                                                      
       ++INCLUDE CTGENFILE                                                      
*FAFACTS                                                                        
       ++INCLUDE FAFACTS                                                        
*FATIOB                                                                         
       ++INCLUDE FATIOB                                                         
*DDCOMFACS                                                                      
       ++INCLUDE DDCOMFACS                                                      
*DRGLOBAL                                                                       
       ++INCLUDE DRGLOBAL                                                       
*DDBIGBOX                                                                       
       ++INCLUDE DDBIGBOX                                                       
*DDWIDED                                                                        
       ++INCLUDE DDWIDED                                                        
*DDOFFICED                                                                      
       ++INCLUDE DDOFFICED                                                      
*DDMASTD                                                                        
       ++INCLUDE DDMASTD                                                        
*DRONEBLKHD                                                                     
       ++INCLUDE DRONEBLKHD                                                     
       ++INCLUDE NEGETHUTD                                                      
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
         PRINT ON                                                               
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DEDBEXTRAD                                                     
         EJECT                                                                  
       ++INCLUDE NEGENPRG                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSIR                                                       
         EJECT                                                                  
       ++INCLUDE NEGENDPT                                                       
         EJECT                                                                  
PRECTDST DSECT                                                                  
PRECSRCE DS    CL3                                                              
         DS    CL1                 FOR ALIGNMENT                                
PRECADDR DS    F                                                                
         EJECT                                                                  
ENTRYTBD DSECT                     DSECT IS LOCAL TO NEPODGEN                   
ETENTRY  DS    CL(L'DRENTRYI)                                                   
ETCOLROW DS    CL1                 COL/ROW INDICATOR                            
ETSPARE  DS    XL1                 SPARE                                        
ENTRYTBQ EQU   *-ENTRYTBD                                                       
*                                                                               
VIEWTYPD DSECT                                                                  
VIEWTYP  DS    CL3                 USER INPUT VIEWING TYPE                      
VIEWINT  DS    C                   INTERNAL VIEWING TYPE                        
VIEWTYPL EQU   *-VIEWTYPD                                                       
*                                                                               
VCOLSVTD DSECT                     COULMN FIILTER FOR SOURCE+VIEWTYPE           
VCSVTIN  DS    CL2                 SVT INPUT                                    
VCSVTSHX DS    XL1                 SOURCE HEX EQUIVALENT FROM VCOLSCTB          
VCSVTSRC DS    CL5                 5-CHARACTER SOURCE                           
VCSVTVT  DS    AL1                 VIEWING TYPE FROM VCOLVTTB                   
VCSVTVTI DS    AL1                 INTERNAL VIEWING TYPE                        
VCOLSVTL EQU   *-VCOLSVTD                                                       
*                                                                               
VALNTWKD DSECT                                                                  
* DBEXTENDS                                                                     
SETNFF   DS    CL16                                                             
CAVGEXT  DS    CL9                 COMMERCIAL AVERAGE EXTENT ('CAVG')           
VALNTWKL EQU   *-VALNTWKD                                                       
*                                                                               
BKWORKD  DSECT                                                                  
DUMYFLD  DS    XL208               DUMMY SOURCE/BOOK INPUT FIELD                
BOOKLN   DS    AL4                 LENGTH OF BOOK EXPRESSION                    
BKWORKL  EQU   *-BKWORKD                                                        
*                                                                               
       ++INCLUDE NEGETNUND                                                      
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE DDMONYREQU                                                     
       ++INCLUDE DEDEMTABD                                                      
       ++INCLUDE GEGENTOK                                                       
       ++INCLUDE DEDEMOVALD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'160NEPODGEN  02/28/20'                                      
         END                                                                    
