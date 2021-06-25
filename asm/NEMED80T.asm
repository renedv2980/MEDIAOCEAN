*          DATA SET NEMED80T   AT LEVEL 002 AS OF 05/01/02                      
*          DATA SET NEMED80    AT LEVEL 080 AS OF 09/07/90                      
*PHASE T31E80A,+0                                                               
         TITLE 'T31E80 - DAYPART WEEKLY ANALYSIS'                               
T31E80   CSECT                                                                  
*                                                                               
******************************************************************              
*                                                                               
*                                                                               
*   ORGANIZATION OF W/S                                                         
*                                                                               
* ANETWS1 -> NET DEMO BLOCK   (516 BYTES)                                       
*                                                                               
*            DBLOCK            (256 BYTES)                                      
*                                                                               
*            NETGOAL BLOCK                           (100 BYTES)                
*                                                                               
*            800 BYTE AREA FOR NETGOAL PRODUCT LIST  (800 BYTES)                
*                                                                               
*            WORKING STORAGE                                                    
*                                                                               
*** REGISTER USAGE:                                                             
*   RC,R9 - ADDRESSABILITY FOR COMMON DSECTS                                    
*   RA    - TWA                                                                 
*   R8    - SPOOL                                                               
*   R7    - NETDEMO BLOCK                                                       
*                                                                               
******************************************************************              
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**DPRT**                                                       
         LA    R6,2048(RB)                                                      
         LA    R6,2048(R6)                                                      
         USING T31E80+4096,R6                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1                                                       
         USING NDDEMBLK,R7                                                      
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R2,HEDSPECS                                                      
         ST    R2,SPECS                                                         
         LA    R1,STALIST                                                       
         ST    R1,NBCNVNTI                                                      
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000AE0'    A(DEMOCON)                          
         MVC   DEMOCON,DMCB                                                     
         EJECT                                                                  
*              EDITING REQUEST                                                  
         SPACE 3                                                                
         CLI   MODE,PRINTREP                                                    
         BE    GETS                                                             
         CLI   MODE,VALKEY                                                      
         BNE   XIT                                                              
         MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
         MVI   NBDATA,C'P'         WILL WANT PACKAGE RECORD FIRST               
         L     R7,ANETWS1          SET A(NET DEMO BLOCK)                        
         ST    R7,NBADEM                                                        
         USING NDDEMBLK,R7                                                      
         LA    R3,DBLOCK           SET UP DBLOCK                                
         USING DBLOCK,R3                                                        
         MVC   DBFILE,=C'NTI'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'N'                                                    
         DROP  R3                                                               
         SPACE 1                                                                
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
         LA    R2,SPLCLIH          CLIENT. REQUIRED.                            
         NETGO NVCLI,DMCB,SPLCLIN                                               
         OI    SPLCLINH+6,X'80'    TRANSMIT CLIENT NAME                         
         SPACE 1                                                                
         LA    R2,SPLPROH          PRODUCT. REQUIRED.                           
         NETGO NVPRD,DMCB,SPLPRON                                               
         OI    SPLPRONH+6,X'80'    TRANSMIT PRODUCT NAME.                       
         SPACE 1                                                                
         MVI   FTERMFLG,1          OTHER FIELDS OPTIONAL                        
         LA    R2,SPLESTH                                                       
         NETGO NVESTRNG,DMCB,SPLESTN,NDDEMBLK                                   
         OI    SPLESTNH+6,X'80'    XMIT ESTIMATE NAME                           
         NETGO NVDELHOM,DMCB,NDDEMOS   REMOVE HOMES REQUESTS                    
         SPACE 1                                                                
         LA    R2,SPLNETH                                                       
         NETGO NVNETALL,DMCB,SAVMKT    GET MARKET NUMBER                        
         SPACE 1                                                                
         LA    R2,SPLDPTH                                                       
         NETGO NVDPT,DMCB,SPLDPTN   DAYPART. OPTIONAL.                          
         OI    SPLDPTNH+6,X'80'    TRANSMIT DAYPART NAME                        
         MVC   DPFILT,NBSELDP                                                   
         SPACE 1                                                                
         LA    R2,SPLPAKH          PACKAGE                                      
         NETGO NVPAKLOK,DMCB,SPLPAKN                                            
         OI    SPLPAKNH+6,X'80'                                                 
         SPACE 1                                                                
         LA    R2,SPLSTRTH                                                      
         NETGO NVSTRDAT,DMCB                                                    
         SPACE 1                                                                
*                                  DEFAULT END IS START + 6 DAYS                
         LA    R2,SPLENDH                                                       
         CLI   5(R2),0                                                          
         BNE   ED2                                                              
         MVI   5(R2),8                                                          
         GOTO1 ADDAY,DMCB,USERQSTR,WORK,6                                       
         GOTO1 DATCON,DMCB,(0,WORK),(8,8(R2))                                   
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
ED2      NETGO NVENDDAT,DMCB                                                    
         SPACE 1                                                                
         LA    R2,SPLOPTH                                                       
         BAS   RE,VALIOPT                                                       
*                                                                               
         LA    R2,SPLPFBH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    ED4                                                              
         CLI   8(R2),C'N'                                                       
         BE    ED4                                                              
         CLI   8(R2),C'Y'                                                       
         BE    ED4                                                              
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
*                                                                               
ED4      LA    R2,SPLCOSH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    ED6                                                              
         CLI   8(R2),C'N'                                                       
         BE    ED6                                                              
         CLI   8(R2),C'Y'                                                       
         BE    ED6                                                              
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
*                                                                               
ED6      LA    R2,SPLCLIH                                                       
         B     XMOD                                                             
         SPACE 1                                                                
EDERR    GOTO1 ERREX,DMCB                                                       
         SPACE 1                                                                
XMOD     XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS                                      
         SPACE 3                                                                
VALIOPT  NTR1                                                                   
*                                  THESE ARE SET FOR MCCANN                     
*                                  COULD BE REVERSED FOR OTHER FOLK             
         SPACE 1                                                                
         MVI   INDEXOPT,C'N'       PRESET OPTION DEFAULTS                       
         MVI   WEEKSEP,C'Y'                                                     
         MVI   ACTOPT,C'N'                                                      
         MVI   BOXOPT,C'N'                                                      
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(6,BLOCK),0                                    
         LA    R3,BLOCK                                                         
         ZIC   R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    OPTERR                                                           
         SPACE 1                                                                
VALIOPT2 CLC   12(5,R3),=C'INDEX'                                               
         BNE   VALIOPT4                                                         
         MVC   INDEXOPT,22(R3)                                                  
         B     VALIOPTX                                                         
         SPACE 1                                                                
VALIOPT4 CLC   12(6,R3),=C'ACTUAL'                                              
         BNE   VALIOPT6                                                         
         MVC   ACTOPT,22(R3)                                                    
         B     VALIOPTX                                                         
         SPACE 1                                                                
VALIOPT6 CLC   12(4,R3),=C'WEEK'                                                
         BNE   VALIOPT8                                                         
         CLC   22(3,R3),=C'SEP'                                                 
         BNE   VALIOPT8                                                         
         MVI   WEEKSEP,C'Y'                                                     
         B     VALIOPTX                                                         
         SPACE 1                                                                
VALIOPT8 CLC   12(3,R3),=C'BOX'                                                 
         BNE   VALIOP10                                                         
         MVC   BOXOPT,22(R3)                                                    
         B     VALIOPTX                                                         
         SPACE 1                                                                
VALIOP10 B     OPTERR                                                           
         SPACE 1                                                                
VALIOPTX LA    R3,32(R3)                                                        
         BCT   R4,VALIOPT2                                                      
         B     XIT                                                              
         SPACE 1                                                                
OPTERR   MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
         EJECT                                                                  
*              CONTROL OF REPORT - GOALS AND UNITS                              
         SPACE 3                                                                
GETS     XC    PERTYPE,PERTYPE                                                  
         MVI   PERTYPE,C'W'        USE WEEKS                                    
         MVI   PERTYPE+1,1         USE MONTHS IF TOO MANY WEEKS                 
         LA    R4,MAXWKS                                                        
         ST    R4,NUMWKS           MAX NUMBER OF WEEKS                          
         SPACE 1                                                                
PROCDAT  NETGO NSNETIO,DMCB,NETBLOCK    PROCESS DATES                           
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBVALDAT                                                  
         BE    GETLIST                                                          
         B     PROCDAT                                                          
         SPACE 1                                                                
GETLIST  NETGO NVWKLST,DMCB,NUMWKS,WKLIST,PERTYPE   GET LIST                    
         SPACE 1                                                                
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         XC    NGBLOCK,NGBLOCK     SET UP FOR NETGOAL                           
         L     R1,NUMWKS           CURRENT NUMBER OF WEEKS IN LIST              
         STC   R1,NGNWEEKS         AND A(LIST) PASSED TO NETGOAL                
         LA    R1,WKLIST                                                        
         ST    R1,NGAWLIST                                                      
         LA    R1,NETBLOCK         A(NETBLOCK)                                  
         ST    R1,NGANTBLK                                                      
         LA    R1,PRDLIST          PASS PRODLIST AREA TO NETGOAL                
         ST    R1,NGAPLIST                                                      
         MVI   NGMAXPRD,198                                                     
         LA    R1,GOALPOST                                                      
         ST    R1,NGAHOOK                                                       
         MVC   NGSELMKT,SAVMKT     OPTIONAL 'MARKET'                            
         MVC   NGSELDP,DPFILT      OPTIONAL DAYPART                             
         MVI   NGEXTOPT,1          GIVE ME 1ST DEMO                             
         GOTO1 NBCALLOV,DMCB,0,X'D9000A35'                                      
         L     RF,DMCB             PICK UP ADDRESS OF NETGOAL                   
         GOTO1 (RF),DMCB,NGBLOCK                                                
         SPACE 1                                                                
         MVC   NBACTOPT,ACTOPT     ACTUAL DEMOS OPTIONAL                        
*                                                                               
         MVI   NBESTOPT,C'P'       IF SHOULD INCLUDE PFBS, USE P                
         CLI   SPLPFB,C'Y'                                                      
         BE    GL2                                                              
         MVI   NBESTOPT,C'A'       ELSE GET 'ACTUAL' ESTIMATES                  
*                                                                               
GL2      MVI   NBSELUOP,C'A'       USE ACTUAL SCHEDULE                          
         OI    NBSPLOPT,X'80'      HANDLE SPLIT 30S                             
         MVI   NBDATA,C'U'         GET UNITS                                    
         MVI   NBHUNOPT,C'Y'       HUNDRED OPTION                               
         SPACE 1                                                                
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBPROCUN                                                  
         BNE   GETUNIT2                                                         
         BAS   RE,UNIT                                                          
         B     GETUNIT                                                          
         SPACE 1                                                                
GETUNIT2 CLI   NBMODE,NBREQLST                                                  
         BNE   GETUNIT                                                          
         BAS   RE,REPORT                                                        
         B     XIT                                                              
*                                                                               
PROCERR  DC    H'0'                                                             
         EJECT                                                                  
*              POST GOALS (HOOK FROM NETGOAL)                                   
         SPACE 3                                                                
GOALPOST NTR1                                                                   
         XC    SREC,SREC           CLEAR                                        
         MVC   SPROD,NGOALPRD      THEN FILL BITS OF SORT RECORD                
         MVC   SDP,NGOALDP                                                      
         CLI   SDP,C'M'            GOAL M=EARLY                                 
         BNE   *+8                                                              
         MVI   SDP,C'E'            BUY E=EARLY                                  
         CLI   WEEKSEP,C'Y'                                                     
         BNE   *+10                                                             
         MVC   SWEEKA,NGOALWKN                                                  
         MVC   SWEEKB,NGOALWKN                                                  
         MVC   STARG,NGOALTRG                                                   
         MVC   SGOALGRP,NGOALGRP                                                
         MVC   SGOALDOL,NGOALDOL                                                
         BAS   RE,PUTSORT                                                       
         B     XIT                                                              
         SPACE 1                                                                
PUTSORT  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'PUT',SREC                                         
         B     XIT                                                              
         EJECT                                                                  
*              POST UNITS - DATA FIELDS                                         
         SPACE 3                                                                
UNIT     NTR1                                                                   
         XC    SREC,SREC           CLEAR                                        
         LA    R3,PRDLIST                                                       
         USING LISTD,R3                                                         
         L     R0,NGNPRDS                                                       
         MVC   BYTE,NBPRD                                                       
         CLI   NBSPLPRN,0                                                       
         BE    UNIT2                                                            
         CLI   NBSPLPRN,X'FF'                                                   
         BE    UNIT2                                                            
         MVC   BYTE,NBSPLPRN                                                    
         SPACE 1                                                                
UNIT2    CLC   LISTNO,BYTE         LOOK UP PRODUCT NUMBER                       
         BE    UNIT4                                                            
*        LA    R3,8(R3)                                                         
         LA    R3,10(R3)                                                        
         BCT   R0,UNIT2                                                         
         B     XIT                                                              
         SPACE 1                                                                
UNIT4    DS    0H                                                               
         CLI   8(R3),X'21'         IS IT USER DEM                               
         BNE   *+8                                                              
         MVI   SUSRDEM,X'21'                                                    
         MVC   SPROD,LISTPRD       PASS PRODUCT ALPHA                           
         MVC   STARG,LISTTARG           AND TARGET DEMO                         
         MVC   SDP,NBACTDP                                                      
         LA    R2,1                                                             
         LA    R3,WKLIST                                                        
         SPACE 1                                                                
UNIT6    CLC   NBACTDAT,2(R3)      LOOK UP WEEK NUMBER                          
         BNH   UNIT8                                                            
         LA    R2,1(R2)                                                         
         LA    R3,4(R3)                                                         
         B     UNIT6                                                            
         SPACE 1                                                                
UNIT8    STC   R2,SWEEKB                                                        
         CLI   WEEKSEP,C'Y'                                                     
         BNE   *+8                                                              
         STC   R2,SWEEKA                                                        
         MVC   SDATE,NBACTDAT                                                   
         MVC   STIME,NBTIME                                                     
         OC    NBAFFTIM,NBAFFTIM   USE AFFID. TIME IF AVAILABLE                 
         BZ    UNIT10                                                           
         XC    STIME,STIME                                                      
         MVC   STIME(2),NBAFFTIM                                                
         SPACE 1                                                                
UNIT10   MVC   SNET,NBACTNET                                                    
         MVC   SPROG,NBPROGNM                                                   
         GOTO1 DATCON,DMCB,(2,SDATE),(0,WORK)                                   
         GOTO1 GETDAY,DMCB,WORK,SDAY                                            
         MVC   SLEN,NBLEN                                                       
         EJECT                                                                  
*              POST UNIT NUMERIC FIELDS                                         
         SPACE 3                                                                
         XC    NDDEMOS,NDDEMOS       CLEAR DEMO LIST                            
         MVI   NDDEMOS+1,C'I'        REQUEST TO RELOOK UP DEMOS                 
         MVC   NDDEMOS+2(1),STARG                                               
         CLI   SUSRDEM,X'21'         IS IT USER DEMO                            
         BNE   UNIT10B                                                          
         ZIC   R1,STARG              GET USER DEMO NAME/UNIV                    
         BCTR  R1,0                                                             
         MH    R1,=H'7'                                                         
         LA    RE,NDUSRNMS                                                      
         AR    RE,R1                                                            
         MVC   SUSRDEM(7),0(RE)                                                 
         LA    RE,NDUSRUNV                                                      
         ZIC   R1,STARG                                                         
         BCTR  R1,0                                                             
         MH    R1,=H'4'                                                         
         AR    RE,R1                                                            
         MVC   SUSRDEM+7(4),0(RE)                                               
         MVI   NDDEMOS+1,X'21'                                                  
UNIT10B  GOTO1 NBNETVAL,DMCB,NETBLOCK                                           
         SPACE 1                                                                
         L     R0,NBACTUAL                                                      
         SRDA  R0,31                                                            
         D     R0,=F'100'                                                       
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,SCOST            COST IN DOLLARS                              
         MVC   SSTATUS,NBUNITST    PASS THE STATUS                              
         MVC   SEHGRP+2(2),NBESTHOM+2                                           
         MVC   SEHIMP,NBESTHOM+4                                                
         MVC   SEDGRP+2(2),NDESTDEM+2                                           
         MVC   SEDIMP,NDESTDEM+4                                                
         CLI   ACTOPT,C'N'         OPTION TO SUPPRESS ACTUAL                    
         BNE   UNIT11                                                           
*******  CLI   NBRESULT,C'E'       ALWAYS SUPPRESS ACTUAL COLUMN                
         BE    UNIT12                                                           
         SPACE 1                                                                
UNIT11   MVC   SAHGRP+2(2),NBACTHOM+2                                           
         MVC   SAHIMP,NBACTHOM+4                                                
         MVC   SADGRP+2(2),NDACTDEM+2                                           
         MVC   SADIMP,NDACTDEM+4                                                
         SPACE 1                                                                
UNIT12   BAS   RE,PUTSORT                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO CONTROL REPORT                                       
         SPACE 3                                                                
REPORT   NTR1                                                                   
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         XC    SREC,SREC                                                        
         SPACE 1                                                                
REP2     GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R5,DMCB+4                                                        
         LTR   R5,R5               EOF                                          
         BNZ   REP4                                                             
         BAS   RE,WEEKBTOT                                                      
         BAS   RE,DPTOTS                                                        
         BAS   RE,WEEKATOT                                                      
         BAS   RE,PRODTOTS                                                      
         BAS   RE,REQTOTS                                                       
         B     XIT                                                              
         SPACE 1                                                                
REP4     OC    SREC,SREC           FIRST TIME - NO TOTALS                       
         BZ    LINE1                                                            
         CLC   SKEY(3),0(R5)       PRODUCT C/B                                  
         BE    REP5                                                             
         BAS   RE,WEEKBTOT                                                      
         BAS   RE,DPTOTS                                                        
         BAS   RE,WEEKATOT                                                      
         BAS   RE,PRODTOTS                                                      
         MVC   SREC,0(R5)                                                       
         B     LINE2                                                            
         SPACE 1                                                                
REP5     CLC   SKEY(4),0(R5)       MAJOR WEEK CONTROL BREAK                     
         BE    REP6                                                             
         BAS   RE,WEEKBTOT                                                      
         BAS   RE,DPTOTS                                                        
         BAS   RE,WEEKATOT                                                      
         MVC   SREC,0(R5)                                                       
         B     LINE2                                                            
         SPACE 1                                                                
REP6     CLC   SKEY(5),0(R5)       DAYPART C/B                                  
         BE    REP8                                                             
         BAS   RE,WEEKBTOT                                                      
         BAS   RE,DPTOTS                                                        
         MVC   SREC,0(R5)                                                       
         B     LINE4                                                            
         SPACE 1                                                                
REP8     CLC   SKEY(6),0(R5)       WEEK C/B                                     
         BE    REP10                                                            
         BAS   RE,WEEK2                                                         
         MVC   SREC,0(R5)                                                       
         B     LINE6                                                            
         SPACE 1                                                                
REP10    MVC   SREC,0(R5)                                                       
         B     LINE6                                                            
         EJECT                                                                  
*              FILL IN LINE DETAILS                                             
         SPACE 3                                                                
LINE1    MVC   SREC,0(R5)          FIRST TIME                                   
         SPACE 1                                                                
LINE2    MVI   FORCEHED,C'Y'       FIRST FOR PRODUCT                            
         MVC   LASTWEEK,SWEEKB                                                  
         SPACE 1                                                                
LINE4    BAS   RE,DPLOOK           FIRST FOR DAYPART                            
         SPACE 1                                                                
LINE6    BAS   RE,ADDEM            ADD NUMERIC TO ACCUMS                        
         OC    SDATE,SDATE         SKIP THIS FOR GOAL RECORDS                   
         BZ    REP2                                                             
*                                  UNIT DATE                                    
         GOTO1 DATCON,DMCB,(2,SDATE),(4,P+10)                                   
         MVC   P+16(4),SNET        NETWORK                                      
         MVC   P+21(16),SPROG      PROGRAM                                      
         MVC   P+38(3),SDAY        DAY AND TIME                                 
         GOTO1 UNTIME,DMCB,STIME,P+42                                           
         EDIT  (1,SLEN),(3,P+54)                                                
         MVI   LEVEL,1                                                          
         BAS   RE,FORMAT                                                        
         B     REP2                                                             
         SPACE 1                                                                
DPLOOK   NTR1                                                                   
         LA    R1,DPLIST           ROUTINE TO EXPAND DAYPART                    
         SPACE 1                                                                
DPLOOK2  CLI   0(R1),X'FF'                                                      
         BE    DPLOOK4                                                          
         CLC   SDP,0(R1)                                                        
         BE    DPLOOK4                                                          
         LA    R1,9(R1)                                                         
         B     DPLOOK2                                                          
         SPACE 1                                                                
DPLOOK4  MVC   P+1(8),1(R1)                                                     
         B     XIT                                                              
         SPACE 1                                                                
DPLIST   DS    0H                                                               
         DC    C'DDAYTIME '                                                     
         DC    C'FFRINGE  '                                                     
         DC    C'PPRIME   '                                                     
         DC    C'KKIDS    '                                                     
         DC    C'YYOUTH   '                                                     
         DC    C'SSPORTS  '                                                     
         DC    C'NNEWS    '                                                     
         DC    C'LLATE    '                                                     
         DC    C'EEARLY   '                                                     
         DC    C'TTEENS   '                                                     
         DC    C'CCABLE   '                                                     
         DC    C'XSYND.   '                                                     
         DC    C'ISPECIAL '                                                     
         DC    C'OOLYMPICS'                                                     
         DC    X'FF',C'OTHERS '                                                 
         EJECT                                                                  
*              ROUTINE TO ADD IN ACCUMULATORS                                   
         SPACE 3                                                                
ADDEM    NTR1                                                                   
         MVC   ACCUMS(48),SCOST    MOVE ACCUMS INTO LINE 1                      
         LA    R2,ACCUMS+48                                                     
         LA    R0,4                                                             
         SPACE 1                                                                
ADD2     BAS   RE,ADD4             ADD TO LINES 2-5                             
         LA    R2,48(R2)                                                        
         BCT   R0,ADD2                                                          
         B     XIT                                                              
         SPACE 1                                                                
ADD4     NTR1                                                                   
         LA    R3,ACCUMS           ROUTINE ADD2 THE 12 COLUMNS                  
         LA    R0,12                                                            
         SPACE 1                                                                
ADD6     L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,ADD6                                                          
         B     XIT                                                              
         EJECT                                                                  
*              TOTALLING ROUTINES                                               
         SPACE 3                                                                
WEEKATOT NTR1                                                                   
         CLI   WEEKSEP,C'Y'        ONLY PROCESS IF WEEK IS HIGH                 
         BNE   XIT                                                              
         B     WEEK5                                                            
         SPACE 1                                                                
WEEKBTOT NTR1                                                                   
         CLI   WEEKSEP,C'Y'        IF WEEK IS HIGH, DON'T PROCESS               
         BE    XIT                                                              
         CLI   WEEKSW,C'Y'         DON'T PRINT WEEK TOTALS                      
         BE    WEEK4               IF THIS IS FIRST FOR DAYPART                 
         XC    WEEKACS,WEEKACS                                                  
         GOTO1 SPOOL,DMCB,(R8)     SPACE A LINE                                 
         B     XIT                                                              
         SPACE 1                                                                
WEEK2    NTR1                                                                   
         CLI   WEEKSEP,C'Y'        IF WEEK IS HIGH, DON'T PROCESS               
         BE    XIT                                                              
         MVI   WEEKSW,C'Y'                                                      
         SPACE 1                                                                
WEEK4    GOTO1 SPOOL,DMCB,(R8)     SPACE A LINE                                 
         SPACE 1                                                                
WEEK5    MVI   LEVEL,2                                                          
         MVC   P+42(11),=C'WEEK TOTALS'                                         
         MVI   SPACING,2                                                        
         BAS   RE,FORMAT                                                        
         B     XIT                                                              
         SPACE 1                                                                
DPTOTS   NTR1                                                                   
         CLI   WEEKSEP,C'Y'        IF WEEK=SEP                                  
         BNE   DPTOTS2                                                          
         GOTO1 SPOOL,DMCB,(R8)     SPACE A LINE                                 
         SPACE 1                                                                
DPTOTS2  MVI   LEVEL,3                                                          
         MVC   P+42(11),=C'TOTALS     '                                         
         MVI   SPACING,2                                                        
         BAS   RE,FORMAT                                                        
         MVI   WEEKSW,C'N'                                                      
         B     XIT                                                              
         SPACE 1                                                                
PRODTOTS NTR1                                                                   
         MVI   LEVEL,4                                                          
         MVC   P+42(11),=C'GRAND TOTAL'                                         
         MVI   SPACING,2                                                        
         BAS   RE,FORMAT                                                        
         B     XIT                                                              
         SPACE 1                                                                
REQTOTS  NTR1                                                                   
         MVI   LEVEL,5                                                          
         MVC   P+42(11),=C'CLIENT TOTS'                                         
         MVI   SPACING,2                                                        
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,FORMAT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              FORMAT NUMERIC FIELDS AND PRINT                                  
         SPACE 3                                                                
FORMAT   NTR1                                                                   
         ZIC   R2,LEVEL                                                         
         BCTR  R2,0                                                             
         MH    R2,=H'48'                                                        
         LA    R2,ACCUMS(R2)                                                    
         USING SCOST,R2                                                         
         CLI   SPLCOS,C'N'         IF SHOULDN'T SHOW COST                       
         BE    FM2                                                              
         EDIT  (4,SCOST),(9,P+57)  COST                                         
         CLI   LEVEL,1             IF WE'RE DOING DETAILS                       
         BNE   FM2                                                              
         TM    SSTATUS,X'20'       CHECK IF ACTUAL COST IS REAL                 
         BNO   FM2                                                              
         OI    P+57+8,X'F0'     YES SO ENSURE ZERO PRINTS                       
         SPACE 1                                                                
FM2      LA    R3,SEHGRP           HOMES GRP                                    
         LA    R4,P+68                                                          
         BAS   RE,EDGRP                                                         
         LA    R3,SAHGRP                                                        
         LA    R4,P+74                                                          
         BAS   RE,EDGRP                                                         
         SPACE 1                                                                
*                                  HOMES IMPS                                   
         NETGO NVPRDEM,DMCB,(C'I',0),SEHIMP,P+79                                
         NETGO NVPRDEM,DMCB,(C'I',0),SAHIMP,P+86                                
         SPACE 1                                                                
         LA    R3,SGOALGRP         GOAL GRP                                     
         LA    R4,P+95                                                          
         BAS   RE,EDGRP                                                         
         SPACE 1                                                                
         LA    R3,SEDGRP           DEMO GRP                                     
         LA    R4,P+101                                                         
         BAS   RE,EDGRP                                                         
         LA    R3,SADGRP                                                        
         LA    R4,P+107                                                         
         BAS   RE,EDGRP                                                         
         SPACE 1                                                                
*                                  DEMO IMPS                                    
         NETGO NVPRDEM,DMCB,(C'I',0),SEDIMP,P+112                               
         NETGO NVPRDEM,DMCB,(C'I',0),SADIMP,P+119                               
         SPACE 1                                                                
         CLI   INDEXOPT,C'N'                                                    
         BE    FORMEND             OPTION TO SUPPRESS INDEX                     
         CLI   LEVEL,1                                                          
         BE    FORMEND             NOT FOR DETAIL LINES                         
         SPACE 1                                                                
         LA    R3,SEDGRP           EST TO GOAL INDEX                            
         LA    R4,P2+101                                                        
         BAS   RE,EDINDEX                                                       
         SPACE 1                                                                
         LA    R3,SADGRP           ACT TO GOAL INDEX                            
         LA    R4,P2+107                                                        
         BAS   RE,EDINDEX                                                       
         SPACE 1                                                                
FORMEND  GOTO1 SPOOL,DMCB,(R8)                                                  
         ZIC   R2,LEVEL                                                         
         BCTR  R2,0                                                             
         MH    R2,=H'48'                                                        
         LA    R2,ACCUMS(R2)                                                    
         XC    0(48,R2),0(R2)                                                   
         B     XIT                                                              
         SPACE 1                                                                
EDGRP    NTR1                                                                   
         L     R1,0(R3)            ROUTINE TO FORMAT POINTS                     
         LTR   R1,R1                                                            
         BZ    XIT                                                              
         CH    R1,=H'9999'                                                      
         BH    EDGRP2                                                           
         EDIT  (R1),(5,0(R4)),1    1 DECIMAL IF ROOM                            
         B     XIT                                                              
         SPACE 1                                                                
EDGRP2   AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(5,0(R4))      ELSE SHOW POINTS                             
         B     XIT                                                              
         SPACE 1                                                                
EDINDEX  NTR1                                                                   
         OC    0(4,R3),0(R3)                                                    
         BZ    XIT                                                              
         OC    SGOALGRP,SGOALGRP                                                
         BZ    XIT                                                              
         MVC   P2+95(5),=C'INDEX'                                               
         L     R1,0(R3)                                                         
         M     R0,=F'2000'                                                      
         D     R0,SGOALGRP                                                      
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         MVI   4(R4),C'%'                                                       
         CH    R1,=H'999'                                                       
         BH    EDINDEX2                                                         
         EDIT  (R1),(4,0(R4)),1                                                 
         B     XIT                                                              
         SPACE 1                                                                
EDINDEX2 AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(4,0(R4))                                                   
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE HOOK - BOXES AND HEADLINES                              
         SPACE 3                                                                
HOOK     NTR1                                                                   
         L     R4,ABOX             SET UP BOXES IF OFF LINE                     
         USING BOXD,R4                                                          
         LTR   R4,R4                                                            
         BZ    HOOK1                                                            
         CLI   BOXOPT,C'N'                                                      
         BE    HOOK1               OPTION TO SUPPRESS BOXES                     
         MVI   H12+131,0                                                        
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,0                                                         
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXROWS+08,C'T'                                                  
         MVI   BOXROWS+11,C'M'                                                  
         MVI   BOXROWS+58,C'B'                                                  
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+9,C'C'                                                   
         MVI   BOXCOLS+15,C'C'                                                  
         MVI   BOXCOLS+20,C'C'                                                  
         MVI   BOXCOLS+37,C'C'                                                  
         MVI   BOXCOLS+41,C'C'                                                  
         MVI   BOXCOLS+53,C'C'                                                  
         MVI   BOXCOLS+57,C'C'                                                  
         MVI   BOXCOLS+67,C'C'                                                  
         MVI   BOXCOLS+79,C'C'                                                  
         MVI   BOXCOLS+94,C'C'                                                  
         MVI   BOXCOLS+112,C'C'                                                 
         MVI   BOXCOLS+127,C'R'                                                 
         SPACE 1                                                                
HOOK1    MVC   H4+10(3),SPLCLI                                                  
         MVC   H5+10(3),SPROD                                                   
         MVC   H6+10(3),SPLEST                                                  
         MVC   H4+14(20),SPLCLIN                                                
         BAS   RE,PRODNAME                                                      
         MVC   H5+14(20),LASTNAME                                               
         MVC   H6+14(24),SPLESTN                                                
         MVC   H6+122(10),TARGUNIV                                              
         SPACE 1                                                                
         CLI   WEEKSEP,C'Y'        HANDLE PERIOD FOR MULTI-WEEK VERSION         
         BNE   HOOK2                                                            
         ZIC   R2,LASTWEEK                                                      
         BCTR  R2,0                                                             
         SLL   R2,2                                                             
         LA    R2,WKLIST(R2)                                                    
         GOTO1 DATCON,DMCB,(2,0(R2)),(8,H4+56)                                  
         GOTO1 DATCON,DMCB,(2,2(R2)),(8,H4+65)                                  
         EJECT                                                                  
*              HEADLINE HOOK - COLUMN HEADINGS ETC.                             
         SPACE 3                                                                
HOOK2    XC    DUB,DUB             BUILD DEMO FOR DEMOCON                       
         MVI   DUB+1,C'R'          USE RATING                                   
         MVC   DUB+2(1),STARG                                                   
         CLI   SUSRDEM,0           IF USER DEM                                  
         BE    HOOK3                                                            
         MVC   WORK(7),SUSRDEM                                                  
         B     HOOK3B                                                           
HOOK3    GOTO1 DEMOCON,DMCB,(0,DUB),(7,WORK),(C'S',DBLOCK)                      
HOOK3B   MVC   H6+105(7),WORK                                                   
         GOTO1 CENTER,DMCB,WORK,7                                               
         MVC   H10+97(7),WORK                                                   
         MVC   H10+113(7),WORK                                                  
         SPACE 1                                                                
         CLI   LEVEL,1                                                          
         BNE   HOOK4                                                            
         BAS   RE,DPLOOK           REFORMAT DAYPART NAME                        
         SPACE 1                                                                
HOOK4    MVC   H1+42(23),=C'DAYPART WEEKLY ANALYSIS'                            
         CLI   SPLTITH+5,0                                                      
         BE    HOOK6                                                            
         MVC   H1+42(40),SPACES    DEAL WITH CUSTOM TITLE                       
         ZIC   R1,SPLTITH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     HOOK6                                                            
         MVC   H1+42(0),SPLTIT                                                  
         SPACE 1                                                                
HOOK6    GOTO1 CENTER,DMCB,H1+42,40                                             
         GOTO1 UNDERLIN,DMCB,(40,H1+42),H2+42                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DIG OUT PRODUCT NAME                                  
         SPACE 3                                                                
PRODNAME NTR1                                                                   
         USING LISTD,R3                                                         
         CLC   LASTPROD,SPROD                                                   
         BE    XIT                                                              
         NETGO NVSETSPT,DMCB       SET UP TO READ SPOTFILE                      
         LA    R4,KEY                                                           
         USING PRDHDR,R4                                                        
         XC    PKEY,PKEY                                                        
         MVC   PKEYAM,NBACTAM                                                   
         MVC   PKEYCLT,NBACTCLI                                                 
         MVC   PKEYPRD,SPROD                                                    
         MVC   FILENAME,=C'SPTDIR '                                             
         GOTO1 HIGH                BUMP TO NEXT PRODUCT                         
         CLC   KEY(4),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL '                                             
         GOTO1 GETREC                                                           
         LA    R4,IO                                                            
         MVC   LASTNAME,PNAME                                                   
         MVC   LASTPROD,SPROD                                                   
         XC    DUB,DUB                                                          
         MVI   DUB+1,C'U'                                                       
         MVC   DUB+2(1),STARG                                                   
         CLI   SUSRDEM,0                      IF USERDEM                        
         BE    PRN10                                                            
         ICM   R1,15,SUSRDEM+7                GET USER UNIV                     
         EDIT  (R1),(10,TARGUNIV),ALIGN=LEFT                                    
         B     PRN12                                                            
PRN10    GOTO1 NBDEMOUT,DMCB,(C'D',DUB),DBLOCK,BLOCK                            
         EDIT  (4,BLOCK),(10,TARGUNIV),ALIGN=LEFT                               
PRN12    NETGO NVSETUNT,DMCB       RESET TO READ UNITFILE                       
         XC    FILENAME,FILENAME                                                
         B     XIT                                                              
         SPACE 1                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              HEADLINE SPECS                                                   
         SPACE 3                                                                
         PRINT NOGEN                                                            
HEDSPECS SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H4,50,PERIOD                                                     
         SSPEC H4,99,NETREP                                                     
         SSPEC H5,99,PAGE                                                       
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H6,1,C'ESTIMATE'                                                 
         SSPEC H6,99,C'TARGET='                                                 
         SSPEC H6,114,C'UNIVERSE='                                              
         SSPEC H10,02,C'DAYPART  DATE  NET    PROGRAM NAME'                     
         SSPEC H10,38,C' DAY   TIME      LEN  COST'                             
         SSPEC H10,69,C'HOMES/GRP    HOMES/(000)'                               
         SSPEC H11,69,C'EST   ACT    EST     ACT'                               
         SSPEC H10,96,C'  XXXXXXX/GRP     XXXXXXX/(000)'                        
         SSPEC H11,96,C'GOAL   EST   ACT    EST    ACT'                         
         DC    X'00'                                                            
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,20,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=120'                                   
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
STALIST  DS    CL2000                                                           
         EJECT                                                                  
*              DSECTS FOR PROGRAM                                               
         SPACE 3                                                                
LISTD    DSECT                     DSECT FOR STRUCTURE OF PRDLIST               
LISTNO   DS    CL1                                                              
LISTDIV  DS    CL2                                                              
LISTPRD  DS    CL3                                                              
LISTTARG DS    CL1                                                              
LISTACNO DS    CL1                                                              
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDD0D                                                       
*                                                                               
*                                                                               
*                                                                               
*                                  SPGENPRD                                     
*                                  DDBIGBOX                                     
*                                  NEGENINCLS                                   
*                                  NETDEMOD                                     
*                                  DEDBLOCK                                     
*                                  NENETGOALD                                   
         PRINT OFF                                                              
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE NEGENINCLS                                                     
         PRINT ON                                                               
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE NENETGOALD                                                     
         EJECT                                                                  
*              WORKING STORAGE                                                  
         SPACE 3                                                                
PRDLIST  DS    800CL1              PROD LIST AREA FOR NETGOAL                   
         DS    800CL1              PROD LIST AREA FOR NETGOAL                   
MAXWKS   EQU   16                  MAXIMUM NUMBER OF WEEKS                      
PERTYPE  DS    CL3                 PERIOD TYPE                                  
NUMWKS   DS    F                   NUMBER OF WEEKS                              
WKLIST   DS    CL(4*MAXWKS)        WEEK LIST                                    
DEMOCON  DS    A                                                                
DPFILT   DS    CL1                                                              
SAVMKT   DS    CL2                                                              
LASTPROD DS    CL3                                                              
LASTNAME DS    CL20                                                             
INDEXOPT DS    CL1                                                              
ACTOPT   DS    CL1                                                              
BOXOPT   DS    CL1                                                              
LEVEL    DS    CL1                                                              
WEEKSW   DS    CL1                                                              
WEEKSEP  DS    CL1                                                              
LASTWEEK DS    CL1                                                              
USRDEM   DS    CL1                                                              
ACCUMS   DS    0D                  5 ROWS OF 12                                 
LINEACS  DS    CL48                                                             
WEEKACS  DS    CL48                                                             
DPACS    DS    CL48                                                             
PRODACS  DS    CL48                                                             
REQACS   DS    CL48                                                             
TARGUNIV DS    CL10                                                             
         EJECT                                                                  
*              SORT RECORD ETC                                                  
         SPACE 3                                                                
         DS    0D                                                               
SREC     DS    0CL120                                                           
         SPACE 1                                                                
SKEY     DS    0CL20               KEY FIELDS                                   
SPROD    DS    CL3                                                              
SWEEKA   DS    XL1                                                              
SDP      DS    CL1                                                              
SWEEKB   DS    XL1                                                              
SDATE    DS    XL2                                                              
STIME    DS    CL4                                                              
         DS    CL4                                                              
         SPACE 1                                                                
SNET     DS    CL4                 NON-NUMERIC DATA                             
SPROG    DS    CL16                                                             
SDAY     DS    CL3                                                              
SLEN     DS    XL1                                                              
STARG    DS    CL1                                                              
SSTATUS  DS    CL1                                                              
SUSRDEM  DS    CL11                USER DEMO NAME(7)/USER UNIV(4)               
         DS    CL6                                                              
         SPACE 1                                                                
         DS    0F                                                               
SCOST    DS    F                   COST IN DOLLARS                              
SEHGRP   DS    F                   HOMES EST GRP                                
SAHGRP   DS    F                   HOMES ACT GRP                                
SEHIMP   DS    F                   HOMES EST IMP                                
SAHIMP   DS    F                   HOMES ACT IMP                                
SEDGRP   DS    F                   DEMO. EST GRP                                
SADGRP   DS    F                   DEMO. ACT GRP                                
SEDIMP   DS    F                   DEMO. EST IMP                                
SADIMP   DS    F                   DEMO. ACT IMP                                
SGOALGRP DS    F                   DEMO. GOAL                                   
SGOALDOL DS    F                   DOLLAR GOAL                                  
         DS    CL4                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEMED80T  05/01/02'                                      
         END                                                                    
