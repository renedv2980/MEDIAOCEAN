*          DATA SET TEST49     AT LEVEL 028 AS OF 11/06/02                      
*PHASE T21649A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE KHDUMMY                                                                
         TITLE 'T21649 TALENT COMMERCIAL ACTIVITY LIST'                         
***********************************************************************         
*                                                                     *         
* LEV 00    SEP13/92 INIT                                             *         
* LEV 03    DEC22/92 FIX FOR STATION FILE                             *         
* LEV 04    DEC30/92 FIX ERROR MESSAGE                                *         
* LEV 05    MAR15/93 ADD 3RD LINE OF COMML DESC                       *         
* LEV 06    MAR15/94 ADD DOWNLOAD & STRAFFIC                          *         
* LEV 07    MAR25/94 SEPARATE CODE AND NAME FIELDS                    *         
* LEV 08    MAR30/94 COMBINE ALL THREE COMML TITLE FIELDS             *         
* LEV 09    JUL21/94 CHANGE TO FILENAME                               *         
* LEV 10    OCT04/95 ADD NEW UNITS FIELD FROM MARKET RECORD           *         
* LEV 11    AUG23/96 ALLOW 1 SPACE BETWEEN START " END "              *         
* LEV 12    SEP04/96 ADD LAST LINE COLON IN COL 1 FOR DOWNLOAD        *         
* LEV 13    SEP06/96 REMOVE ALL DOUBLE QUOTES FROM DOWNLOADED DATA    *         
* LEV 14    DEC10/96 ADD TOTALS FOR UNIT CTS                          *         
*           JAN20/97 STOP PAGE BREAKS WITH TOTALS                     *         
* LEV 15    JAN11/02 ADD STARCOM FEATURES                             *         
* LEV 16    FEB20/02 FIX CLIENT COMML CODE                            *         
* LEV 17    APR15/02 USE TRAFFIC OFFICE                               *         
* LEV 18 BG APR30/02 MAKE CABLE STA LARGER                            *         
* LEV 19 BG JUL18/02 SHOW CABLE STA NET                               *         
* LEV 25 BG OCT01/02 READ NON-CLT SPECIFIC STA REC, MAKE TABLES LARGER*         
* LEV 26 SM NOV04/02 IMPROVE TALCOM REPORTING: READ SPOT BUYS         *         
*                                                                     *         
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER      *         
*                    (DDGENCON-T00A30)                                *         
*             AIO2 - PATTERN RECORDS IN INPUT PHASE, MARKET RECS      *         
*                     IN OUTPUT PRINT PHASE                           *         
*             NOW IN VADUMMY                                                    
*             AIO3 - CABLE STATIONS AND INDEX POINTERS TO             *         
*                    CABLE NAME TABLE                                 *         
*                                                                     *         
* REGISTER USAGE -                                                    *         
*        R0 - WORK REG                                                *         
*        R1 - WORK REG                                                *         
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR       *         
*        R3 - POINTER FOR COMML TABLE                                 *         
*        R4 - WORK REG & KEY DSECT POINTER                            *         
*        R5 - WORK REG - ALSO CTR IN LR BUILD COMML TABLE RTN         *         
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM   *         
*              FOR DSECT IN VALREC                                    *         
*        R7 - SECOND BASE                                             *         
*        R8 - POINTER TO SPOOLD                                       *         
*        R9 - POINTER TO SYSD                                         *         
*        RA - POINTER TO ATWA                                         *         
*        RB - FIRST BASE                                              *         
*        RC - POINTER TO GEND                                         *         
*        RD - SAVE AREA POINTER                                       *         
*        RE - GOTO1 REG                                               *         
*        RF - GOTO1 REG                                               *         
*                                                                     *         
*        PROGRAM LABELS MEANING:                                      *         
*        V PREFIX = VALIDATE                                          *         
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                    *         
*        F PREFIX = FIND                                              *         
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST          *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* THIS PROGRAM READS THRU TRAFFIC INSTRUCTION RECAP RECORDS, CREATES  *         
* A MARKET ENTRY FOR EACH COMML AND FIRST/LAST DATE PAIR.             *         
*                                                                     *         
***********************************************************************         
T21649   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21649**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA             BASE SCREEN FOR SYSTEM + THIS PROG           
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR49RR                                                      
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,PRINTREP       OFF-LINE ACTIVITY                            
         BE    LR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
         SPACE                                                                  
VK       LA    R2,TRAMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         SPACE                                                                  
         LA    R2,TRACLTH          CLIENT                                       
         XC    BCLT,BCLT                                                        
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         SPACE                                                                  
         GOTO1 VALICLT                                                          
         MVC   SVBCLT,BCLT                                                      
         SPACE                                                                  
* GET PROFILE REC(S)                                                            
         SPACE                                                                  
* READ T0 PROFILE *                                                             
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         SPACE                                                                  
         MVI   WORK+3,C'1'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         SPACE                                                                  
         MVI   BPRD,0                                                           
         MVI   BPRD2,0                                                          
         XC    QPRD,QPRD                                                        
         XC    QPRD2,QPRD2                                                      
         LA    R2,TRAPRDH                                                       
         CLI   5(R2),0                                                          
         BE    VK10                                                             
         SPACE                                                                  
         GOTO1 VALIPRD                                                          
         SPACE                                                                  
         MVC   QPRD,WORK                                                        
         MVC   BPRD,WORK+3                                                      
         MVC   PRDNM,WORK+5                                                     
         SPACE                                                                  
VK10     LA    R2,TRAPTRH                                                       
         CLI   5(R2),0                                                          
         BE    VK16                                                             
         SPACE                                                                  
         CLI   BPRD,0                                                           
         BE    MISSPRD                                                          
         SPACE                                                                  
         GOTO1 VALIPRD                                                          
         SPACE                                                                  
         MVC   QPRD2,WORK                                                       
         MVC   BPRD2,WORK+3                                                     
         MVC   PRD2NM,WORK+5                                                    
         SPACE                                                                  
VK16     LA    R2,TRAMKTH          MARKET                                       
         XC    BMKT,BMKT                                                        
         XC    QMKT,QMKT                                                        
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         CLC   =C'ALL',8(R2)                                                    
         BE    VK20                                                             
         GOTO1 VALIMKT                                                          
         SPACE                                                                  
VK20     LA    R2,TRACMLH          COMML ID                                     
         XC    SVCMML,SVCMML                                                    
         BAS   RE,VCML                                                          
         SPACE                                                                  
         LA    R2,TRAPERH          PERIOD                                       
         BAS   RE,VPER                                                          
         SPACE                                                                  
         LA    R2,TRAOPTH                                                       
         BAS   RE,VOPT            VALIDATE OPTIONS                              
         SPACE                                                                  
* NOW BUILD KEY                                                                 
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A24'                                                  
         MVC   KEY+2(3),BAGYMD AND BCLT                                         
         MVC   KEY+6(2),BMKT                                                    
         MVI   FIRSTSW,0                                                        
         B     EXIT                                                             
         EJECT                                                                  
* ONLINE ACTIVITY LIST *                                                        
         SPACE 2                                                                
*                                   SET TO READ INSTRUCTION RECAP               
LR       DS    0H                                                               
         TM    SVOPTSW,OPTCABLE    CABLE STATION ONLY                           
         BZ    LR00                                                             
         SPACE                                                                  
         L     R2,VADUMMY                                                       
         MVC   0(8,R2),=CL8'*CBLSTA*'                                           
         LA    R2,8(R2)                                                         
         ST    R2,CBLSTB                                                        
         LR    R0,R2                                                            
         LHI   R1,CBLSTASZ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  (R0),(RE)                                                        
         AHI   R2,CBLSTASZ         ROOM FOR 1600 CABLE STA + INDEX              
         ST    R2,CBLSTBX                                                       
         MVC   0(8,R2),=CL8'*CBLNAM*'                                           
         LA    R2,8(R2)                                                         
         ST    R2,CBLNAM                                                        
         LR    R0,R2                                                            
         LHI   R1,CBLNAMSZ         ROOM FOR 300 CABLE NAMES                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  (R0),(RE)                                                        
         AHI   R2,CBLNAMSZ         ROOM FOR 300 CABLE NAMES                     
         ST    R2,CBLNAMX                                                       
         SPACE                                                                  
CBLSTASZ EQU   1600*5                                                           
CBLNAMSZ EQU   CBLNAML*300                                                      
CBLNAML  EQU   24                                                               
         SPACE                                                                  
LR00     DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING INSKEY,R4                                                        
         MVC   INSKID,=X'0A24'                                                  
         MVC   INSKAM(3),BAGYMD AND BCLT                                        
         MVC   INSKMKT,BMKT                                                     
         SPACE                                                                  
         LA    R3,CMLIST                                                        
         LR    R1,R3                                                            
         SR    R1,R9                                                            
         ST    R1,CMLSTA                                                        
         LR    R0,R9                                                            
         A     R0,LSYSD                                                         
         SH    R0,=AL2(L'CMLENT)                                                
         ST    R0,CMLSTX                                                        
         CLI   OFFLINE,C'Y'        IS THIS ONLINE                               
         BNE   LR10                                                             
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         SPACE                                                                  
         USING CMLISTD,R3                                                       
LR10     SR    R5,R5                                                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     LR26                                                             
         SPACE                                                                  
LR20     MVC   KEY(L'SVKEY),SVKEY                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
LR24     GOTO1 SEQ                                                              
         SPACE                                                                  
LR26     LA    R4,KEY                                                           
         MVC   SVKEY,KEY                                                        
         CLC   KEY(3),KEYSAVE     ID/AM                                         
         BNE   LR80                                                             
         SPACE                                                                  
         CLC   KEY+3(2),SVBCLT                                                  
         BNE   LR80                                                             
         OC    BMKT,BMKT           WAS MARKET ENTERED                           
         BZ    LR30                                                             
         CLC   INSKMKT,BMKT                                                     
         BNE   LR24                                                             
         SPACE                                                                  
         SPACE                                                                  
LR30     CLI   OPTEST,0                                                         
         BE    LR32                                                             
         CLC   OPTEST,INSKCOPY                                                  
         BNE   LR24                                                             
LR32     CLI   BPRD,0                                                           
         BE    LR34                                                             
         CLC   BPRD,INSKPRD                                                     
         BNE   LR24                                                             
         SPACE                                                                  
LR34     DS    0H                                                               
         TM    SVOPTSW,OPTCABLE    CABLE STATION ONLY                           
         BZ    LR36                                                             
         CLI   INSKSTA,X'F0'       THIS A CABLE STATION                         
         BL    LR24                NO, GET NEXT                                 
         B     LR38                                                             
         SPACE                                                                  
LR36     DS    0H                                                               
         TM    SVOPTSW,OPTSTA      STD STATIONS ONLY                            
         BZ    LR38                                                             
         CLI   INSKSTA,X'F0'       THIS A STD STATION                           
         BNL   LR24                NO, GET NEXT                                 
         SPACE                                                                  
LR38     L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         LR    R4,R6                                                            
         USING INSKEY,R4                                                        
         SPACE                                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL            FIND INST DATA ELEM                          
         BNE   LR24                                                             
         USING INSDTAEL,R6                                                      
LR40     CLI   BPRD2,0                                                          
         BE    LR44                                                             
         CLC   BPRD2,INSPRD2                                                    
         BNE   LR78                                                             
         SPACE                                                                  
* CALC NO OF PATTERNS (SUBELS) IN ELEMENT *                                     
         SPACE                                                                  
LR44     SR    R0,R0                                                            
         ZIC   R1,INSDTALN                                                      
         AHI   R1,-(INSPTTN-INSDTAEL)                                           
         D     R0,=F'9'                                                         
         LTR   R0,R0                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R2,INSPTTN                                                       
         ST    R6,SVR6                                                          
LR46     STM   R1,R2,SVR1R2                                                     
         L     R6,SVR6                                                          
         SPACE                                                                  
         CLC   PERSTART,6(R2)      LTD                                          
         BH    LR76                                                             
         CLC   PEREND,3(R2)        FTD                                          
         BL    LR76                                                             
         SPACE                                                                  
         OC    0(3,R2),0(R2)      HIATUS PATTERN IS ZERO                        
         BZ    LR76                                                             
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(3,3(R2)),(2,SVTCDTS)                                
         GOTO1 (RF),(R1),(3,6(R2)),(2,SVTCDTS+2)                                
         SPACE                                                                  
         TM    SVOPTSW,OPTCABLE    DOING CABLE STATIONS ONLY                    
         BZ    *+8                                                              
         SPACE                                                                  
         BAS   RE,CBLFND           GET INDEX FOR CABLE NAME                     
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING PATKEY,R1                                                        
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM,BAGYMD                                                    
         MVC   PATKCLT,SVBCLT                                                   
         MVC   PATKPRD(4),INSPRD1                                               
         MVC   PATKCODE,INSKCOPY                                                
         MVC   PATKREF,0(R2)                                                    
         DROP  R1                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   LR76                                                             
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATCMLEL,R6                                                      
         ZIC   RE,PATCMLLN                                                      
         SRL   RE,4                DIVIDE BY 16 - DROP ODD                      
         LA    R2,2(R6)                                                         
         SPACE                                                                  
LR50     OC    SVCMML,SVCMML       WAS COMML ENTERED                            
         BZ    LR54                                                             
         ZIC   R1,SVCMMLN                                                       
         EX    R1,LRCLCA                                                        
         BE    LR54                                                             
         OC    8(8,R2),8(R2)                                                    
         BZ    LR74                                                             
         EX    R1,LRCLCB                                                        
         BNE   LR74                                                             
         SPACE                                                                  
LR54     CLC   0(8,R2),=XL8'5C00000000000000' DELETED CML                       
         BE    LR74                                                             
         CLC   0(8,R2),HIATUS     BYPASS HIATUS                                 
         BE    LR74                                                             
         OC    0(8,R2),0(R2)      BETTER BE A CML  TEMP                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
* SEARCH FOR EQUAL ENTRY OR BUILD ONE *                                         
         SPACE                                                                  
         LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   CMLMKT,INSKMKT                                                   
         MVC   CMLFTD(4),SVTCDTS                                                
         MVC   CMLCML(16),0(R2)                                                 
         SPACE                                                                  
         TM    SVOPTSW,OPTCABLE                                                 
         BZ    LR56                                                             
         MVC   CMLCSTA,INSKSTA                                                  
         MVC   CMLINDX,SVCBLIND                                                 
         SPACE                                                                  
LR56     DS    0H                                                               
         TM    SVOPTSW,OPTESTP                                                  
         BZ    *+10                                                             
         MVC   CMLEST,INSKCOPY                                                  
         SPACE                                                                  
         CLI   SVMGRPS,0           BY MARKET GROUP                              
         BE    LR58                 NO                                          
         ST    RE,SVRE                                                          
         BAS   RE,RMG              READ & SAVE MARKET GROUP                     
         L     RE,SVRE                                                          
         SPACE                                                                  
LR58     L     R3,CMLSTA                                                        
         AR    R3,R9                                                            
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        IF OFFLINE, SORTING, NO TABLE                
         BE    LR66                                                             
         SPACE                                                                  
         LTR   R0,R5               COUNT OF EXISTING                            
         BZ    LR64                                                             
         SPACE                                                                  
LR60     CLC   CMLENT,WORK                                                      
         BE    LR74                                                             
         LA    R3,CMLNEXT                                                       
         BCT   R0,LR60                                                          
         C     R3,CMLSTX                                                        
         BNL   LSTSIZER                                                         
LR64     MVC   CMLENT,WORK                                                      
         LA    R3,CMLNEXT                                                       
         C     R3,CMLSTX                                                        
         BNL   LSTSIZER                                                         
         B     LR70                BYPASS TABLE BUILD                           
LR66     ST    RE,SVRE                                                          
         TM    SVOPTSW,OPTRACE                                                  
         BZ    LR68                                                             
         MVC   SVP,P                                                            
         MVC   P,SPACES                                                         
         MVC   P(28),WORK                                                       
         GOTO1 HEXOUT,DMCB,WORK,P+30,30,0,0                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P,SVP                                                            
         SPACE                                                                  
LR68     DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',WORK                                     
         L     RE,SVRE                                                          
         SPACE                                                                  
LR70     LA    R5,1(,R5)           ADD TO TABLE COUNT                           
         SPACE                                                                  
LR74     LA    R2,16(,R2)          NEXT CML PR                                  
         BCT   RE,LR50                                                          
LR76     LM    R1,R2,SVR1R2                                                     
         LA    R2,9(,R2)                                                        
         BCT   R1,LR46                                                          
         SPACE                                                                  
         L     R6,SVR6                                                          
         MVI   ELCODE,X'10'                                                     
LR78     BAS   RE,NEXTEL                                                        
         BE    LR40                                                             
         B     LR20                                                             
         SPACE                                                                  
* TABLE (IF ANY) BUILT *                                                        
         SPACE                                                                  
LR80     LTR   R5,R5               WAS ANYTHING FOUND                           
         BNZ   LR90                                                             
         SPACE                                                                  
         CLI   OFFLINE,C'Y'                                                     
         BNE   LR84                                                             
         SPACE                                                                  
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         SPACE                                                                  
LR84     XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=CL30'* NOTE * NO RECORDS SELECTED *'                
         GOTO1 ERREX2                                                           
LRCLCA   CLC   0(0,R2),SVCMML                                                   
LRCLCB   CLC   8(0,R2),SVCMML                                                   
         SPACE                                                                  
         SPACE                                                                  
LR90     MVC   SVKEY,KEY                                                        
         L     R3,CMLSTA                                                        
         AR    R3,R9                                                            
         CLI   OFFLINE,C'Y'        IF OFFLINE, SORTING, NO TABLE                
         BE    LR96                                                             
         SPACE                                                                  
* SORT BY MARKET FTD LTD CMMLS AND STATION *                                    
         SPACE                                                                  
         GOTO1 XSORT,DMCB,(R3),(R5),L'CMLENT,L'CMLENT,0                         
         SPACE                                                                  
         LR    R1,R3                                                            
         LA    R1,CMLNEXT-CMLENT(,R1)                                           
         BCT   R5,*-4                                                           
         XC    0(L'CMLENT,R1),0(R1)                                             
         SPACE                                                                  
LR96     CLI   MODE,PRINTREP       OFF-LINE ACTIVITY                            
         BE    LRR                                                              
         DC    H'0'                                                             
         EJECT                                                                  
* OFF LINE ACTIVITY LIST *                                                      
         SPACE 3                                                                
LRR      GOTO1 =A(FCLT),RR=SPTR49RR                                             
         BNE   EXIT                                                             
         SPACE                                                                  
         XC    HOLDALL,HOLDALL                                                  
         XC    TOTUNTS,TOTUNTS                                                  
         SPACE                                                                  
         LM    R0,R1,=A(HEADING,HDHK)                                           
         A     R0,SPTR49RR                                                      
         ST    R0,SPECS                                                         
         A     R1,SPTR49RR                                                      
         ST    R1,HEADHOOK                                                      
         SPACE                                                                  
         TM    SVOPTSW,OPTDOWN     DOWNLOADING                                  
         BZ    LRR06                NO                                          
         XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
         MVI   LINE,1                                                           
         SPACE                                                                  
         MVC   P(37),=C'"MEDIA" "NAME" "CLIENT" "CLIENT NAME"'                  
         LA    R2,P+38                                                          
         SPACE                                                                  
         CLI   SVMGRPS,0           BY MARKET GROUP                              
         BE    *+14                                                             
         MVC   0(14,R2),=C'"MARKET GROUP"'                                      
         LA    R2,15(,R2)                                                       
         SPACE                                                                  
         MVC   0(12,R2),=C'"COMMERCIAL"'                                        
         LA    R2,13(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(8,R2),=C'"LENGTH"'                                             
         LA    R2,9(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(18,R2),=C'"COMMERCIAL TITLE"'                                  
         LA    R2,19(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(12,R2),=C'"COMMERCIAL"'                                        
         LA    R2,13(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(8,R2),=C'"LENGTH"'                                             
         LA    R2,9(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(18,R2),=C'"COMMERCIAL TITLE"'                                  
         LA    R2,19(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(6,R2),=C'"PROD"'                                               
         LA    R2,7(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(14,R2),=C'"PRODUCT NAME"'                                      
         LA    R2,15(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(6,R2),=C'"PART"'                                               
         LA    R2,7(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(14,R2),=C'"PARTNER NAME"'                                      
         LA    R2,15(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         CLI   OPTEST,0                                                         
         BE    *+14                                                             
         MVC   0(10,R2),=C'"ESTIMATE"'                                          
         LA    R2,11(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(8,R2),=C'"MARKET"'                                             
         TM    SVOPTSW,OPTCABLE    CABLE STATION ONLY                           
         BZ    *+10                                                             
         MVC   0(8,R2),=C'"CABLE "'                                             
         SPACE                                                                  
         LA    R2,9(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(13,R2),=C'"MARKET NAME"'                                       
         LA    R2,14(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         TM    SVOPTSW,OPTUNIT                                                  
         BZ    LRR02                                                            
         SPACE                                                                  
         MVC   0(5,R2),=C'"UNITS"'                                              
         LA    R2,6(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
LRR02    MVC   0(14,R2),=C'"ACTIVE DATES"'                                      
         LA    R2,15(,R2)                                                       
         SPACE                                                                  
         CLI   OPTEST,0                                                         
         BNE   LRR04                                                            
         TM    SVOPTSW,OPTESTP                                                  
         BZ    LRR04                                                            
         MVC   0(21,R2),=C'"ESTIMATE(S) COVERED"'                               
         LA    R2,22(,R2)                                                       
         SPACE                                                                  
LRR04    MVI   0(R2),X'5E'         SEMI COLON                                   
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
LRR06    L     R3,CMLSTA                                                        
         AR    R3,R9                                                            
         CLI   OFFLINE,C'Y'        IF OFFLINE, SORTING, NO TABLE                
         BNE   LRR10                                                            
         XC    CMLENT,CMLENT                                                    
         BAS   RE,GSRT             GO GET SORT RECORD                           
         BNE   LRR90                                                            
         SPACE                                                                  
         OC    CMLCML,CMLCML       BETTER BE AN ENTRY                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
LRR10    CLC   SVCMLMGR,CMLMGR                                                  
         BNE   LRR14                                                            
         SPACE                                                                  
         CLC   SVCMLCML,CMLCML     COMPARES BOTH COMMLS                         
         BE    LRR20                                                            
         SPACE                                                                  
LRR14    TM    SVOPTSW,OPTDOWN     IS THIS DOWNLOADING                          
         BO    LRR16                YES, NO FORCE HEADINGS                      
         SPACE                                                                  
         TM    SVOPTSW,OPTUNIT                                                  
         BZ    LRR16                                                            
         OC    TOTUNTS,TOTUNTS                                                  
         BZ    LRR16                                                            
         SPACE                                                                  
         MVC   PMKTUNTS-18(16),=C'COMMERCIAL TOTAL'                             
         EDIT  (B4,TOTUNTS),(7,PMKTUNTS-1),MINUS=NO,COMMAS=YES                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    TOTUNTS,TOTUNTS                                                  
         SPACE                                                                  
LRR16    MVC   SVCMLMGR,CMLMGR                                                  
         MVC   SVCMLCML,CMLCML                                                  
         TM    SVOPTSW,OPTDOWN     IS THIS DOWNLOADING                          
         BO    LRR20                YES, NO FORCE HEADINGS                      
         MVI   FORCEHED,C'Y'                                                    
         SPACE                                                                  
LRR20    DS   0H                                                                
         CLC   SVMKT,CMLMKT                                                     
         BE    LRR22                                                            
         SPACE                                                                  
         MVC   SVMKT,CMLMKT                                                     
         SPACE                                                                  
         SR    R0,R0                                                            
         ICM   R0,3,CMLMKT                                                      
         CVD   R0,DUB                                                           
         UNPK  QMKT,DUB                                                         
         OI    QMKT+3,X'F0'                                                     
         BAS   RE,FMKT                                                          
         SPACE                                                                  
         MVC   PMKT,QMKT                                                        
         MVC   PMKTNM,MKTNM                                                     
         SPACE                                                                  
LRR22    DS   0H                                                                
         TM    SVOPTSW,OPTCABLE    CABLE STATION ONLY                           
         BZ    LRR23                                                            
         SPACE                                                                  
         XC    DUB(5),DUB                                                       
         MVC   DUB+2(3),CMLCSTA                                                 
         SPACE                                                                  
         GOTO1 MSUNPK,DMCB,(X'80',DUB),WORK,WORK+4                              
         SPACE                                                                  
         MVC   PMKT,WORK+4                                                      
         LA    R1,BIGKEY                                                        
         USING STAPACKD,R1                                                      
         MVI   PMKT+4,C'/'                                                      
         MVC   PMKT+5(3),STAPQNET    PRINT CABLE NET (IF ANY)                   
         DROP  R1                                                               
         SPACE                                                                  
LRR23    DS   0H                                                                
         LA    R2,PMKTUNTS                                                      
         SPACE                                                                  
         TM    SVOPTSW,OPTUNIT                                                  
         BZ    LRR24                                                            
         SPACE                                                                  
         EDIT  (B2,MKTUNTS),(6,(R2)),MINUS=NO,COMMAS=YES                        
         LA    R2,10(,R2)                                                       
         SPACE                                                                  
LRR24    DS   0H                                                                
         TM    SVOPTSW,OPTCABLE    CABLE STATION ONLY                           
         BZ    LRR26                                                            
         SPACE                                                                  
         OC    CMLINDX,CMLINDX     MUST HAVE INDEX TO CABLE GROUP NAME          
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         ICM   RE,3,CMLINDX                                                     
         BCTR  RE,R0                                                            
         MHI   RE,CBLNAML          LENGTH OF 1 ENTRY                            
         LR    RF,RE                                                            
         A     RF,CBLNAM           ADDR OF TABLE                                
         MVC   0(24,R2),0(RF)                                                   
         LA    R2,30(,R2)                                                       
         SPACE                                                                  
LRR26    XC    BLOCK(256),BLOCK                                                 
         MVC   BLOCK(1),CMLEST                                                  
         SPACE                                                                  
LRR30    MVC   HOLDALL,CMLENT      SAVE ENTIRE                                  
         CLI   OFFLINE,C'Y'        IF OFFLINE, SORTING, NO TABLE                
         BNE   LRR34                                                            
         SPACE                                                                  
         BAS   RE,GSRT             GO GET SORT RECORD                           
         BE    LRR36                                                            
         B     LRR50               END OF SORT                                  
         SPACE                                                                  
LRR34    LA    R3,CMLNEXT          BUMP TABLE POINTER                           
         SPACE                                                                  
         OC    CMLENT,CMLENT       END OF TABLE                                 
         BZ    LRR50                                                            
         SPACE                                                                  
* IF SAME ENTRY EXCEPT EST #, JUST SAVE & PRINT ESTIMATE                        
         SPACE                                                                  
LRR36    DS    0H                                                               
         CLC   CMLCOMPA,HOLDCOMP   MGR/CMLS/MARKET/CABLE NAME/DATES             
         BNE   LRR40                                                            
         SPACE                                                                  
LRR38    BAS   RE,SEST             GO SAVE ESTIMATE (IF ANY)                    
         SPACE                                                                  
         B     LRR30                                                            
         SPACE                                                                  
* COMBINE OVERLAPPING OR CONSEQUTIVE DATES                                      
         SPACE                                                                  
LRR40    CLC   CMLCOMP,HOLDCOMP    MGRP/CMLS/MARKET/CABLE NAME                  
         BNE   LRR50                                                            
         SPACE                                                                  
* DO DATES OVERLAP - SINCE SORT IS ON DATES, FTD OF THIS REC                    
* HAS TO BE EQUAL OR HIGHER THAN THE LAST                                       
         SPACE                                                                  
         CLC   CMLLTD-CMLENT+HOLDALL,CMLFTD   NO DATE OVERLAP                   
         BL    LRR42                                                            
         CLC   CMLFTD-CMLENT+HOLDALL,CMLFTD                                     
         BNL   *+10                                                             
         MVC   CMLFTD,CMLFTD-CMLENT+HOLDALL  SAVE LOWER START DATE              
         SPACE                                                                  
         CLC   CMLLTD-CMLENT+HOLDALL,CMLLTD  IS LAST DATE HIGHER                
         BNH   *+10                                                             
         MVC   CMLLTD,CMLLTD-CMLENT+HOLDALL  SAVE HIGHER END DATE               
         SPACE                                                                  
LRR42    CLC   CMLLTD-CMLENT+HOLDALL,CMLFTD                                     
         BNL   LRR44                                                            
         GOTO1 DATCON,DMCB,(2,CMLFTD),(0,WORK)                                  
         LA    R4,CMLLTD-CMLENT+HOLDALL                                         
         GOTO1 (RF),(R1),(2,(R4)),(0,WORK+6)                                    
         GOTO1 ADDAY,(R1),WORK+6,WORK+12,F'1'                                   
         CLC   WORK(6),WORK+12                                                  
         BNE   LRR50                                                            
         SPACE                                                                  
LRR44    MVC   CMLFTD,CMLFTD-CMLENT+HOLDALL                                     
         B     LRR38                                                            
         SPACE                                                                  
LRR50    LA    R4,CMLFTD-CMLENT+HOLDALL                                         
         CLC   PERSTRP,0(R4)                                                    
         BNH   *+10                                                             
         MVC   0(2,R4),PERSTRP                                                  
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(2,(R4)),(5,(R2))                                    
         MVI   8(R2),C'-'                                                       
         LA    R4,CMLLTD-CMLENT+HOLDALL                                         
         CLC   PERENDP,0(R4)                                                    
         BNL   *+10                                                             
         MVC   0(2,R4),PERENDP                                                  
         SPACE                                                                  
         GOTO1 (RF),(R1),(2,(R4)),(5,9(R2))                                     
         SPACE                                                                  
         MVC   HOLDATES,0(R2)                                                   
         SPACE                                                                  
         TM    SVOPTSW,OPTDOWN     IS THIS DOWNLOADING                          
         BO    LRR60                YES                                         
         SPACE                                                                  
         BAS   RE,PEST             GO PRINT ESTIMATE(S)                         
         SPACE                                                                  
LRR56    OC    CMLENT,CMLENT       END OF TABLE                                 
         BZ    LRR90                YES                                         
         B     LRR10                                                            
         SPACE                                                                  
         SPACE                                                                  
LRR60    MVC   P,SPACES                                                         
         LA    R2,P                                                             
         MVI   0(R2),C'"'                                                       
         MVC   1(1,R2),QMED                                                     
         MVI   2(R2),C'"'                                                       
         LA    R2,4(,R2)                                                        
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         MVC   1(L'MEDNM,R2),MEDNM                                              
         OC    1(L'MEDNM,R2),SPACES                                             
         SPACE                                                                  
         LA    R0,L'MEDNM          LENGTH OF FIELD                              
         BAS   RE,BLKQ             GO BLANK DOUBLE QUOTES                       
         SPACE                                                                  
         LA    RF,1+L'MEDNM(,R2)                                                
         BAS   RE,DBLK                                                          
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         MVC   1(L'QCLT,R2),QCLT                                                
         LA    RF,1+L'QCLT(,R2)                                                 
         BAS   RE,DBLK                                                          
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         MVC   1(L'CLTNM,R2),CLTNM                                              
         SPACE                                                                  
         LA    R0,L'CLTNM          LENGTH OF FIELD                              
         BAS   RE,BLKQ             GO BLANK DOUBLE QUOTES                       
         SPACE                                                                  
         LA    RF,1+L'CLTNM(,R2)                                                
         BAS   RE,DBLK                                                          
         SPACE                                                                  
         CLI   SVMGRPS,0           BY MARKET GROUP                              
         BE    LRR64                                                            
         MVI   0(R2),C'"'                                                       
         MVC   1(1,R2),SVMGRPS                                                  
         SR    R0,R0                                                            
         ICM   R0,3,SVCMLMGR                                                    
         SLL   R0,4                                                             
         STCM  R0,7,DUB                                                         
         OI    DUB+2,X'0F'                                                      
         UNPK  2(4,R2),DUB(3)                                                   
         MVI   6(R2),C'"'                                                       
         LA    R2,8(,R2)                                                        
         SPACE                                                                  
LRR64    DS   0H                                                                
         MVI   0(R2),C'"'                                                       
         LA    R5,SVCMLCML                                                      
         SPACE                                                                  
         BRAS  RE,FCML                                                          
         MVC   1(8,R2),SVCMLCML                                                 
         MVI   9(R2),C'"'                                                       
         LA    R2,11(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         ZIC   R0,SVCMLSLN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(3,R2),DUB                                                      
         MVI   4(R2),C'"'                                                       
         LA    R2,6(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         MVC   1(L'SVCMLDSC,R2),SVCMLDSC                                        
         SPACE                                                                  
         LA    R0,L'SVCMLDSC       LENGTH OF FIELD                              
         BAS   RE,BLKQ             GO BLANK DOUBLE QUOTES                       
         SPACE                                                                  
         LA    RF,1+L'SVCMLDSC(,R2)                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R2,2(,RF)                                                        
         SPACE                                                                  
         MVC   0(L'SVCMLDS2,R2),SVCMLDS2                                        
         OC    0(L'SVCMLDS2,R2),SPACES                                          
         SPACE                                                                  
         BCTR  R2,0                                                             
         LA    R0,L'SVCMLDS2       LENGTH OF FIELD                              
         BAS   RE,BLKQ             GO BLANK DOUBLE QUOTES                       
         LA    R2,1(,R2)                                                        
         SPACE                                                                  
         LA    RF,1+L'SVCMLDS2(,R2)                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R2,2(,RF)                                                        
         SPACE                                                                  
         MVC   0(L'SVCMLDS3,R2),SVCMLDS3                                        
         OC    0(L'SVCMLDS3,R2),SPACES                                          
         SPACE                                                                  
         BCTR  R2,0                                                             
         LA    R0,L'SVCMLDS3       LENGTH OF FIELD                              
         BAS   RE,BLKQ             GO BLANK DOUBLE QUOTES                       
         LA    R2,1(,R2)                                                        
         SPACE                                                                  
         LA    RF,1+L'SVCMLDS3(,R2)                                             
         BAS   RE,DBLK                                                          
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         OC    SVCMLCML+8(8),SVCMLCML+8                                         
         BNZ   LRR66                                                            
         OC    1(9,R2),SPACES                                                   
         MVI   2(R2),C'"'          COMML CODE                                   
         LA    R2,4(,R2)                                                        
         SPACE                                                                  
         MVI   0(R2),C'"'          COMML LENGTH                                 
         MVI   2(R2),C'"'                                                       
         LA    R2,4(,R2)                                                        
         SPACE                                                                  
         MVI   0(R2),C'"'          COMML DESC                                   
         MVI   2(R2),C'"'                                                       
         LA    R2,4(,R2)                                                        
         SPACE                                                                  
         B     LRR70                                                            
         SPACE                                                                  
LRR66    LA    R5,SVCMLCML+8                                                    
         BRAS  RE,FCML                                                          
         MVC   1(8,R2),SVCMLCML+8                                               
         MVI   9(R2),C'"'                                                       
         LA    R2,11(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         ZIC   R0,SVCMLSLN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(3,R2),DUB                                                      
         MVI   4(R2),C'"'                                                       
         LA    R2,6(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         MVC   1(L'SVCMLDSC,R2),SVCMLDSC                                        
         SPACE                                                                  
         LA    R0,L'SVCMLDSC       LENGTH OF FIELD                              
         BAS   RE,BLKQ             GO BLANK DOUBLE QUOTES                       
         SPACE                                                                  
         LA    RF,1+L'SVCMLDSC(,R2)                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R2,2(,RF)                                                        
         SPACE                                                                  
         MVC   0(L'SVCMLDS2,R2),SVCMLDS2                                        
         OC    0(L'SVCMLDS2,R2),SPACES                                          
         SPACE                                                                  
         BCTR  R2,0                                                             
         LA    R0,L'SVCMLDS2       LENGTH OF FIELD                              
         BAS   RE,BLKQ             GO BLANK DOUBLE QUOTES                       
         LA    R2,1(,R2)                                                        
         SPACE                                                                  
         LA    RF,1+L'SVCMLDS2(,R2)                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R2,2(,RF)                                                        
         SPACE                                                                  
         MVC   0(L'SVCMLDS3,R2),SVCMLDS3                                        
         OC    0(L'SVCMLDS3,R2),SPACES                                          
         SPACE                                                                  
         BCTR  R2,0                                                             
         LA    R0,L'SVCMLDS3       LENGTH OF FIELD                              
         BAS   RE,BLKQ             GO BLANK DOUBLE QUOTES                       
         LA    R2,1(,R2)                                                        
         SPACE                                                                  
         LA    RF,1+L'SVCMLDS3(,R2)                                             
         BAS   RE,DBLK                                                          
         SPACE                                                                  
LRR70    DS    0H                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(L'QPRD,R2),QPRD                                                
         OC    1(L'QPRD,R2),SPACES                                              
         LA    RF,1+L'QPRD(,R2)                                                 
         BAS   RE,DBLK                                                          
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         MVC   1(L'PRDNM,R2),PRDNM                                              
         OC    1(L'PRDNM,R2),SPACES                                             
         SPACE                                                                  
         LA    R0,L'PRDNM          LENGTH OF FIELD                              
         BAS   RE,BLKQ             GO BLANK DOUBLE QUOTES                       
         SPACE                                                                  
         LA    RF,1+L'PRDNM(,R2)                                                
         BAS   RE,DBLK                                                          
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         CLI   BPRD2,0                                                          
         BNE   LRR72                                                            
         OC    1(9,R2),SPACES                                                   
         MVI   2(R2),C'"'          PRD CODE                                     
         MVI   4(R2),C'"'                                                       
         MVI   6(R2),C'"'          PRD NAME                                     
         LA    R2,8(,R2)                                                        
         B     LRR74                                                            
         SPACE                                                                  
LRR72    MVC   1(L'QPRD2,R2),QPRD2                                              
         LA    RF,1+L'QPRD(,R2)                                                 
         BAS   RE,DBLK                                                          
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         MVC   1(L'PRD2NM,R2),PRD2NM                                            
         OC    1(L'PRD2NM,R2),SPACES                                            
         SPACE                                                                  
         LA    R0,L'PRD2NM         LENGTH OF FIELD                              
         BAS   RE,BLKQ             GO BLANK DOUBLE QUOTES                       
         SPACE                                                                  
         LA    RF,1+L'PRD2NM(,R2)                                               
         BAS   RE,DBLK                                                          
         SPACE                                                                  
LRR74    DS    0H                                                               
         CLI   OPTEST,0                                                         
         BE    LRR76                                                            
         MVI   0(R2),C'"'                                                       
         ZIC   R0,OPTEST                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(3,R2),DUB                                                      
         MVI   4(R2),C'"'                                                       
         LA    R2,6(,R2)                                                        
         SPACE                                                                  
LRR76    DS    0H                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(4,R2),QMKT                                                     
         MVI   5(R2),C'"'                                                       
         LA    R2,7(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         MVC   1(L'MKTNM,R2),MKTNM                                              
         OC    1(L'MKTNM,R2),SPACES                                             
         SPACE                                                                  
         LA    R0,L'MKTNM          LENGTH OF FIELD                              
         BAS   RE,BLKQ             GO BLANK DOUBLE QUOTES                       
         SPACE                                                                  
         LA    RF,L'MKTNM+1(,R2)                                                
         BAS   RE,DBLK                                                          
         SPACE                                                                  
         TM    SVOPTSW,OPTUNIT                                                  
         BZ    LRR78                                                            
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         EDIT  (B2,MKTUNTS),(6,1(R2)),MINUS=NO,COMMAS=YES,ALIGN=LEFT            
         LA    RF,7(,R2)                                                        
         BAS   RE,DBLK                                                          
         SPACE                                                                  
LRR78    MVI   0(R2),C'"'                                                       
         MVC   1(5,R2),HOLDATES    KEEP DOWN LOAD TO MMMDA                      
         MVI   6(R2),C'-'                                                       
         MVC   7(8,R2),HOLDATES+9                                               
         MVI   1+14(R2),C'"'                                                    
         LA    R2,2+14(,R2)                                                     
         SPACE                                                                  
         OC    BLOCK(256),BLOCK                                                 
         BZ    LRR86                                                            
         SPACE                                                                  
         MVI   LINE,1                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         LA    R2,P                                                             
         LA    R4,P+125                                                         
         MVI   0(R2),C'"'                                                       
         LA    R5,BLOCK                                                         
         LA    R2,1(,R2)                                                        
         SPACE                                                                  
LRR80    EDIT  (B1,(R5)),(3,0(R2)),ALIGN=LEFT                                   
         LA    R5,1(,R5)                                                        
         AR    R2,R0                                                            
         CLI   0(R5),0                                                          
         BE    LRR84                                                            
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         CR    R4,R2                                                            
         BH    LRR80                                                            
         SPACE                                                                  
         MVI   LINE,1                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,P                                                             
         B     LRR80                                                            
         SPACE                                                                  
LRR84    MVI   0(R2),C'"'                                                       
         LA    R2,1(,R2)                                                        
         SPACE                                                                  
LRR86    MVI   1(R2),X'5E'         SEMI COLON                                   
         SPACE                                                                  
         MVI   LINE,1                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         OC    CMLENT,CMLENT       END OF TABLE                                 
         BZ    LRR90                                                            
         SPACE                                                                  
         B     LRR10                                                            
         SPACE                                                                  
LRR90    TM    SVOPTSW,OPTDOWN     DOWNLOADING                                  
         BZ    LRR96                                                            
         MVI   P,X'7A'             COLON AS END MARKER TO DOWN LOAD             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         B     EXIT                                                             
         SPACE                                                                  
LRR96    TM    SVOPTSW,OPTUNIT                                                  
         BZ    EXIT                                                             
         OC    TOTUNTS,TOTUNTS                                                  
         BZ    EXIT                                                             
         SPACE                                                                  
         MVC   PMKTUNTS-18(16),=C'COMMERCIAL TOTAL'                             
         EDIT  (B4,TOTUNTS),(7,PMKTUNTS-1),MINUS=NO,COMMAS=YES                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         SPACE 2                                                                
CKFULL   LA    R0,P+70                                                          
         CR    R2,R0                                                            
         BL    CKFULLX                                                          
         SPACE                                                                  
         LR    R0,RE                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,P                                                             
         LR    RE,R0                                                            
CKFULLX  BR    RE                                                               
         SPACE 2                                                                
* DROP ALL BLANKS ON RIGHT                                                      
         SPACE                                                                  
DBLK     CLI   0(RF),C' '                                                       
         BH    DBLK10                                                           
         BCT   RF,DBLK                                                          
         DC    H'0'                                                             
DBLK10   CLI   0(RF),C'"'                                                       
         BNE   DBLK20                                                           
         LA    RF,1(,RF)           ALLOW 1 BLK BETWEEN APOSTROPHES              
         SPACE                                                                  
DBLK20   MVI   1(RF),C'"'                                                       
         LA    R2,3(,RF)                                                        
         LA    R0,P+70                                                          
         CR    R2,R0                                                            
         BLR   RE                                                               
         SPACE                                                                  
         LR    R0,RE                                                            
         MVI   LINE,1                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,P                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE                                                                  
         LA    R0,L'MKTNM          LENGTH OF FIELD                              
         SPACE 2                                                                
* BLANK ALL DOUBLE QUOTES IN FIELD *                                            
         SPACE                                                                  
BLKQ     LA    RF,1(,R2)                                                        
         SPACE                                                                  
BLKQ10   CLI   0(RF),C'"'                                                       
         BNE   BLKQ20                                                           
         MVI   0(RF),C' '                                                       
BLKQ20   LA    RF,1(,RF)                                                        
         BCT   R0,BLKQ10                                                        
         BR    RE                                                               
         EJECT                                                                  
* VALIDATE COMMERCIAL RECORD *                                                  
         SPACE                                                                  
VCML     NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    EXIT                                                             
         GOTO1 ANY                                                              
         CLI   5(R2),8                                                          
         BL    VCML10                                                           
         BH    CMLENER                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(3),BAGYMD AND BCLT                                         
         MVC   KEY+5(8),WORK                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BADCOMM                                                          
         SPACE                                                                  
VCML10   MVC   SVCMML,WORK                                                      
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         STC   R1,SVCMMLN                                                       
         B     EXIT                                                             
         SPACE 3                                                                
* VALIDATE PERIOD                                                               
         SPACE                                                                  
VPER     NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         XC    PEREND,PEREND                                                    
         LA    R3,8(,R2)           START DATE                                   
         GOTO1 DATVAL,DMCB,(R3),WORK                                            
         L     R5,DMCB             GET LENGTH OF FIELD                          
         LTR   R5,R5                                                            
         BZ    DATERR                                                           
         LA    R3,1(R5,R3)         POINT TO END DATE                            
         GOTO1 DATCON,(R1),(0,WORK),(3,PERSTART)                                
         CLM   R5,1,5(R2)          WAS ONLY 1 DATE ENTERED                      
         BE    ONEDATER            YES, ALL DONE                                
         GOTO1 DATVAL,(R1),(R3),WORK+6                                          
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,WORK+6),(3,PEREND)                                
         SPACE                                                                  
         GOTO1 ADDAY,(R1),WORK,WORK+12,F'366'                                   
         CLC   WORK+6(6),WORK+12                                                
         BH    DTSPRDER            ERROR - DATE SPREAD MORE THAN 1 YEAR         
         SPACE                                                                  
         CLC   PERSTART,PEREND                                                  
         BH    DATERR                                                           
         GOTO1 DATCON,(R1),(3,PERSTART),(2,PERSTRP)                             
         GOTO1 (RF),(R1),(3,PEREND),(2,PERENDP)                                 
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE OPTIONS                                                              
         SPACE                                                                  
* VALID OPTIONS ARE  ESTIMATE, EST=, MGROUP *                                   
         SPACE                                                                  
VOPT     NTR1                                                                   
         SPACE                                                                  
         MVI   SVOPTSW,0                                                        
         MVI   SVMGRPS,0                                                        
         MVI   OPTEST,0                                                         
         XC    SVGRPKEY,SVGRPKEY                                                
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VOPT96              NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    VOPTHLP             YES                                          
         CLI   5(R2),4                                                          
         BNH   VOPT02                                                           
         LA    R1,3                                                             
         B     VOPT04                                                           
VOPT02   ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
VOPT04   EX    R1,VOPTCLCJ         HELP                                         
         BE    VOPTHLP                                                          
         SPACE                                                                  
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
         GOTO1 SCANNER,DMCB,TRAOPTH,(7,(R4))                                    
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERR             NO                                           
VOPT10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         SPACE                                                                  
         EX    R1,VOPTCLCA         ESTIMATE                                     
         BNE   VOPT20                                                           
         SPACE                                                                  
         CLI   SVPROF11,C'E'       USING COPY CODE = ESTIMATE                   
         BNE   INVESTOP                                                         
         SPACE                                                                  
         CLI   1(R4),0             WAS ESTIMATE NUMBER ENTERED                  
         BNE   VOPT16               YES                                         
         OI    SVOPTSW,OPTESTP                                                  
         B     VOPT90                                                           
         SPACE                                                                  
VOPT16   TM    3(R4),X'80'         IS THIS NUMERIC                              
         BZ    NUMERR                                                           
         CLC   8(4,R4),=F'255'                                                  
         BH    ESTVALER                                                         
         OC    8(4,R4),8(R4)                                                    
         BZ    ESTVALER                                                         
         MVC   OPTEST,11(R4)                                                    
         B     VOPT90                                                           
         SPACE                                                                  
VOPT20   EX    R1,VOPTCLCB         MGROUP                                       
         BNE   VOPT30                                                           
         SPACE                                                                  
         BAS   RE,VMG              GO VALIDATE MARKET GROUP                     
         SPACE                                                                  
         OI    SVOPTSW,OPTMGRP                                                  
         B     VOPT90                                                           
         SPACE                                                                  
VOPT30   EX    R1,VOPTCLCD         DOWN LOAD                                    
         BNE   VOPT40                                                           
         SPACE                                                                  
         OI    SVOPTSW,OPTDOWN                                                  
         OI    REQRTYP,REQTDOWN    SET DOWNLOAD                                 
         MVC   CONOUT(4),=C'DOWN'                                               
         MVI   CONOUTH+5,4                                                      
         OI    CONOUTH+6,X'80'                                                  
         B     VOPT90                                                           
         SPACE                                                                  
VOPT40   EX    R1,VOPTCLCU         SHOW UNITS FROM MARKET RECORD                
         BNE   VOPT50                                                           
         OI    SVOPTSW,OPTUNIT                                                  
         SPACE                                                                  
         B     VOPT90                                                           
         SPACE                                                                  
VOPT50   EX    R1,VOPTCLCC         SHOW CABLE STATIONS ONLY                     
         BNE   VOPT60                                                           
         SPACE                                                                  
         CLI   QMED,C'T'           MUST BE TV                                   
         BNE   MEDERR                                                           
         SPACE                                                                  
         OI    SVOPTSW,OPTCABLE                                                 
         B     VOPT90                                                           
         SPACE                                                                  
VOPT60   EX    R1,VOPTCLCS         SHOW STATIONS ONLY                           
         BNE   VOPT70                                                           
         SPACE                                                                  
         CLI   QMED,C'T'           MUST BE TV                                   
         BNE   MEDERR                                                           
         SPACE                                                                  
         OI    SVOPTSW,OPTSTA                                                   
         SPACE                                                                  
         B     VOPT90                                                           
         SPACE                                                                  
VOPT70   EX    R1,VOPTCLCT         TRACE                                        
         BNE   VOPTHLP                                                          
         SPACE                                                                  
         OI    SVOPTSW,OPTRACE                                                  
         SPACE                                                                  
VOPT90   LA    R4,32(,R4)          POINT TO NEXT BLOCK                          
         BCT   R3,VOPT10           FOR NUMBER OF BLOCKS FOUND                   
         SPACE                                                                  
VOPT96   B     EXIT                                                             
         SPACE                                                                  
VOPTHLP  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'VOPTHMS),VOPTHMS                                       
         B     ERREXIT                                                          
         SPACE                                                                  
VOPTCLCA CLC   12(0,R4),=CL9'ESTIMATE '                                         
VOPTCLCB CLC   12(0,R4),=CL7'MGROUP '                                           
VOPTCLCC CLC   12(0,R4),=CL6'CABLE '                                            
VOPTCLCD CLC   12(0,R4),=CL9'DOWNLOAD '                                         
VOPTCLCU CLC   12(0,R4),=CL6'UNITS '                                            
VOPTCLCJ CLC   8(0,R2),=CL4'HELP'                                               
VOPTCLCS CLC   12(0,R4),=CL8'STATION '                                          
VOPTCLCT CLC   12(0,R4),=CL6'TRACE '                                            
VOPTHMS  DC    C'* OPTIONS=ESTIMATE, ESTIMATE=, MGROUP, UNITS *'                
         EJECT                                                                  
* VALIDATE MARKET GROUP FROM PROFILE AND BUILD MARKET GROUP TABLE *             
         SPACE                                                                  
VMG      NTR1                                                                   
         CLI   22(R4),C'A'                                                      
         BL    MGRPERR                                                          
         CLI   22(R4),C'Z'                                                      
         BH    MGRPERR                                                          
         SPACE                                                                  
         CLI   1(R4),1             ONLY 1 CHARACTER ENTRY                       
         BE    MGRPERR                                                          
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(3),BAGYMD & BCLT                                           
         MVC   KEY+8(1),22(R4)                                                  
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA SYSTEM                
         GOTO1 HIGH                                                             
         OC    KEY+5(3),KEY+5      FIND PRODUCT GROUP                           
         BZ    VMG20                                                            
         CLC   KEY(5),KEYSAVE                                                   
         BE    VMG10                                                            
         SPACE                                                                  
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE      FIND THIS AGENCY/MEDIA                       
         BNE   MGRPERR                                                          
         SPACE                                                                  
VMG10    CLC   KEY+8(1),22(R4)                                                  
         BE    VMG30                                                            
         MVC   KEY+8(1),22(R4)                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
VMG20    CLC   KEY(9),KEYSAVE     FIND A MARKET GROUP                           
         BE    VMG30                                                            
         SPACE                                                                  
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE      FIND THIS AGENCY/MEDIA                       
         BNE   BDMGRPER                                                         
         SPACE                                                                  
VMG30    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT MEDIA SYSTEM                
         GOTO1 GETREC                                                           
         CLI   24(R6),01                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MGRPTITL,26(R6)                                                  
         MVI   KEY+1,X'82'                                                      
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA SYSTEM                
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVMGRPS,22(R4)                                                   
         MVC   SVGRPKEY(4),KEY+5                                                
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         B     EXIT                                                             
         EJECT                                                                  
* READ MARKET GROUP RECORDS TO FIND MARKETS FOR THIS MARKET GROUP *             
         SPACE                                                                  
RMG      NTR1                                                                   
         SPACE                                                                  
         CLC   RMGMKT,CMLMKT                                                    
         BNE   RMG00                                                            
         MVC   CMLMGR,RMGMGR                                                    
         B     EXIT                                                             
         SPACE                                                                  
RMG00    MVC   RMGKEY,KEY                                                       
         SPACE                                                                  
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING MKGRECD,R4                                                       
         MVC   MKGPTYP,=X'0D82'                                                 
         MVC   MKGPAGMD(3),BAGYMD & BCLT                                        
         MVC   MKGPMID,SVMGRPS                                                  
*        MVC   MKGPMID(4),SVGRPKEY       MOVE IN PRD GRP/MKT GRP SCHEME         
*                                        FROM VMG (VALIDATE MARKET GRP)         
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA SYSTEM                
         GOTO1 HIGH                                                             
         SPACE                                                                  
         CLC   KEY(9),KEYSAVE                                                   
         BE    RMG14                                                            
         SPACE                                                                  
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         SPACE                                                                  
RMG10    CLC   KEY(9),KEYSAVE                                                   
         BNE   RMG40                                                            
         SPACE                                                                  
RMG14    CLC   INSKMKT-INSKEY+SVKEY,MKGPMKT       THIS THE MARKET               
         BE    RMG20                                                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     RMG10                                                            
         SPACE                                                                  
RMG20    MVC   CMLMGR,MKGPMGRP                                                  
         MVC   RMGMGR,MKGPMGRP                                                  
         MVC   RMGMKT,INSKMKT-INSKEY+SVKEY  SAVE THIS MARKET                    
         SPACE                                                                  
RMG36    MVC   KEY(L'RMGKEY),RMGKEY                                             
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    EXIT                                                             
         DC    H'0'                                                             
         SPACE                                                                  
RMG40    MVC   CMLMGR,=XL2'9999'  PUT IN UNKNOWN MARKET GROUP                   
         MVC   SVMGRP+1(2),=XL2'9999'                                           
         CLC   SVMGRPMK,INSKMKT-INSKEY+SVKEY                                    
         B     RMG36                                                            
         EJECT                                                                  
* GET SORT RECORDS, ELIMINATING EQUALS *                                        
         SPACE                                                                  
GSRT     NTR1                                                                   
         SPACE                                                                  
         CLI   FIRSTSW,2           EOFSW SET                                    
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
GSRT10   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         SPACE                                                                  
         ICM   RF,15,4(R1)         GET ADDRESS OF REC                           
         BZ    GSRT50               EOF IF ZERO                                 
         SPACE                                                                  
         MVI   FIRSTSW,1                                                        
         SPACE                                                                  
         CLC   HOLDALL,0(RF)        EQUAL REC                                   
         BE    GSRT10                                                           
         SPACE                                                                  
         MVC   CMLENT,0(RF)         MOVE FOR VISIBLITY                          
         SPACE                                                                  
         TM    SVOPTSW,OPTRACE                                                  
         BZ    GSRT20                                                           
         MVC   SVP,P                                                            
         MVC   P,SPACES                                                         
         MVC   P+1(28),CMLENT                                                   
         GOTO1 HEXOUT,DMCB,CMLENT,P+30,30,0,0                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   P,C'H'                                                           
         MVC   P+1(28),HOLDALL                                                  
         GOTO1 HEXOUT,DMCB,HOLDALL,P+30,30,0,0                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P,SVP                                                            
         SPACE                                                                  
GSRT20   DS    0H                                                               
         CR    R0,R0                                                            
         B     EXIT                                                             
         SPACE                                                                  
GSRT50   XC    CMLENT,CMLENT                                                    
         SPACE                                                                  
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         CLI   FIRSTSW,0           TEST FIRST TIME                              
         BNE   GSRT60              NO                                           
         MVC   P(16),=C'NO INPUT RECORDS'                                       
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         B     EXIT                                                             
         SPACE                                                                  
GSRT60   MVI   FIRSTSW,2           SET EOFSW                                    
         SPACE                                                                  
         B     EXIT                                                             
         EJECT                                                                  
* SAVE ESTIMATE # IN BLOCK *                                                    
         SPACE                                                                  
SEST     CLI   CMLEST,0                                                         
         BER   RE                                                               
         LA    R0,256                                                           
         LA    R1,BLOCK                                                         
SEST10   CLI   0(R1),0                                                          
         BE    SEST20                                                           
         CLC   CMLEST,0(R1)                                                     
         BER   RE                                                               
         LA    R1,1(,R1)                                                        
         BCT   R0,SEST10                                                        
         DC    H'0'                                                             
SEST20   MVC   0(1,R1),CMLEST                                                   
         BR    RE                                                               
         SPACE 2                                                                
* PRINT ESTIMATE TABLE FROM BLOCK                                               
         SPACE                                                                  
PEST     NTR1                                                                   
         LA    R3,BLOCK                                                         
         LR    R1,R3                                                            
         SR    R0,R0                                                            
PEST10   CLI   0(R1),0                                                          
         BE    PEST14                                                           
         LA    R1,1(,R1)                                                        
         BCT   R0,PEST10                                                        
         DC    H'0'                                                             
PEST14   LPR   R4,R0                                                            
         BZ    PEST16                                                           
         GOTO1 XSORT,DMCB,BLOCK,(R4),1,1,0                                      
         SPACE                                                                  
PEST16   TM    SVOPTSW,OPTDOWN     DOWNLOADING                                  
         BO    PESTX                YES                                         
         LTR   R4,R4                                                            
         BZ    PEST30                                                           
         SPACE                                                                  
         LA    R5,PESTS-PACTDTES+L'PESTS(,R2)                                   
PEST20   LA    R4,PESTS-PACTDTES(,R2)                                           
         SPACE                                                                  
PEST24   EDIT  (B1,(R3)),(3,0(R4)),ALIGN=LEFT                                   
         LA    R3,1(,R3)                                                        
         CLI   0(R3),0                                                          
         BE    PEST30                                                           
         AR    R4,R0                                                            
         MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         CR    R5,R4                                                            
         BH    PEST24                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PEST20                                                           
         SPACE                                                                  
PEST30   GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
PESTX    B     EXIT                                                             
         EJECT                                                                  
* READ MARKET RECORD *                                                          
         SPACE                                                                  
FMKT     NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
         SPACE                                                                  
         MVC   AIO,AIO2                                                         
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO                      
         SPACE                                                                  
         MVC   MKTNM,=CL24'**** UNKNOWN ****'                                   
         XC    MKTUNTS,MKTUNTS                                                  
         L     R1,AIO                                                           
         CLC   KEY(8),0(R1)                                                     
         BNE   EXIT                                                             
         SPACE                                                                  
         USING MKTRECD,R1                                                       
         MVC   MKTNM,MKTNAME                                                    
         SR    R0,R0                                                            
         ICM   R0,3,MKTUNIT                                                     
         STH   R0,MKTUNTS                                                       
         A     R0,TOTUNTS                                                       
         ST    R0,TOTUNTS                                                       
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
* FIND CABLE STATION NAME AND SAVE CABLE STATION, INDEX, AND NAME               
         SPACE                                                                  
CBLFND   NTR1                                                                   
         SPACE                                                                  
         USING INSKEY,R4                                                        
         L     R2,CBLSTB                                                        
         USING CBLSTAD,R2                                                       
CBLFND10 DS   0H                                                                
         OC    CBLSTA,CBLSTA                                                    
         BZ    CBLFND30                                                         
         CLC   CBLSTA,INSKSTA      THIS THE SAME STATION                        
         BE    CBLFND20                                                         
         LA    R2,CBLNEXT                                                       
         C     R2,CBLSTBX                                                       
         BL    CBLFND10                                                         
         DC    H'0'                                                             
         SPACE                                                                  
CBLFND20 DS   0H                                                                
         MVC   SVCBLIND,CBLINDEX                                                
         B     EXIT                                                             
         SPACE                                                                  
CBLFND30 DS   0H                                                                
         MVC   CBLSTA,INSKSTA      SAVE STATION                                 
         GOTO1 MSUNPK,DMCB,(X'80',INSKMKT-INSKEY+SVKEY),WORK,WORK+4             
         SPACE                                                                  
         LA    R1,BIGKEY                                                        
         USING STAPACKD,R1                                                      
         MVC   FULL(3),STAPQNET    SAVE CABLE NET (IF ANY)                      
         DROP  R1                                                               
         SPACE                                                                  
         LA    R6,KEY                                                           
         USING STARECD,R6                                                       
         XC    KEY,KEY                                                          
         MVI   STAKTYPE,STAKTYPQ                                                
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,WORK+4                                                  
         MVI   STAKCALL+4,C'T'                                                  
         MVC   STAKAGY,AGENCY                                                   
*        MVC   STAKCLT,QCLT                                                     
         MVC   STAKCLT,=C'000'     AMS CDS NOT IN CLT SPECIFIC REC              
         MVC   STAKFILL,=C'000'                                                 
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,(R6)                     
         SPACE                                                                  
         CLC   KEY(9),0(R6)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         LA    R0,1                                                             
         L     R3,CBLNAM                                                        
         SPACE                                                                  
CBLFND50 DS   0H                                                                
         OC    0(24,R3),0(R3)                                                   
         BZ    CBLFND60                                                         
         CLC   SSYSNAME,0(R3)      THIS THE CABLE SYSTEM NAME                   
         BE    CBLFND60                                                         
         LA    R3,CBLNAML(,R3)                                                  
         AHI   R0,1                                                             
         C     R3,CBLNAMX                                                       
         BL    CBLFND50                                                         
         DC    H'0'                                                             
         SPACE                                                                  
CBLFND60 DS   0H                                                                
         MVC   0(24,R3),SSYSNAME   SAVE THE CABLE SYSTEM NAME                   
         STH   R0,SVCBLIND                                                      
         STCM  R0,3,CBLINDEX                                                    
         SPACE                                                                  
         B     EXIT                                                             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
ESTVALER MVC   CONHEAD,BADESTMS                                                 
         B     ERREXIT                                                          
INVESTOP MVC   CONHEAD,INVESTMS                                                 
         B     ERREXIT                                                          
LSTSIZER TM    WHEN,X'40'          THIS NOW                                     
         BZ    LSTSIZPT            NO PRINT MESSAGE                             
         MVC   CONHEAD,LSTSIZMS                                                 
         B     ERREXIT                                                          
LSTSIZPT XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
         MVC   P+20(33),=CL33'REQUEST REPORT FOR SMALLER PERIOD'                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         SPACE                                                                  
MGRPERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MGRPERMS),MGRPERMS                                     
         B     ERREXIT                                                          
BDMGRPER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BDMGRPMS),BDMGRPMS                                     
         MVC   CONHEAD+20(1),22(R4)                                             
         B     ERREXIT                                                          
CLTREQ   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'CLTREQMS),CLTREQMS                                     
         B     ERREXIT                                                          
ONEDATER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'ONEDATMS),ONEDATMS                                     
         B     ERREXIT                                                          
OLER     XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OLERMS),OLERMS                                         
         B     ERREXIT                                                          
MEDERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MEDMS),MEDMS                                           
         B     ERREXIT                                                          
DTSPRDER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DTSPRDMS),DTSPRDMS                                     
ERREXIT  GOTO1 ERREX2                                                           
         SPACE                                                                  
NUMERR   MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
CMLENER  MVI   ERROR,INVCMMLN                                                   
         B     TRAPERR                                                          
DATERR   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
BADCOMM  MVI   ERROR,INVCOMM                                                    
         B     TRAPERR                                                          
MISSPRD  LA    R2,TRAPRDH                                                       
MISSERR  MVI   ERROR,MISSING                                                    
TRAPERR  GOTO1 ERREX                                                            
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
MGRPERMS DC    C'* ERROR * ENTER MARKET GROUP CNNNN, C=CHARS A-Z, 1-4 DI        
               IGITS*'                                                          
BDMGRPMS DC    C'* ERROR * NO MGROUP X FOUND, TRY AGAIN *'                      
BADESTMS DC    C'* ERROR * ESTIMATE MUST BE 1 TO 255 *'                         
INVESTMS DC    C'* ERROR * TO PROFILE 11 NOT SET FOR ESTIMATE *'                
LSTSIZMS DC    C'* ERROR * LIST TOO LARGE FOR ONLINE, RUN SMALLER OR OVC        
                *'                                                              
CLTREQMS DC    C'* ERROR * CLIENT REQUIRED FOR ONLINE LIST *'                   
ONEDATMS DC    C'* ERROR * MUST ENTER BOTH FROM AND TO DATES *'                 
OLERMS   DC    C'* ERROR * ONLINE LIST NOT SUPPORTED *'                         
MEDMS    DC    C'* ERROR * CABLE/STATION ONLY VALID FOR MEDIA T *'              
DTSPRDMS DC    C'* ERROR * CAN NOT RUN FOR MORE THAN 1 YEAR *'                  
SORTCARD DC    CL80'SORT FIELDS=(1,27,A),FORMAT=BI '                            
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=32 '                                   
HIATUS   DC    CL6'HIATUS',XL2'00'                                              
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,35,C'TALENT COMMERCIAL ACTIVITY REPORT'                       
         SSPEC H2,35,C'---------------------------------'                       
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H3,75,RUN                                                        
         SSPEC H4,75,REQUESTOR                                                  
         SSPEC H4,99,PAGE                                                       
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H3,3,C'CLIENT'                                                   
         SPACE                                                                  
         DC    X'00'               END MARKER FOR SSPECS                        
         SPACE 2                                                                
         DROP  RB,R7,RC                                                         
         EJECT                                                                  
* FIND CLIENT HEADER AND SAVE CLIST                                             
         SPACE                                                                  
         DS    0H                                                               
FCLT     NMOD1 0,**FCLT**                                                       
         SPACE                                                                  
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
FCLT10   MVC   SVKEY,KEY                                                        
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,SVBCLT,QCLT                                          
         SPACE                                                                  
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
         MVC   SVBCLT,BCLT                                                      
         SPACE                                                                  
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    FCLT20                                                           
         CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   KEY,SVKEY                                                        
         MVI   KEY+5,X'FF'         GET NEXT CLIENT                              
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE      IF SAME REC TYPE & A/M                       
         BNE   FCLTNE                                                           
         SPACE                                                                  
         MVC   SVBCLT,KEY+3        SAVE CLIENT                                  
         B     FCLT10                                                           
         SPACE                                                                  
* GET PROFILE REC(S)                                                            
         SPACE                                                                  
* READ T0 PROFILE *                                                             
         SPACE                                                                  
FCLT20   XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         SPACE                                                                  
         MVI   WORK+3,C'1'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         SPACE                                                                  
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         SPACE                                                                  
         CR    RB,RB                                                            
         B     FCLTX                                                            
         SPACE                                                                  
FCLTNE   LTR   RB,RB                                                            
FCLTX    XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         SPACE                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
* FIND COMMERCIAL TITLE AND LENGTH *                                            
         SPACE                                                                  
FCML     NMOD1 0,**FCML**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         XC    SVCMLCLT,SVCMLCLT                                                
         XC    SVCMLDS2,SVCMLDS2                                                
         XC    SVCMLDS3,SVCMLDS3                                                
         XC    SVBASIC,SVBASIC                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,SVBCLT                                                   
         MVC   CMLKCML,0(R5)                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         MVC   SVCMLDSC(L'CMLTITLE),CMLTITLE                                    
         MVI   SVCMLDSC+L'CMLTITLE,0                                            
         TM    CMLSTAT,X'80'                                                    
         BZ    *+8                                                              
         MVI   SVCMLDSC+L'CMLTITLE,C'*'                                         
         MVC   SVCMLSLN,CMLSLN                                                  
         SPACE                                                                  
         MVC   SVCMLCLT,CMLCLTNO                                                
         SPACE                                                                  
         MVI   ELCODE,X'30'        CK FOR COMM DESC LINES 2 & 3                 
         BRAS  RE,NEXTEL                                                        
         BNE   FCML10                                                           
         MVC   SVCMLDS2,3(R6)                                                   
         BRAS  RE,NEXTEL                                                        
         BNE   FCML10                                                           
         MVC   SVCMLDS3,3(R6)                                                   
FCML10   DS    0H                                                               
         CLC   AGENCY,=C'H9'       THIS STARCOM?                                
         BNE   FCMLX                NO                                          
         SPACE                                                                  
         L     R6,AIO1                                                          
         MVI   ELCODE,X'90'                                                     
         BRAS  RE,GETEL                                                         
         BNE   FCMLX                                                            
         USING CMLBBEL,R6                                                       
         MVC   SVBASIC,CMLBBBCP                                                 
FCMLX    XIT1                                                                   
         DROP  R4,R6,RB,RC                                                      
         EJECT                                                                  
* INITIALIZE TSAROFF                                                            
         SPACE                                                                  
ITSAROFF NTR1                                                                   
         XC    PRTCT,PRTCT                                                      
         XC    TSAROFF,TSAROFF                                                  
         XC    TSARBYTE,TSARBYTE                                                
         SPACE                                                                  
         L     R0,=A(L'ELEM*5000)                                               
         ST    R0,TSARBUFL                                                      
         GETMAIN RC,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,TSARBUFF                                                      
*                                                                               
         LHI   RE,TSARBLK-SYSD                                                  
         AR    RE,R9                                                            
         LR    R2,RE                                                            
         LA    RF,TSARDL2                                                       
         XCEF                                                                   
         USING TSARD,R2                                                         
*                                                                               
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSKEYL,2                                                         
         OI    TSINDS,TSINODSK                                                  
         MVI   TSRECL+1,138                                                     
         MVC   TSABUF,TSARBUFF                                                  
         MVC   TSAREC,TSARBUFL                                                  
         SPACE                                                                  
* TSAR CALLOV HERE                                                              
         SPACE                                                                  
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A7D')                                 
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,TSAROFF                                                       
         SPACE                                                                  
         MVI   TSOFFACT,TSAINI      SET 1ST TSAROFF FOR INIT                    
         GOTO1 TSAROFF,(R2)                                                     
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         EJECT                                                                  
*********************************************                                   
* READ THROUGH BUYS AND BUILD ACTIVITY LIST *                                   
*********************************************                                   
         SPACE                                                                  
         DS    0H                                                               
BLDACT   NMOD1 0,*BLDACT*                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
         GOTO1 =A(ITSAROFF),RR=RB  INIT TSAR FOR OFFLINE                        
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(5,0),(3,TODAYP)                                     
         SPACE                                                                  
* CLEAR EQUIVALENT PRODUCT AREA *                                               
         SPACE                                                                  
         XC    DATELIST(232),DATELIST                                           
         SPACE                                                                  
* 001-230 115 PRS (EQUIV/BASE PRODS) 203-232 SAVED PROD EQUIV TABLE             
* 233-256 24 SAVED BUY KEY                                                      
         SPACE                                                                  
* CLEAR ACTIVITY LIST BUILD AREA *                                              
         SPACE                                                                  
BLD01    L     R4,AIO3                                                          
         LR    RE,R4                                                            
         LA    RF,1000                                                          
         XCEF                                                                   
         SPACE                                                                  
         USING ACLDATA,R4                                                       
         CLI   SVPROF15,C'Y'       USE TRAFFIC, NOT SPOT BUYS                   
         BE    BLDTR                                                            
         SPACE                                                                  
         MVC   SYSDIR(3),=C'SPT'   SWITCH TO SPOT MEDIA                         
         MVC   SYSFIL(3),=C'SPT'                                                
         SPACE                                                                  
         XC    KEY,KEY                                                          
         SPACE                                                                  
         MVC   KEY(3),BAGYMD       A-M/CLT                                      
         MVC   KEY+3(1),BPRD                                                    
         MVC   KEY+4(5),BMKT                                                    
         SPACE                                                                  
BLD02    MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
         SPACE                                                                  
         MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT!                          
         CLI   ERROR,0             ANY ERROR?                                   
         BE    BLD02A                                                           
         SPACE                                                                  
BLDACTER GOTO1 ERREX                                                            
         SPACE                                                                  
BLD02A   CLC   KEY(9),KEYSAVE      A-M/C/P/MKT/STA                              
         BE    BLD03                                                            
         SPACE                                                                  
         OC    KEYSAVE+4(9),KEYSAVE+4   THIS STARTING KEY                       
         BZ    BLD03                     YES, CONTINUE                          
         SPACE                                                                  
         CLI   SVT2PR05,C'Y'       COMBINE CABLE NETWORKS                       
         BNE   BLD92                NO                                          
         SPACE                                                                  
         CLI   KEY+6,X'F0'         THIS A CABLE STATION                         
         BL    BLD92                NO                                          
         SPACE                                                                  
         CLC   KEY(8),KEYSAVE                                                   
         BNE   BLD92                                                            
         MVC   HALF(1),KEY+8                                                    
         MVC   HALF+1(1),KEYSAVE+8                                              
         NI    HALF,X'80'                                                       
         NI    HALF+1,X'80'                                                     
         CLC   HALF(1),HALF+1                                                   
         BE    BLD03                                                            
         B     BLD92                                                            
         EJECT                                                                  
* IF FILTERING ON MARKET GROUP, CHECK IT OUT *                                  
         SPACE                                                                  
BLD03    DS    0H                                                               
         CLI   SVMGRPS,0           BY MARKET GROUP                              
         BE    BLD03C                                                           
         CLC   KEY+4(2),BMKT                                                    
         BE    BLD03C                                                           
         SPACE                                                                  
         GOTO1 =A(RMGB),RR=RB      GO GET NEXT MARKET IN GROUP                  
         BNE   BLD122                                                           
         SPACE                                                                  
BLD03C   DS    0H                                                               
         OC    BMKT,BMKT           TEST SINGLE MKT REQUEST                      
         BZ    BLD03E                                                           
         CLC   KEY+4(2),BMKT                                                    
         BNE   BLD122                                                           
         SPACE                                                                  
BLD03E   DS    0H                                                               
         TM    SVOPTSW,OPTCABLE                                                 
         BZ    BLD04                                                            
         SPACE                                                                  
         CLI   KEY+6,X'F0'         THIS A CABLE STATION                         
         BL    BLD122                                                           
         SPACE                                                                  
BLD04    ZIC   RE,KEY+9                                                         
         LA    RE,SVESTAB(RE)                                                   
         CLI   0(RE),0             TEST EST ACTIVE                              
         BNE   BLD05                YES                                         
         SPACE                                                                  
         XC    KEY+10(3),KEY+10                                                 
         CLI   OPTEST,0            WAS ESTIMATE ENTERED                         
         BE    BLD05                NO                                          
         SPACE                                                                  
         CLC   KEY+9(1),OPTEST     TEST LOWER THAN START EST                    
         BE    BLD05                                                            
         BH    *+14                NO                                           
         MVC   KEY+9(1),OPTEST     FORCE LOW EST                                
         B     BLD02                                                            
         SPACE                                                                  
         MVI   KEY+9,X'FF'         SET TO FORCE NEW STA                         
         SPACE                                                                  
         MVC   KEY+10(3),=3X'FF'                                                
         B     BLD02                                                            
         SPACE                                                                  
BLD05    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         EJECT                                                                  
* GET DAYPART EQUIVALENCY CODE IF PROFILE ON AND NONE IN FOR ESTIMATE *         
         SPACE                                                                  
         CLI   SVPROF11,C'A'       TEST COPY CODE = ADJACENCY                   
         BE    BLD10                                                            
         CLI   SVPROF11,C'D'       TEST COPY CODE = DAYPART                     
         BE    BLD10                                                            
         CLI   SVPROF11,C'Y'       TEST COPY CODE = DAYPART                     
         BNE   BLD16                                                            
BLD10    CLC   EQVEST,KEY+9        SAME EST                                     
         BE    BLD16                                                            
         MVC   TRBUYKEY,KEY                                                     
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING DPEKEY,R1                                                        
         MVC   DPEKID,=X'0A34'                                                  
         MVC   DPEKAM(3),BAGYMD AND BCLT                                        
         MVC   DPEKEST,TRBUYKEY+9                                               
         DROP  R1                                                               
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'TRFDIR'                                            
         GOTO1 HIGH                                                             
         MVI   EQVEST,0                                                         
         XC    EQVTAB,EQVTAB                                                    
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BLD14                                                            
         MVI   ELCODE,X'10'                                                     
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'TRFFIL'                                            
         GOTO1 GETREC                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DPEDTAEL,R6                                                      
         LA    R1,EQVTAB                                                        
BLD12    MVC   0(2,R1),DPEDPCDE                                                 
         LA    R1,2(,R1)                                                        
         BAS   RE,NEXTEL                                                        
         BE    BLD12                                                            
BLD14    MVC   EQVEST,TRBUYKEY+9                                                
         XC    FILENAME,FILENAME                                                
         DROP  R6                                                               
         MVC   KEY,TRBUYKEY                                                     
         SPACE                                                                  
         XC    TRBUYKEY,TRBUYKEY                                                
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
* NOW GET BUY RECORD *                                                          
         SPACE                                                                  
BLD16    DS    0H                                                               
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 GETREC                                                           
         SPACE                                                                  
         MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT!                          
         CLI   ERROR,0             ANY ERROR?                                   
         BNE   BLDACTER                                                         
         SPACE                                                                  
         L     R7,AIO                                                           
         USING BUYRECD,R7                                                       
         SPACE                                                                  
         CLC   KEY+4(2),4(R7)     TEST SAME MARKET                              
         BNE   BLD90                NO - SPILL                                  
         SPACE                                                                  
         MVC   BUYDATE,BDCHG                                                    
         SPACE                                                                  
* CALCULATE DIFFERENCE BETWEEN FIRST/LAST DAYS OF ROTATOR *                     
         SPACE                                                                  
         XC    ROTDAYS,ROTDAYS                                                  
         ZIC   R0,BDDAY                                                         
         SLL   R0,25                                                            
         SPACE                                                                  
         CLI   OWRSDAY,0                                                        
         BE    BLD17                                                            
         ZIC   RF,OWRSDAY                                                       
         SPACE                                                                  
         LTR   R0,R0                                                            
         BNM   *+8                                                              
         O     R0,=X'00800000'                                                  
         SLL   R0,1                                                             
         BCT   RF,*-14                                                          
         SPACE                                                                  
BLD17    LTR   R0,R0               REG GOES NEGATIVE WHEN BIT ARRIVES           
         BZ    *+16                                                             
         BM    *+12                                                             
         SLL   R0,1                                                             
         B     *-14                                                             
         SPACE                                                                  
         SR    RE,RE               CLEAR COUNTER                                
         SLL   R0,1                                                             
         LTR   R0,R0               SHIFT TILL NO MORE BITS ON                   
         BZ    *+8                                                              
         BCT   RE,*-10                                                          
         LPR   RE,RE                                                            
         STH   RE,ROTDAYS          AND SAVE DAYS                                
         SPACE                                                                  
         CLI   BUYKEY+3,X'FF'      TEST POOL BUYREC                             
         BNE   BLD60                                                            
         SPACE                                                                  
         LA    R1,24(,R7)          POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
GET61    ICM   RF,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                BLOW UP ON ELEMENT LENGTH ZERO               
         AR    R1,RF               POINT TO NEXT ELEMENT                        
         CLI   0(R1),0             TEST END OF RECORD                           
         BE    GET61ND                                                          
         CLI   0(R1),X'61'         TEST FOR MASTER CLIENT ELEMENT               
         BNE   GET61                                                            
         SPACE                                                                  
         CLI   1(R1),6             THIS ELEM INCLUDE PROD CODE                  
         BL    GET61ND                                                          
         SR    R0,R0                                                            
         ICM   R0,1,5(R1)          SAVE PROD CODE                               
         BZ    GET61ND                                                          
         SPACE                                                                  
         LA    R1,24(,R7)          POINT TO FIRST ELEMENT                       
         SPACE                                                                  
GET61EL  ICM   RF,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                BLOW UP ON ELEMENT LENGTH ZERO               
         AR    R1,RF               POINT TO NEXT ELEMENT                        
         CLI   0(R1),0             TEST END OF RECORD                           
         BE    GET61ND                                                          
         CLI   0(R1),X'0B'         TEST FOR POL ORIG                            
         BL    GET61EL                                                          
         CLI   0(R1),X'0D'         TEST FOR POL FLIP                            
         BH    GET61EL                                                          
         CLI   1(R1),10            UNALLOCATED PRODUCT                          
         BNH   GET61EL                                                          
         STC   R0,10(,R1)          STORE NEW PRODUCT CODE                       
         B     GET61EL                                                          
         SPACE                                                                  
GET61ND  DS   0H                                                                
         EJECT                                                                  
* POL PROCESSING *                                                              
         SPACE                                                                  
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         LA    R6,BDELEM                                                        
         SPACE                                                                  
BLD20    BAS   RE,BUYEL                                                         
         BNE   BLD90                                                            
         CLI   1(R6),10            TEST UNALL                                   
         BNH   BLD20                                                            
         TM    6(R6),X'C0'         TEST MINUS OR MINUSED                        
         BNZ   BLD20                                                            
         MVC   ELDATE,2(R6)        SAVE ELEM START DATE                         
         MVC   ELDATEX,2(R6)       AND PRESET ELEM END DATE                     
*                                                                               
         CLI   SVT1PR1,0           TEST ADJUST ROTATOR DAYS                     
         BE    BLD22               NO                                           
         CLC   SVT1PR1(1),BDDAY    TEST DAYS MATCH                              
         BNE   BLD22                                                            
         SPACE                                                                  
* BACK UP ONE DAY *                                                             
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(2,ELDATE),WORK                                      
         ZIC   R0,SVT1PR2          GET NUMBER OF DAYS TO BACK UP                
         LNR   R0,R0                                                            
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,ELDATE)                                    
*                                                                               
BLD22    CLC   ELDATE,PERENDP      TEST AFTER PERIOD END DATE                   
         BH    BLD20                                                            
*                                                                               
         CLI   0(R6),11            TEST REGEL                                   
         BNE   BLD24                                                            
         GOTO1 DATCON,DMCB,(2,ELDATEX),WORK                                     
         LH    R0,ROTDAYS                                                       
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,ELDATEX)                                   
*                                                                               
BLD24    CLC   ELDATEX,PERSTRP     TEST BEFORE PERIOD START                     
         BL    BLD20                                                            
*                                                                               
         XC    ACLWORK,ACLWORK                                                  
         LA    R4,ACLWORK                                                       
         USING ACLDATA,R4                                                       
*                                                                               
         SPACE                                                                  
         GOTO1 =A(CPY),RR=RB       GO SET COPY CODE IF ANY                      
         EJECT                                                                  
BLD24X   MVC   ACLSLN,11(R6)       SLN                                          
         MVC   BYTE,10(R6)                                                      
         BAS   RE,GETPRD                                                        
         MVC   ACLEBCP1,0(R1)      EBCDIC PRD                                   
         MVC   ACLPRD,BYTE         MAY BE EQUIVALENT PROD                       
         SPACE                                                                  
         CLI   1(R6),18            TEST PIGGYBACK                               
         BNL   BLD24Y               YES                                         
         CLI   BPRD2,X'FF'         TEST PTR = NONE                              
         BE    BLD26                                                            
         CLI   BPRD2,0             TEST PTR REQUIRED                            
         BNE   BLD20                YES, BYPASS                                 
         B     BLD26                                                            
         SPACE                                                                  
BLD24Y   CLI   BPRD2,X'FF'         TEST PTR = NONE                              
         BE    BLD20                BYPASS                                      
         MVC   ACLSLN2,15(R6)      SLN2                                         
         MVC   BYTE,14(R6)                                                      
         BAS   RE,GETPRD                                                        
         MVC   ACLEBCP2,0(R1)                                                   
         MVC   ACLPRD2,BYTE         MAY BE EQUIVALENT PROD                      
         SPACE                                                                  
* MUST BE IN ALPHA SEQ *                                                        
         SPACE                                                                  
         CLC   ACLEBCP1,ACLEBCP2                                                
         BL    BLD25                                                            
         SPACE                                                                  
* REVERSE THE ENTRIES *                                                         
         SPACE                                                                  
         XC    ACLPRD(2),ACLPRD2                                                
         XC    ACLPRD2(2),ACLPRD                                                
         XC    ACLPRD(2),ACLPRD2                                                
         XC    ACLEBCP1,ACLEBCP2                                                
         XC    ACLEBCP2,ACLEBCP1                                                
         XC    ACLEBCP1,ACLEBCP2                                                
         SPACE                                                                  
BLD25    CLI   BPRD2,0             LIMIT ON PTR                                 
         BE    BLD26                NO                                          
         CLC   BPRD2,ACLPRD2                                                    
         BNE   BLD20                                                            
         SPACE                                                                  
* TEST DATA IN TABLE ALREADY *                                                  
         SPACE                                                                  
BLD26    CLI   BPRD,X'FF'          TEST POL REQ                                 
         BE    BLD26X                                                           
         CLC   ACLPRD,BPRD         ELSE MUST MATCH PRD 1                        
         BNE   BLD20                                                            
*                                                                               
BLD26X   L     R4,AIO3                                                          
*                                                                               
BLD28    CLI   ACLPRD,0                                                         
         BE    BLD30                                                            
         CLC   ACLPRD(5),ACLWORK+6  PRD/SLN/PRD2/SLN2/COPY                      
         BE    BLD32                                                            
         LA    R4,L'ACLDATA(R4)                                                 
         B     BLD28                                                            
         SPACE                                                                  
BLD30    L     RE,AIO3                                                          
         LA    RE,1500(,RE)                                                     
         SH    RE,=AL2(L'ACLDATA)                                               
         CR    R4,RE                                                            
         BNH   *+6                                                              
         DC    H'0'                                                             
         MVC   ACLEBCP1(11),ACLWORK                                             
         MVC   ACLFTD,ELDATE                                                    
         MVC   ACLLTD,ELDATEX                                                   
         SPACE 3                                                                
BLD32    CLC   TODAYP,BUYDATE      IF BUY ACTIVE TODAY                          
         BNE   *+8                                                              
         OI    ACLFLG,ACLFTDAY     SET BUY ACTIVE TODAY                         
         SPACE                                                                  
         CLC   ACLFTD,ELDATE                                                    
         BL    *+10                                                             
         MVC   ACLFTD,ELDATE                                                    
         SPACE                                                                  
         CLC   ACLFTD,PERSTRP      TEST PRIOR TO PERIOD START                   
         BH    *+10                                                             
         MVC   ACLFTD,PERSTRP                                                   
*                                                                               
         CLC   ACLLTD,ELDATEX                                                   
         BH    *+10                                                             
         MVC   ACLLTD,ELDATEX                                                   
*                                                                               
         CLC   ACLLTD,PERENDP      TEST AFTER PERIOD END                        
         BL    *+10                                                             
         MVC   ACLLTD,PERENDP                                                   
*                                                                               
         B     BLD20                                                            
         EJECT                                                                  
* NON-POL PROCESSING *                                                          
         SPACE                                                                  
BLD60    XC    ACLWORK,ACLWORK                                                  
         LA    R4,ACLWORK                                                       
         USING ACLDATA,R4                                                       
*                                                                               
         MVC   BYTE,BUYKEY+3                                                    
         BAS   RE,GETPRD                                                        
         MVC   ACLEBCP1,0(R1)                                                   
         MVC   ACLPRD,BYTE         SET PRD                                      
         MVC   ACLSLN,BDSEC        AND SLN                                      
*                                                                               
         SPACE                                                                  
         CLI   BDTIME,0            TEST PIGGYBACK                               
         BNE   BLD62                YES                                         
         CLI   BPRD2,0             LIMIT TO PTR PRD                             
         BE    BLD66                NO                                          
         B     BLD90               BYPASS                                       
         SPACE                                                                  
BLD62    CLI   BPRD2,X'FF'         PTR = NONE                                   
         BE    BLD90                                                            
         SPACE                                                                  
         MVC   ACLSLN,BDTIME       SET ACTIVE PRD SLN                           
         MVI   ELCDLO,4                                                         
         MVI   ELCDHI,4                                                         
         LA    R6,BDELEM                                                        
         BAS   RE,BUYEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,2(R6)                                                       
         BAS   RE,GETPRD                                                        
         MVC   ACLEBCP2,0(R1)      EBCDIC PRD                                   
         MVC   ACLPRD2,BYTE         MAY BE EQUIVALENT PROD                      
         SPACE                                                                  
         MVC   ACLSLN2,4(R6)       SLN2                                         
         CLC   ACLEBCP1,ACLEBCP2                                                
         BL    BLD64                                                            
         MVC   ACLEBCP2,ACLEBCP1                                                
         MVC   ACLPRD2(2),ACLPRD                                                
         MVC   ACLEBCP1,6(R6)                                                   
         MVC   ACLPRD,2(R6)                                                     
         MVC   ACLSLN,4(R6)                                                     
BLD64    CLI   BPRD2,0             LIMIT ON PTR                                 
         BE    BLD66                NO                                          
         CLC   BPRD2,ACLPRD2                                                    
         BNE   BLD90                                                            
         SPACE                                                                  
BLD66    CLC   ACLPRD,BPRD         MUST MATCH PRD 1                             
         BNE   BLD90               IF NOT, SKIP BUY                             
*                                                                               
         GOTO1 =A(CPY),RR=RB       GO SET COPY CODE IF ANY                      
*                                                                               
         MVC   ACLFTD,=2X'FF'      FORCE HIGH START DATE/0 END DATE             
*                                                                               
BLD68    MVI   ELCDLO,6                                                         
         MVI   ELCDHI,8                                                         
         LA    R6,BDELEM                                                        
         BAS   RE,BUYEL                                                         
         MVC   FRSTDATE,2(R6)                                                   
*                                                                               
         LA    R6,BDELEM                                                        
*                                                                               
         XC    ELDATE,ELDATE                                                    
         XC    ELCOUNT,ELCOUNT                                                  
*                                                                               
BLD70    BAS   RE,BUYEL                                                         
         BNE   BLD72                                                            
         CLC   ELDATE,2(R6)        TEST SAME DATE                               
         BNE   BLD72                                                            
*                                                                               
BLD71    ZIC   R0,7(R6)            GET NUMBER OF SPOTS                          
         TM    6(R6),X'80'         TEST MINUS                                   
         BZ    *+6                                                              
         LCR   R0,R0                                                            
         AH    R0,ELCOUNT                                                       
         STH   R0,ELCOUNT                                                       
         B     BLD70                                                            
*                                                                               
BLD72    OC    ELCOUNT,ELCOUNT     TEST ANY SPOTS THIS DATE                     
         BZ    BLD74X              NO                                           
         MVC   ELDATEX,ELDATE      PRESET END DATE                              
*                                                                               
         CLC   ELDATE,PERENDP      ELEM TO PERIOD END                           
         BH    BLD74X                                                           
*                                                                               
         CLI   SVT1PR1,0           TEST ADJUST ROTATORS                         
         BE    BLD72B                                                           
         CLC   SVT1PR1(1),BDDAY    TEST DAYS MATCH                              
         BNE   BLD72B                                                           
         SPACE                                                                  
* ADJUST START DATE *                                                           
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(2,ELDATE),WORK                                      
         ZIC   R0,SVT1PR2                                                       
         LNR   R0,R0                                                            
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,ELDATE)                                    
*                                                                               
BLD72B   GOTO1 DATCON,DMCB,(2,ELDATEX),WORK   CALCULATE LAST TLCST              
         LH    R0,ROTDAYS                                                       
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,ELDATEX)                                   
*                                                                               
         CLC   ELDATEX,PERSTRP     LAST TLCST DATE TO PERIOD START              
         BL    BLD74X                                                           
*                                                                               
         CLC   ELDATE,ACLFTD       ELEM TO FIRST TLCST                          
         BH    BLD74                                                            
         MVC   ACLFTD,ELDATE                                                    
         CLC   ACLFTD,PERSTRP      CHECK PRIOR TO PERIOD START                  
         BH    *+10                IF LOW,                                      
         MVC   ACLFTD,PERSTRP      FORCE FIRST TLCST = PER START                
*                                                                               
BLD74    CLC   ELDATEX,ACLLTD      ELEM TO LAST TLCST                           
         BL    BLD74X                                                           
         MVC   ACLLTD,ELDATEX                                                   
*                                                                               
         CLC   ACLLTD,PERENDP      CHECK AFTER FLIGHT END                       
         BNH   *+10                IF HIGH,                                     
         MVC   ACLLTD,PERENDP      FORCE LAST TLCST = FLT END                   
*                                                                               
BLD74X   CLI   0(R6),0             TEST REACHED EOR                             
         BE    BLD76                                                            
         MVC   ELDATE,2(R6)        SAVE NEW DATE                                
         XC    ELCOUNT,ELCOUNT                                                  
         B     BLD71                                                            
         SPACE                                                                  
* FIND ACLTAB ENTRY *                                                           
         SPACE                                                                  
BLD76    OC    ACLLTD,ACLLTD       TEST FOR ACTIVITY                            
         BZ    BLD90               NONE - IGNORE                                
         L     R4,AIO3                                                          
*                                                                               
BLD78    CLI   ACLPRD,0                                                         
         BE    BLD80                                                            
         CLC   ACLPRD(5),ACLWORK+6    PRD/SLN/PRD2/SLN2/COPY                    
         BE    BLD82                                                            
         SPACE                                                                  
BLD79    LA    R4,L'ACLDATA(R4)                                                 
         B     BLD78                                                            
*                                                                               
BLD80    MVC   0(L'ACLDATA,R4),ACLWORK                                          
         CLC   TODAYP,BUYDATE      IF BUY ACTIVE TODAY                          
         BNE   BLD90                                                            
         OI    ACLFLG,ACLFTDAY     SET BUY ACTIVE TODAY                         
         B     BLD90                                                            
         SPACE                                                                  
BLD82    CLC   TODAYP,BUYDATE      IF BUY ACTIVE TODAY                          
         BNE   *+8                                                              
         OI    ACLFLG,ACLFTDAY     SET BUY ACTIVE TODAY                         
         SPACE                                                                  
         LA    RE,ACLFTD-ACLDATA+ACLWORK                                        
         CLC   ACLFTD,0(RE)                                                     
         BL    *+10                                                             
         MVC   ACLFTD,0(RE)                                                     
         SPACE                                                                  
         LA    RE,ACLLTD-ACLDATA+ACLWORK                                        
         CLC   ACLLTD,0(RE)                                                     
         BH    *+10                                                             
         MVC   ACLLTD,0(RE)                                                     
         SPACE                                                                  
BLD90    MVC   KEYSAVE,KEY         SAVE BUY DATA                                
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 SEQ                                                              
         B     BLD02A                                                           
         EJECT                                                                  
* CHANGE OF STATION *                                                           
         SPACE                                                                  
BLD92    MVC   SVKEY,KEY           SAVE KEY THAT GAVE BREAK                     
*                                                                               
         L     R4,AIO3                                                          
         CLI   ACLPRD,0            TEST ACTIVITY IN TABLE                       
         BE    BLD120X             NO - TEST TO CONTINUE                        
         SPACE                                                                  
* COUNT LINES NEEDED FOR DISPLAY                                                
         SPACE                                                                  
BLD92X   OI    SVOPT2,OP2ISACT     SET ON ACTIVITY SW                           
         SPACE                                                                  
         MVC   TRBUYKEY,BUYRECD    SAVE BUY KEY FOR SVTABLE                     
         SPACE                                                                  
* BLANK COPY CODES WITHOUT PATTERNS IF COPY CODE = DAYPART *                    
         SPACE                                                                  
BLD95    CLI   SVT1PR8,C'Y'        TEST AUTO P/B PATTN                          
         BE    BLD96                                                            
         CLI   SVPROF11,C'A'       TEST ADJACENCY=COPY CODE                     
         BE    BLD95C                                                           
         CLI   SVPROF11,C'D'       TEST DPT=COPY CODE                           
         BE    BLD95C                                                           
         CLI   SVPROF11,C'P'       TEST DPT=1ST CHAR OF PROGRAM CODE            
         BE    BLD95C                                                           
         CLI   SVPROF11,C'Y'       TEST DPT=COPY CODE                           
         BNE   BLD96                                                            
         SPACE                                                                  
* CHECK WHICH DAYPARTS HAVE PATTERNS *                                          
         SPACE                                                                  
BLD95C   GOTO1 =A(CHKPTN),RR=RB                                                 
         EJECT                                                                  
* SORT BY COPY CODE/SLN2/SLN1/EBCDIC PRD *                                      
         SPACE                                                                  
BLD96    L     R4,AIO3                                                          
         SR    R0,R0                                                            
*                                                                               
         CLI   0(R4),0                                                          
         BE    *+12                                                             
         LA    R4,L'ACLDATA(R4)                                                 
         BCT   R0,*-12                                                          
*                                                                               
         LPR   R0,R0                                                            
         GOTO1 XSORT,DMCB,AIO3,(R0),L'ACLDATA,1,10  COPY CODE                   
         GOTO1 (RF),(R1),,,,,9                      SLN2                        
         GOTO1 (RF),(R1),,,,,7                      SLN1                        
         GOTO1 (RF),(R1),,,,6,0                     ALPHA PROD1 & 2             
         SPACE                                                                  
         CLI   SVT2PR12,C'Y'       SORT BY DATE HIGH                            
         BNE   BLD100                                                           
         SPACE                                                                  
         GOTO1 (RF),(R1),,,,4,11                   DATE                         
         EJECT                                                                  
* BUILD SVTABLE ENTRIES FROM ACTIVITY LIST *                                    
         SPACE                                                                  
BLD100   DS    0H                                                               
         L     R4,AIO3                                                          
         USING ACLDATA,R4                                                       
         SPACE                                                                  
         L     R7,ASVTABLE                                                      
         USING SVTABLED,R7                                                      
         SPACE                                                                  
BLD100M  OC    SVTBLINE,SVTBLINE                                                
         BZ    *+12                                                             
         LA    R7,SVTBNEXT                                                      
         B     BLD100M                                                          
*                                                                               
         L     R4,AIO3                                                          
         USING ACLDATA,R4                                                       
*                                                                               
         MVC   SVTBMKST,TRBUYKEY+4   MOVE SAVED MKT/STA                         
         SPACE                                                                  
         CLI   SVTBMKST+2,X'F0'  THIS A CABLE STATION                           
         BL    BLD100S              NO                                          
         NI    SVTBMKST+4,X'80'                                                 
         SPACE                                                                  
BLD100S  DS   0H                                                                
         XC    WORK,WORK                                                        
         XC    STANET,STANET                                                    
         GOTO1 MSUNPK,DMCB,(X'80',SVTBMKST),WORK,WORK+4                         
         MVC   SVTBSTA,WORK+4                                                   
         CLI   SVTBSTA+4,C' '                                                   
         BNE   *+8                                                              
         MVI   SVTBSTA+4,C'T'                                                   
         SPACE                                                                  
         CLC   WORK+9(3),SPACES                                                 
         BE    BLD101                                                           
         MVC   STANET(4),WORK+4                                                 
         MVI   STANET+4,C'/'                                                    
         MVC   STANET+5(3),WORK+9                                               
         OI    SVTBIND2,SVTBICAB   SET ON CABLE HEAD STATION                    
         SPACE                                                                  
* READ STATION MASTER RECORD FOR AFFL/TYPE *                                    
         SPACE                                                                  
BLD101   XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),TRAMED                                                  
         MVC   KEY+2(5),SVTBSTA                                                 
         MVC   KEY+7(2),AGENCY                                                  
         MVC   KEY+9(3),QCLT                                                    
         MVC   AIO,AIO1                                                         
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
*                                                                               
         L     R6,AIO                                                           
         USING STARECD,R6                                                       
         SPACE                                                                  
         MVC   SVTBAFFL,SCANNTWK                                                
         CLI   SPOTCAN,C'C'        CANADIAN AGCY?                               
         BE    BLD101A                                                          
         MVC   SVTBAFFL,SNETWRK                                                 
*                                                                               
BLD101A  DS    0H                  CANADIAN AGCY?                               
         MVC   SVTBTYPE,STYPE                                                   
         MVC   QMKT,SMKT           THIS FORCES STATION MARKET                   
         SPACE                                                                  
         CLI   SVTBSTA,C'0'        THIS A CABLE STATION                         
         BL    BLD101C              NO                                          
         SPACE                                                                  
         CLC   =C'7000',SVTBSTA    THIS PART OF USER CODED STATIONS             
         BH    *+14                 NO                                          
         SPACE                                                                  
         CLC   =C'7500',SVTBSTA    THIS PART OF USER CODED STATIONS             
         BNL   BLD101C              YES                                         
         SPACE                                                                  
         OI    SVTBIND2,SVTBICGR     THIS IS A CABLE GROUP STATION              
         XC    SVTBAFFL,SVTBAFFL                                                
         MVI   SVTBTYPE,0                                                       
         DROP  R6                                                               
         SPACE                                                                  
* FORMAT STATION FOR PRINTING *                                                 
         SPACE                                                                  
BLD101C  DS    0H                                                               
         MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),SVTBSTA                                               
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),SVTBSTA+4                                                
         MVI   3(RE),C'V'                                                       
         CLI   QMED,C'T'                                                        
         BE    BLD102                                                           
         MVI   3(RE),C'M'                                                       
         CLI   QMED,C'R'                                                        
         BE    BLD102                                                           
         MVI   3(RE),C' '                                                       
*                                                                               
BLD102   DS    0H                                                               
*        TM    SVTBIND2,SVTBICGR     THIS IS A CABLE GROUP STATION              
*        BO    BLD104                                                           
         SPACE                                                                  
* READ MARKET RECORD *                                                          
         SPACE                                                                  
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         GOTO1 (RF),(R1)                                                        
*                                                                               
         L     R6,AIO                                                           
         USING MKTRECD,R6                                                       
*                                                                               
         MVC   MKTNM,MKTNAME                                                    
         DROP  R6                                                               
*                                                                               
BLD104   MVC   SVTBPRD(4),ACLPRD   PRD/SLN/PRD2/SLN2                            
*                                                                               
         GOTO1 DATCON,DMCB,(2,ACLFTD),(3,SVTBSTR)                               
         GOTO1 (RF),(R1),(2,ACLLTD),(3,SVTBEND)                                 
         SPACE                                                                  
         CLI   SVT1PR13,C'Y'       USE FLT/EST DATES FOR ACTIVITY               
         BNE   *+10                 NO                                          
         MVC   SVTBSTR(6),SVGENDTS USE INSTR PERIOD AS FTD/LTD                  
         SPACE                                                                  
         MVC   SVTBCOPY,ACLCOPY                                                 
*                                                                               
         L     R0,=F'-1'           SET DUMMY LINE ADDRESS FOR OFFLINE           
         ST    R0,SVTBLINE                                                      
         CLI   OFFLINE,C'Y'                                                     
         BE    BLD110                                                           
         CLI   SVBIGSW,C'Y'        TEST EXTENDED SVTABLE                        
         BNE   *+12                                                             
         CLI   NEXTSELF,0          TEST BEYOND USER SCREEN                      
         BNE   BLD110              YES - SKIP DISPLAY                           
*                                                                               
         L     R2,NEXTSELF         POINT TO SELECT FIELD                        
         S     R2,ATWA                                                          
         ST    R2,SVTBLINE         SET REL LINE DSPL                            
         A     R2,ATWA                                                          
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO DISPLAY AREA                        
*                                                                               
         MVC   DSPSTA,STAPRNT      MOVE STATION TO DISPLAY LINE                 
         OC    STANET,STANET                                                    
         BZ    BLD105                                                           
         MVC   DSPSTA(8),STANET                                                 
         SPACE                                                                  
BLD105   MVC   DSPMKT,MKTNM                                                     
*                                                                               
         MVC   STAPRNT,SPACES      NAMES APPEAR ONCE ONLY                       
         MVC   STANET,SPACES                                                    
         MVC   MKTNM,SPACES                                                     
*                                                                               
         USING DSPLINED,R2                                                      
         SPACE                                                                  
* BUILD DISPLAY LINE *                                                          
         SPACE                                                                  
         MVC   DSPPRD(3),ACLEBCP1                                               
         LA    R1,DSPPRD+2                                                      
         ZIC   R0,SVTBSLN                                                       
         BAS   RE,FMTSLN                                                        
*                                                                               
         MVC   DSPPRD2(3),ACLEBCP2                                              
         LA    R1,DSPPRD2+2                                                     
         ZIC   R0,SVTBSLN2                                                      
         BAS   RE,FMTSLN                                                        
*                                                                               
         LA    R1,SVTBSTR                                                       
         BAS   RE,FMTDT                                                         
         MVC   DSPFTD,WORK                                                      
*                                                                               
         LA    R1,SVTBEND                                                       
         BAS   RE,FMTDT                                                         
         MVC   DSPLTD,WORK                                                      
*                                                                               
         MVC   DSPCOPY,SVTBCOPY                                                 
         CLI   SVPROF11,C'E'       TEST COPY CODE = EST                         
         BNE   BLD106                                                           
         ZIC   R0,ACLCOPY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSPCOPY-1(3),DUB                                                 
*                                                                               
BLD106   OI    6(R2),X'80'         SET XMT                                      
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ST    R2,NEXTSELF         SET NEXT LINE ADDRESS                        
*                                                                               
         CLI   SVBIGSW,C'Y'        TEST EXTENDED LIST                           
         BNE   BLD110              NO                                           
         CLI   0(R2),9             TEST REACHED LAST LINE YET                   
         BH    BLD110              NO                                           
         SR    R2,R0               BACK UP TO PREVIOUS FIELD                    
         MVC   8(L'TRADSP1,R2),SPACES                                           
         MVC   8(41,R2),=C'** ADDITIONAL DATA CANNOT BE DISPLAYED **'           
         MVI   NEXTSELF,X'FF'      SET FLAG FOR BEYOND SCREEN                   
*                                                                               
BLD110   LA    R4,L'ACLDATA(R4)                                                 
         CLI   ACLPRD,0            TEST MORE DATA                               
         BE    BLD120              NO                                           
         CLI   0(R2),9             TEST REACHED EOS                             
         BH    BLD114               NO                                          
         TM    SVOPT3,OPT3MKLR                                                  
         BZ    *+8                                                              
         NI    SVOPT3,X'FF'-OPT3MKLR  OFF MARKET LIST READ                      
         CLI   SVBIGSW,C'Y'        TEST KNOW IT WON'T FIT                       
         BE    *+6                                                              
         DC    H'0'                REALLY SHOULDN'T GET HERE                    
         SPACE                                                                  
* MORE DATA *                                                                   
         SPACE                                                                  
BLD114   MVC   SVTBNEXT,SVTBDATA   COPY CURRENT DATA TO NEXT                    
         LA    R7,SVTBNEXT                                                      
         C     R7,ASVTABLX                                                      
         BL    BLD104                                                           
BLD118   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD+10(30),=C'TOO MUCH DATA-REQUEST OFF-LINE'                
         MVI   SVBIGSW,0                                                        
         MVC   CONHEAD(9),=C'* ERROR *'                                         
         GOTO1 ERREX2                                                           
         SPACE                                                                  
* NO MORE DATA - FORCE NEXT STA AND CONTINUE *                                  
         SPACE                                                                  
BLD120   CLI   OFFLINE,C'Y'                                                     
         BE    BLD120T             PROCESS ONE STATION AT A TIME                
         CLI   SVBIGSW,C'Y'        TEST EXTENDED SCREEN                         
         BNE   BLD120X                                                          
         SPACE                                                                  
* NEED TO WRITE TWA2 WITH EXTENDED LIST BEFORE EXIT *                           
         SPACE                                                                  
         XC    DMCB(24),DMCB                                                    
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    SET TERMINAL NUMBER                          
         MVI   DMCB+8,2            SET PAGE                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,ASVTABLE                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
BLD120T  XC    FILENAME,FILENAME   SWITCH BACK TO STRAFFIC                      
         SPACE                                                                  
         CLC   SVKEY(4),BAGYMD     TEST SAME A-M/C/P                            
         BE    BLDX                YES - LEAVE SVKEY SET                        
         SPACE                                                                  
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    *+12                 YES, CK POL                                 
         CLI   SVPROF15,C'Y'       THIS TRAFFIC BUYS                            
         BNE   BLD120U                                                          
         CLI   BPRD,X'FF'          THIS TRAFFIC POL INSTR                       
         BNE   BLD120U                                                          
         CLC   SVKEY(3),BAGYMD     TEST SAME A-M/CLT                            
         BE    BLDX                                                             
         SPACE                                                                  
BLD120U  MVI   SVKEY,X'FF'         ELSE SET NO CONTINUATION                     
         B     BLDX                                                             
         SPACE                                                                  
BLD120V  XC    SVTBDATA,SVTBDATA                                                
         OI    1(R2),X'20'         SET PROTECTED AGAIN                          
         NI    6(R2),X'FF'-X'80'   SET OFF XMT                                  
         SPACE                                                                  
BLD120X  CLC   SVKEY(3),BAGYMD     TEST SAME A-M/CLT                            
         BNE   BLD121               NO - QUIT                                   
         SPACE                                                                  
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    BLD01                YES - CONTINUE                              
         CLI   SVPROF13,C'Y'       RUNNING PROD EQUIV                           
         BE    BLD01                YES - CONTINUE                              
         SPACE                                                                  
         CLC   SVKEY+3(1),BPRD     TEST SAME PRODUCT                            
         BE    BLD01                YES - CONTINUE                              
         SPACE                                                                  
BLD121   CLI   SVPROF15,C'Y'       THIS TRAFFIC BUYS                            
         BE    *+12                                                             
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BZ    BLD122                                                           
         SPACE                                                                  
         CLI   BPRD,X'FF'          THIS TRAFFIC POL INSTR                       
         BNE   BLD122                                                           
         CLC   SVKEY(3),BAGYMD     TEST SAME A-M/CLT                            
         BE    BLD01                                                            
         SPACE                                                                  
BLD122   XC    SVTBNEXT,SVTBNEXT   CLEAR NEXT SVTB ENTRY                        
         MVI   SVKEY,X'FF'         SET NO CONTINUATION                          
         SPACE                                                                  
BLDX     MVC   SYSDIR(3),=C'TRF'                                                
         MVC   SYSFIL(3),=C'TRF'                                                
         SPACE                                                                  
         XIT1                                                                   
         EJECT                                                                  
* HEADHOOK ROUTINE                                                              
         SPACE                                                                  
HDHK     NMOD1 0,**HDHK**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
         TM    SVOPTSW,OPTCABLE    CABLE STATION ONLY                           
         BO    HDHK10                                                           
         MVC   PMKT,QMKT                                                        
         SPACE                                                                  
HDHK10   DS    0H                                                               
         MVC   PMKTNM,MKTNM                                                     
         SPACE                                                                  
         MVC   H2+10(1),QMED                                                    
         MVC   H2+15(L'MEDNM),MEDNM                                             
         MVC   H3+10(L'QCLT),QCLT                                               
         MVC   H3+15(L'CLTNM),CLTNM                                             
         MVC   H3+37(8),=C'PERIOD ='                                            
         GOTO1 DATCON,DMCB,(3,PERSTART),(5,H3+46)                               
         MVI   H3+55,C'-'                                                       
         GOTO1 (RF),(R1),(3,PEREND),(5,H3+57)                                   
         SPACE                                                                  
         CLI   SVMGRPS,0           BY MARKET GROUP                              
         BE    HDHK14                                                           
         SPACE                                                                  
         MVC   H4+2(12),=C'MARKET GROUP'                                        
         MVC   H4+21(1),SVMGRPS                                                 
         SR    R0,R0                                                            
         ICM   R0,3,SVCMLMGR                                                    
         SLL   R0,4                                                             
         STCM  R0,7,DUB                                                         
         OI    DUB+2,X'0F'                                                      
         UNPK  H4+22(4),DUB(3)                                                  
         SPACE                                                                  
HDHK14   DS    0H                                                               
         LA    R5,SVCMLCML                                                      
         SPACE                                                                  
         BRAS  RE,FCML                                                          
         SPACE                                                                  
         CLC   AGENCY,=C'H9'       THIS STARCOM?                                
         BNE   HDHK16               NO                                          
         SPACE                                                                  
         OC    SVBASIC,SVBASIC                                                  
         BZ    HDHK16                                                           
         MVC   H4+2(14),=C'BASIC ISCI/LEN'                                      
         MVC   H4+21(16),SVBASIC                                                
         SPACE                                                                  
HDHK16   DS    0H                                                               
         LA    R6,H5                                                            
         MVC   2(17,R6),=C'COMMERCIAL ID/LEN'                                   
         MVC   132+2(16,R6),=C'COMMERCIAL TITLE'                                
         SPACE                                                                  
         MVC   21(8,R6),SVCMLCML                                                
         ZIC   R0,SVCMLSLN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  30(3,R6),DUB                                                     
         CLI   30(R6),C'0'                                                      
         BNE   *+8                                                              
         MVI   30(R6),C' '                                                      
         SPACE                                                                  
         CLC   AGENCY,=C'H9'       THIS STARCOM?                                
         BNE   HDHK20               NO                                          
         SPACE                                                                  
         OC    SVCMLCLT,SVCMLCLT                                                
         BZ    HDHK20                                                           
         SPACE                                                                  
         MVC   21+1+L'SVCMLCLT(3,R6),30(R6) MOVE CML LEN                        
         MVC   21(L'SVCMLCLT,R6),SVCMLCLT                                       
         SPACE                                                                  
HDHK20   DS   0H                                                                
         MVC   132+21(L'SVCMLDSC,R6),SVCMLDSC                                   
         LA    R1,264(,R6)                                                      
         SPACE                                                                  
         OC    SVCMLDS2,SVCMLDS2                                                
         BZ    *+14                                                             
         MVC   21(L'SVCMLDS2,R1),SVCMLDS2                                       
         LA    R1,132(,R1)                                                      
         SPACE                                                                  
         OC    SVCMLDS3,SVCMLDS3                                                
         BZ    *+14                                                             
         MVC   21(L'SVCMLDS3,R1),SVCMLDS3                                       
         LA    R1,132(,R1)                                                      
         SPACE                                                                  
         CLC   AGENCY,=C'H9'       THIS STARCOM?                                
         BE    HDHK24               YES, ALREADY DONE                           
         SPACE                                                                  
         OC    SVCMLCLT,SVCMLCLT                                                
         BZ    HDHK24                                                           
         MVC   21(L'SVCMLCLT,R1),SVCMLCLT                                       
         SPACE                                                                  
HDHK24   DS   0H                                                                
         OC    SVCMLCML+8(8),SVCMLCML+8                                         
         BZ    HDHK50                                                           
         SPACE                                                                  
         LA    R5,SVCMLCML+8                                                    
         BRAS  RE,FCML                                                          
         SPACE                                                                  
         MVC   50(8,R6),SVCMLCML+8                                              
         ZIC   R0,SVCMLSLN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  60(3,R6),DUB                                                     
         CLI   60(R6),C'0'                                                      
         BNE   *+8                                                              
         MVI   60(R6),C' '                                                      
HDHK30   DS    0H                                                               
         SPACE                                                                  
         CLC   AGENCY,=C'H9'       THIS STARCOM?                                
         BNE   HDHK40               NO                                          
         SPACE                                                                  
         OC    SVCMLCLT,SVCMLCLT                                                
         BZ    HDHK40                                                           
         SPACE                                                                  
         MVC   50+1+L'SVCMLCLT(3,R6),60(R6)                                     
         MVC   50(L'SVCMLCLT,R6),SVCMLCLT                                       
         SPACE                                                                  
HDHK40   DS   0H                                                                
         MVC   132+50(L'SVCMLDSC,R6),SVCMLDSC                                   
         LA    R1,264(,R6)                                                      
         SPACE                                                                  
         OC    SVCMLDS2,SVCMLDS2                                                
         BZ    *+14                                                             
         MVC   50(L'SVCMLDS2,R1),SVCMLDS2                                       
         LA    R1,132(,R1)                                                      
         SPACE                                                                  
         OC    SVCMLDS3,SVCMLDS3                                                
         BZ    *+14                                                             
         MVC   50(L'SVCMLDS3,R1),SVCMLDS3                                       
         LA    R1,132(,R1)                                                      
         SPACE                                                                  
         CLC   AGENCY,=C'H9'       THIS STARCOM?                                
         BE    HDHK50               YES, ALRADY DONE                            
         SPACE                                                                  
         CLI   SVT1PR7,C'Y'        SHOW CLIENT COMMERCIAL NUMBER                
         BNE   HDHK50               NO                                          
         SPACE                                                                  
         OC    SVCMLCLT,SVCMLCLT                                                
         BZ    HDHK50                                                           
         MVC   50(L'SVCMLCLT,R1),SVCMLCLT                                       
         SPACE                                                                  
HDHK50   CLC   SPACES,0(R6)        BLANK LINE                                   
         BE    HDHK60                                                           
         SPACE                                                                  
         LA    R6,132(,R6)                                                      
         B     HDHK50                                                           
         SPACE                                                                  
HDHK60   CLI   BPRD,0                                                           
         BE    HDHK70                                                           
         MVC   2(7,R6),=C'PRODUCT'                                              
         MVC   11(3,R6),QPRD                                                    
         MVC   16(L'PRDNM,R6),PRDNM                                             
         LA    R6,132(,R6)                                                      
         SPACE                                                                  
HDHK70   CLI   BPRD2,0                                                          
         BE    HDHK80                                                           
         MVC   2(7,R6),=C'PARTNER'                                              
         MVC   11(3,R6),QPRD2                                                   
         MVC   16(L'PRD2NM,R6),PRD2NM                                           
         LA    R6,132(,R6)                                                      
         SPACE                                                                  
HDHK80   CLI   OPTEST,0                                                         
         BE    HDHK90                                                           
         MVC   2(8,R6),=C'ESTIMATE'                                             
         ZIC   R0,OPTEST                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  11(3,R6),DUB                                                     
         LA    R6,132(,R6)                                                      
         SPACE                                                                  
HDHK90   MVC   MID1+13(6),=C'MARKET'                                            
         SPACE                                                                  
         TM    SVOPTSW,OPTCABLE    CABLE STATION ONLY                           
         BZ    *+10                                                             
         MVC   MID1+13(6),=C'CABLE '                                            
         SPACE                                                                  
         MVC   MID2+13(6),DASHES                                                
         MVC   MID1+23(11),=C'MARKET-NAME'                                      
         MVC   MID2+23(24),DASHES                                               
         SPACE                                                                  
         LA    R2,PMKTUNTS-P+MID1                                               
         SPACE                                                                  
         TM    SVOPTSW,OPTUNIT                                                  
         BZ    HDHK92                                                           
         SPACE                                                                  
         MVC   1(5,R2),=C'UNITS'                                                
         MVC   132+1(5,R2),DASHES                                               
         LA    R2,10(,R2)                                                       
         SPACE                                                                  
HDHK92   DS    0H                                                               
         TM    SVOPTSW,OPTCABLE                                                 
         BZ    HDHK94                                                           
         SPACE                                                                  
                                                                                
         MVC   0(17,R2),=C'CABLE SYSTEM NAME'                                   
         MVC   132(17,R2),DASHES                                                
         LA    R2,30(,R2)                                                       
         SPACE                                                                  
HDHK94   MVC   0(12,R2),=C'ACTIVE DATES'                                        
         MVC   132(14,R2),DASHES                                                
         SPACE                                                                  
         TM    SVOPTSW,OPTESTP                                                  
         BZ    HDHKX                                                            
         SPACE                                                                  
         MVC   PESTS-PACTDTES(19,R2),=C'ESTIMATE(S) COVERED'                    
         MVC   132+PESTS-PACTDTES(19,R2),DASHES                                 
         SPACE                                                                  
HDHKX    XIT1                                                                   
DASHES   DC    24C'-'                                                           
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPTRINST                                                       
         EJECT                                                                  
       ++INCLUDE SPTRPAT                                                        
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKG                                                       
         EJECT                                                                  
* INCLUDED DSECTS                                                               
* INCLUDE DDSPOOLD                                                              
* INCLUDE DDSPLWORKD                                                            
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRA8FD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
* INCLUDED DSECTS                                                               
* INCLUDE SPTRAWORKD                                                            
         PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
       ++INCLUDE SPSTAPACKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
TSAROFF  DS    A                   TSAROFF CORE RESIDENT T00A7D                 
TSARBUFL DS    A(L'ELEM*5000)      BUFFER LENGTH                                
TSARBUFF DS    A                   ADDRESS OF GETMAIN BUFFER                    
SPTR49RR DS    F                                                                
SVR1R2   DS   2F                                                                
SVR6     DS    F                                                                
SVRE     DS    F                                                                
         SPACE                                                                  
CMLSTA   DS    F                   ADDRESS OF COMMERCIAL LIST TABLE             
CMLSTX   DS    F                                                                
CBLSTB   DS    F                                                                
CBLSTBX  DS    F                                                                
CBLNAM   DS    F                                                                
CBLNAMX  DS    F                                                                
SVCBLIND DS    H                                                                
RMGMKT   DS    XL2                                                              
RMGMGR   DS    XL2                                                              
RMGKEY   DS    CL24                                                             
TOTUNTS  DS    F                                                                
MKTUNTS  DS    H                   UNITS FIELD FROM MARKET RECORD               
FLDH     DS    CL8                                                              
FLD      DS    CL32                                                             
         SPACE                                                                  
* SAVED DATES FROM INSTRUCTION RECAP SUBEL                                      
         SPACE                                                                  
SVTCDTS  DS    XL4                                                              
         SPACE                                                                  
* FILTERING ON ALL OR PART OF FOLLOWING COMMERCIAL CODE                         
         SPACE                                                                  
SVCMML   DS    CL8                                                              
SVCMMLN  DS    XL1                 LENGTH-1 OF COMPARE FOR CODE ABOVE           
         SPACE                                                                  
* USED TO COMPARE LAST TO NEXT                                                  
         SPACE                                                                  
HOLDCOMP DS   0CL(L'CMLCOMP)                                                    
HOLDALL  DS    CL(L'CMLENT)                                                     
         SPACE                                                                  
* CURRENT MARKET GROUP/COMMERCIAL (OR PAIR) BEING LISTED                        
         SPACE                                                                  
SVCMLMGR DS    XL2                                                              
SVCMLCML DS    CL16                                                             
         SPACE                                                                  
SVCMLDSC DS    CL16                15 BYTES CMML TITLE, 1 DELETE IND            
SVCMLDS2 DS    CL20                20 BYTES CMML DESC 2                         
SVCMLDS3 DS    CL20                20 BYTES CMML DESC 3                         
SVCMLCLT DS    CL20                                                             
SVCMLSLN DS    XL1                                                              
         SPACE                                                                  
SVP      DS    CL132                                                            
SVMKT    DS    XL2                                                              
SVBASIC  DS    CL16                                                             
         SPACE                                                                  
OPTEST   DS    XL1                                                              
SVOPTSW  DS    XL1                                                              
OPTESTP  EQU   X'80'              PRINT ALL ESTIMATES                           
OPTMGRP  EQU   X'40'              PRINT BY MARKET GROUP                         
OPTDOWN  EQU   X'20'              DOWNLOAD OPTION                               
OPTUNIT  EQU   X'10'              SHOW UNITS FROM MARKET RECORD                 
OPTCABLE EQU   X'08'              SHOW CABLE STATIONS ONLY                      
OPTSTA   EQU   X'04'              SHOW STD STATIONS ONLY                        
OPTRACE  EQU   X'02'              TRACE                                         
         SPACE                                                                  
FIRSTSW  DS    XL1                                                              
         SPACE                                                                  
* PERIOD FILTER START AND END DATES                                             
         SPACE                                                                  
PERSTART DS    XL3                                                              
PEREND   DS    XL3                                                              
PERSTRP  DS    XL2                                                              
PERENDP  DS    XL2                                                              
         SPACE                                                                  
SVBCLT   DS    XL2                                                              
         SPACE                                                                  
SVMGRPS  DS    CL1                                                              
SVGRPKEY DS   0XL8                                                              
SVPGRP   DS    XL3                                                              
SVMGRP   DS    XL3                                                              
SVMGRPMK DS    XL2                                                              
         SPACE                                                                  
MGRPTITL DS    CL12                ROOM FOR MGRP TITLE                          
HOLDATES DS    CL17                                                             
         SPACE                                                                  
         DS    0D                                                               
TSARBLK  DS    CL(TSARDL2)                                                      
*                                                                               
* LIST OF COMMERCIALS BY MARKET, FTD, LTD, AND STATION                          
* BUILT HERE FOR NOW, OR AT DUMMY OFFLINE                                       
*                                                                               
CMLIST   DS    0D                                                               
         SPACE                                                                  
* OFFLINE PRINT LINE LAYOUT *                                                   
         SPACE                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
         DS    CL5                 WAS MARKET GROUP                             
         DS    CL6                                                              
PMKT     DS    CL4                                                              
         DS    CL6                                                              
PMKTNM   DS    CL24                                                             
         DS    CL3                                                              
PMKTUNTS DS    CL6                                                              
         DS    CL4                                                              
PACTDTES DS    CL16                                                             
         DS    CL4                                                              
PESTS    DS    CL20                                                             
         SPACE                                                                  
* CML LIST *                                                                    
         SPACE                                                                  
CMLISTD  DSECT                                                                  
CMLENT   DS    0XL32                                                            
CMLCOMP  DS   0XL23                MGRP, BOTH CMLS, MKT, OPTIONAL CABLE         
CMLCOMPA DS   0XL29                ABOVE + DATES                                
CMLMGR   DS    XL2                                                              
CMLCML   DS    CL8                                                              
CMLCML2  DS    CL8                                                              
CMLMKT   DS    XL2                                                              
CMLCSTA  DS    XL3                 CABLE STATION IF OPT CABLE                   
         SPACE                                                                  
CMLFTD   DS    XL2                                                              
CMLLTD   DS    XL2                                                              
CMLEST   DS    XL1                                                              
CMLINDX  DS    XL2                 CABLE STATION GROUP INDEX                    
         DS    CL2                 SPARE                                        
CMLNEXT  EQU   *                                                                
         SPACE                                                                  
* CABLE STATION SYSTEM NAME TABLE                                               
         SPACE                                                                  
CBLSTAD  DSECT                                                                  
CBLENT   DS    0XL5                                                             
CBLSTA   DS    XL3                                                              
CBLINDEX DS    CL2                                                              
CBLNEXT  EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028TEST49    11/06/02'                                      
         END                                                                    
