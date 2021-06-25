*          DATA SET SPTRA42    AT LEVEL 016 AS OF 09/03/09                      
*PHASE T21642C                                                                  
*INCLUDE XSORT                                                                  
*                                                                               
         TITLE 'T21642 COMMERCIAL RECAP LIST'                                   
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 - MARKET RECORDS                                             
*             AIO3 - NOT USED                                                   
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG                                                          
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE                 
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - POINT TO SHIP RECAP INFO TABLE                                    
*        R8 - POINTER TO SPOOLD                                                 
*        R9 - POINTER TO SYSD                                                   
*        RA - POINTER TO ATWA                                                   
*        RB - FIRST BASE                                                        
*        RC - POINTER TO GEND                                                   
*        RD - SAVE AREA POINTER                                                 
*        RE - GOTO1 REG                                                         
*        RF - GOTO1 REG                                                         
*                                                                               
*        PROGRAM LABELS MEANING:                                                
*        V PREFIX = VALIDATE                                                    
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                              
*        F PREFIX = FIND                                                        
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST                    
*                                                                               
***********************************************************************         
*                                                                     *         
*  LEV  4    JUN20/89 FIX ONLINE DISPLAY BUGS                         *         
*  LEV  5- 6 OCT07/91 ADD MARKET AND DATE FILTERS                     *         
*  LEV  7    MAR17/93 CABLE HEAD CHANGE                               *         
*  LEV  8 SMUR SEP11/97 ADD OPTION TO SORT BY CML TYPE                *         
*  LEV  9 BOTH FEB09/98 SIZE ERR MSG, BIGGER TABLE OFFLINE            *         
*  LEV 10 BGRI JAN20/00 STOP REPORT REQUEST                           *         
*  LEV 11 BGRI MAY07/01 CHANGE DUMMY                                  *         
*  LEV 12 BGRI JUL09/01 USE MORE SPACE IN DUMMY                       *         
*                                                                     *         
***********************************************************************         
*                                                                               
T21642   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21642**,RR=R3                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA             BASE SCREEN FOR SYSTEM + THIS PROG           
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR42RR                                                      
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000AFE'                                           
         GOTO1 CALLOV,DMCB                                                      
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,LISTRECS       ON-LINE RECAP                                
         BE    ONRP                                                             
         CLI   MODE,PRINTREP       OFF-LINE RECAP                               
         BE    ONRP                                                             
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       DS    0H                                                               
         TM    TRAMEDH+4,X'20'     WAS IT VALIDATED BEFORE                      
         BZ    VK10                 NO                                          
         TM    TRACLTH+4,X'20'     WAS IT VALIDATED BEFORE                      
         BZ    VK10                 NO                                          
         TM    TRACIDH+4,X'20'     WAS IT VALIDATED BEFORE                      
         BZ    VK10                 NO                                          
         TM    TRAFLTRH+4,X'20'    WAS IT VALIDATED BEFORE                      
         BO    EXIT                 NO                                          
*                                                                               
VK10     LA    R2,TRAMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         OI    TRAMEDH+4,X'20'     TURN ON VALIDATED BIT                        
*                                                                               
         LA    R2,TRACLTH          CLIENT                                       
         GOTO1 VALICLT                                                          
         OI    TRACLTH+4,X'20'     TURN ON VALIDATED BIT                        
*                                                                               
         LA    R2,TRACIDH          COMM ID                                      
         GOTO1 ANY                                                              
         CLI   5(R2),8                                                          
         BL    BADCMML                                                          
         BH    *+10                                                             
         MVC   SVCMML,WORK         SAVE ISCI                                    
         XC    SVCMMLP,SVCMMLP                                                  
                                                                                
* SET TO READ COMMERCIAL REC                                                    
                                                                                
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+5(8),SVCMML                                                  
         CLI   5(R2),12                                                         
         BH    BADCMML                                                          
* ALWAYS TRY TO PACK INPUT                                                      
         GOTO1 VTRPACK,DMCB,(C'P',WORK),SVCMMLP                                 
         BNE   VK14                IF INVALID, MUST BE ISCI                     
         CLI   5(R2),8             TEST 8 CHARACTERS INPUT                      
         BE    VK14                YES - USE ISCI FOR DIR READ                  
         MVC   KEY+5(8),SVCMMLP    ELSE USE PACKED ADID                         
*                                                                               
VK14     GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NFERR                                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         USING CMLDTAEL,R6                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SEQSV,CMLSEQ                                                     
         MVC   TITLESV,CMLTITLE                                                 
*                                                                               
         OI    TRACIDH+4,X'20'     TURN ON VALIDATED BIT                        
*                                                                               
         LA    R2,TRAFLTRH         FILTER                                       
         BAS   RE,VFTR                                                          
         OI    TRAFLTRH+4,X'20'    TURN ON VALIDATED BIT                        
*                                                                               
         XC    NEXTENT,NEXTENT     INIT DISPLACEMENT INTO THE TABLE             
*                                                                               
* NOW BUILD KEY                                                                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A25'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         B     EXIT                                                             
         EJECT                                                                  
* ONLINE RECAP LIST                                                             
*                                                                               
ONRP     DS    0H                                                               
         LA    R7,SHPRTBL                                                       
         LA    RF,L'SHPRTBL*SHPRNUM(R7)                                         
         SR    RF,R9                                                            
*                                                                               
         CLI   OFFLINE,C'Y'        MORE FOR OFFLINE                             
         BNE   ONRPA                                                            
         L     R7,VADUMMY                                                       
         LR    RF,R7                                                            
         A     RF,=F'100000'                                                    
*                                                                               
ONRPA    ST    RF,ATABLEX          END OF TABLE ADDRESS                         
         ST    R7,ATABLE                                                        
*                                                                               
         OC    NEXTENT,NEXTENT     FIRST TIME THROUGH                           
         BNZ   RP70                 NO                                          
*                                                                               
         USING SVRECD,R7                                                        
         L     RE,ATABLE                                                        
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         MVI   ELCODE,X'10'                                                     
*                                                                               
         OC    KEY(13),KEY                                                      
         BNZ   RP10                NO, FIND POSITION IN LIST                    
*                                                                               
*                                   SET TO READ SHIPPING REC                    
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A25'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
*                                                                               
RP10     GOTO1 HIGH                                                             
         B     RP25                                                             
*                                                                               
RP20     GOTO1 SEQ                                                              
*                                                                               
RP25     CLC   KEY(5),KEYSAVE     ID/AM/CLT                                     
         BNE   RP60                                                             
         CLC   KEY+10(3),SEQSV    CMML SEQ NUMBER                               
         BNE   RP20                                                             
*                                                                               
         BAS   RE,FTRA             GO FILTER ON MARKET                          
         BNE   RP20                                                             
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         LR    R5,R6                                                            
         USING SHPKEY,R5                                                        
*                                                                               
         GOTO1 GETREC                                                           
         BAS   RE,GETEL            FIND SHIPPING DATA ELEM                      
         BNE   RP20                                                             
         USING SHPDTAEL,R6                                                      
*                                                                               
RP30     CLI   SHPPIG,0            TEST PIGGY BACK ENTRY                        
         BNE   RP50                YES, GET NXT ELEM                            
*                                                                               
         BAS   RE,FTRB             GO FILTER ON DATES                           
         BNE   RP50                                                             
*                                                                               
         TM    SHPNOSHP,SHPISADI   TEST SHIPPING CMMLS ARE PACKED               
         BO    RP35                YES                                          
         CLC   SHPCMML,SVCMML      MATCH ISCI                                   
         BE    RP40                                                             
         CLC   SHPCMML2,SVCMML                                                  
         BE    RP40                                                             
         B     RP50                                                             
*                                                                               
RP35     CLC   SHPCMML,SVCMMLP     MATCH PACKED                                 
         BE    RP40                                                             
         CLC   SHPCMML2,SVCMMLP                                                 
         BE    RP40                                                             
         B     RP50                                                             
*                                                                               
*SAVE SHIP RECAP INFO IN TABLE                                                  
*                                                                               
RP40     MVC   SVMKTSTA,SHPKMKT    MARKET/STATION                               
         MVC   SVCMLS,SHPCMML      COMMERCIAL 1 AND 2                           
         MVC   SVSHPDT,SHPSHPDT    COMML SHIP DATE                              
         MVC   SVLTD,SHPLTD        LAST TELECAST DATE                           
         MVC   SVSHPFLG,SHPNOSHP   SHIP/ NO SHIP FLAG                           
*                                                                               
         BAS   RE,GETTYPE          GET CML TYPE                                 
*                                                                               
         LA    R7,SVNEXT                                                        
         L     R1,ATABLEX                                                       
         CLI   OFFLINE,C'Y'        MORE FOR OFFLINE                             
         BE    *+6                                                              
         AR    R1,R9                                                            
         CR    R7,R1               REACHED END OF TABLE                         
         BNL   TABSIZER             YES, QUIT                                   
*                                                                               
         XC    0(L'SVLREC+1,R7),0(R7)  CLEAR TABLE ENTRY                        
*                                                                               
RP50     BAS   RE,NEXTEL                                                        
         BE    RP30                                                             
         B     RP20                                                             
*                                                                               
RP60     L     R7,ATABLE           ANY ENTRIES IN THE TABLE                     
         LR    R3,R7                                                            
         SR    RE,RE                                                            
         OC    0(L'SVLREC,R7),0(R7)                                             
         BZ    RP100               ERROR NO RECS SELECTED                       
*                                                                               
RP62     BCTR  RE,0                                                             
         LA    R7,SVNEXT                                                        
         L     R1,ATABLEX                                                       
         CLI   OFFLINE,C'Y'        MORE FOR OFFLINE                             
         BE    *+6                                                              
         AR    R1,R9                                                            
         CR    R7,R1               REACHED END OF TABLE                         
         BNL   *+14                 YES                                         
         OC    0(L'SVLREC,R7),0(R7)                                             
         BNZ   RP62                                                             
*                                                                               
         LPR   R2,RE                                                            
         BZ    RP100               NOTHING TO DISPLAY                           
*                                                                               
         CLI   BYTE,C'Y'           SORT BY CML TYPE                             
         BNE   RP70                 NO, GO TO DISPLAY                           
*                                                                               
         GOTO1 =V(XSORT),DMCB,(R3),(R2),L'SVLREC,L'SVTYPE,             X        
               SVTYPE-SVRECD,RR=SPTR42RR                                        
*                                                                               
         EJECT                                                                  
RP70     DS    0H                                                               
         CLI   MODE,PRINTREP       OFF-LINE                                     
         BE    OFRP                 YES                                         
         EJECT                                                                  
* PROCESS FOR ON-LINE                                                           
*                                                                               
         L     R7,ATABLE           POINT TO THE TABLE                           
         USING SVRECD,R7                                                        
         OC    NEXTENT,NEXTENT     FIRST TIME THROUGH                           
         BZ    RP80                                                             
         ICM   R1,15,NEXTENT                                                    
         AR    R7,R1               NEXT ENTRY TO DISPLAY                        
         LA    R7,L'SVLREC(R7)                                                  
*                                                                               
RP80     MVC   LISTAR,SPACES                                                    
*                                                                               
         BAS   RE,FMTSTA                                                        
*                                                                               
         MVC   LSTA,STAPRNT                                                     
         OC    STANET,STANET                                                    
         BZ    *+10                                                             
         MVC   LSTA(8),STANET                                                   
*                                                                               
         MVC   LTYPE,SVTYPE                                                     
         MVC   LMKTNM,MKTNM                                                     
         MVC   LPTRCML,SPACES                                                   
* NEED TO FIGURE OUT WHICH CMML IS PARTNER!                                     
         MVC   LPTRCML(8),SVCML2                                                
         OC    SVCML2,SVCML2                                                    
         BZ    RP82                                                             
*                                                                               
         LA    RE,SVCMML                                                        
         TM    SVSHPFLG,SHPISADI   TEST CMML IS PACKED                          
         BZ    *+8                                                              
         LA    RE,SVCMMLP                                                       
*                                                                               
         CLC   0(8,RE),SVCML1      IF CMML1=KEYCMML, CML 2 IS PTNR              
         BE    *+10                                                             
         MVC   LPTRCML(8),SVCML1   ELSE CML1 IS PARTNER                         
*                                                                               
         TM    SVSHPFLG,SHPISADI   TEST CMML IS PACKED                          
         BZ    RP82                                                             
         GOTO1 VTRPACK,DMCB,(C'U',LPTRCML),LPTRCML                              
*                                                                               
RP82     TM    SVSHPFLG,X'80'       TEST NO SHIP                                
         BZ    RP84                                                             
         MVC   LNOSHIP,=C'NS'                                                   
*                                                                               
RP84     TM    SVSHPFLG,X'40'       TEST MANUAL SHIP                            
         BZ    RP86                                                             
         MVC   LMSHIP,=C'MS'                                                    
*                                                                               
RP86     OC    SVSHPDT,SVSHPDT                                                  
         BZ    RP88                                                             
         GOTO1 DATCON,DMCB,(3,SVSHPDT),(5,LSHPDTE)                              
*                                                                               
RP88     GOTO1 DATCON,DMCB,(3,SVLTD),(5,LLSTTCDT)                               
*                                                                               
         LR    RE,R7               POINT TO THE CURRENT ENTRY                   
         L     R1,ATABLE                                                        
         SR    RE,R1               GET DISPLACEMENT                             
         ST    RE,NEXTENT          AND SAVE IT                                  
*                                                                               
         MVI   NLISTS,16                                                        
         GOTO1 LISTMON                                                          
*                                                                               
         LA    R7,SVNEXT                                                        
         OC    0(L'SVLREC,R7),0(R7)                                             
         BNZ   *+12                                                             
         NI    TRAMEDH+4,X'FF'-X'20' TURN OFF VALIDATE BIT                      
         B     EXIT                                                             
*                                                                               
         L     R1,ATABLEX                                                       
         CLI   OFFLINE,C'Y'        MORE FOR OFFLINE                             
         BE    *+6                                                              
         AR    R1,R9                                                            
         CR    R7,R1               REACHED END OF TABLE ?                       
         BL    RP80                 NO, CONTINUE                                
         DC    H'0'                TABLE TOO SMALL                              
*                                                                               
RP100    LA    R2,TRAMEDH          MEDIA                                        
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=CL30'* NOTE * NO RECORDS SELECTED *'                
         GOTO1 ERREX2                                                           
         DROP  R6                                                               
         EJECT                                                                  
* READ STATION ADDRESS RECORD TO GET COMMERCIAL TYPE                            
*                                                                               
GETTYPE  NTR1                                                                   
*                                                                               
         MVC   SVKEY,KEY                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=XL2'0A28'   READ STATION RECORD                          
         MVC   KEY+2(1),BAGYMD                                                  
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',SHPKMKT),WORK,WORK+4                          
*                                                                               
         CLC   WORK+8(1),SPACES                                                 
         BNE   *+8                                                              
         MVI   WORK+8,C'T'                                                      
         MVC   KEY+3(5),WORK+4                                                  
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETTYPEX                                                         
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,GETEL                                                         
         BNE   GETTYPEX                                                         
*                                                                               
         USING STADTAEL,R6                                                      
*                                                                               
         MVC   SVTYPE,STACMLT      SAVE CML TYPE                                
*                                                                               
GETTYPEX DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                DUMMY READ FOR SEQ                           
         XIT1                                                                   
*                                                                               
         DROP  R5,R6                                                            
         EJECT                                                                  
*  SET TO READ MARKET REC TO GET MARKET NAME *                                  
*                                                                               
FMTSTA   NTR1                                                                   
*                                                                               
         USING SVRECD,R7                                                        
*                                                                               
         XC    STANET,STANET                                                    
         GOTO1 MSUNPK,DMCB,(X'80',SVMKTSTA),WORK,WORK+4                         
*                                                                               
         CLC   WORK+9(3),SPACES                                                 
         BE    *+14                                                             
         MVC   STANET,WORK+4                                                    
         MVI   STANET+4,C'/'                                                    
*                                                                               
         CLI   WORK+8,C' '                                                      
         BNE   *+8                                                              
         MVI   WORK+8,C'T'                                                      
*                                                                               
* FORMAT STATION FOR PRINTING *                                                 
*                                                                               
         MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),WORK+4                                                
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),WORK+8                                                   
         MVI   3(RE),C'V'                                                       
         CLI   QMED,C'T'                                                        
         BE    FMTSTA2                                                          
         MVI   3(RE),C'M'                                                       
         CLI   QMED,C'R'                                                        
         BE    FMTSTA2                                                          
         MVI   3(RE),C' '                                                       
*                                                                               
FMTSTA2  CLC   QMKT,WORK           TEST SAME MARKET AS PREV                     
         BE    FMTSTAX                                                          
         MVC   QMKT,WORK           SAVE MARKET                                  
*                                                                               
* READ MARKET RECORD *                                                          
*                                                                               
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO2                     
*                                                                               
         MVC   MKTNM,=CL24'**** UNKNOWN ****'                                   
         L     R3,AIO2                                                          
         CLC   KEY(8),0(R3)                                                     
         BNE   FMTSTAX                                                          
*                                                                               
         USING MKTRECD,R3                                                       
         MVC   MKTNM,MKTNAME                                                    
*                                                                               
FMTSTAX  XIT1                                                                   
         DROP  R3,R7                                                            
         EJECT                                                                  
* OFF LINE RECAP                                                                
*                                                                               
OFRP     DS    0H                                                               
         LA    R4,P                                                             
         USING P,R4                                                             
*                                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
*TEMP    L     R7,AIO3             POINT TO SHIP RECAP TABLE                    
         L     R7,ATABLE           POINT TO SHIP RECAP TABLE                    
         USING SVRECD,R7                                                        
*                                                                               
OP10     BAS   RE,FMTSTA                                                        
*                                                                               
         MVC   PSTA,STAPRNT                                                     
         OC    STANET,STANET                                                    
         BZ    *+10                                                             
         MVC   LSTA(8),STANET                                                   
*                                                                               
         MVC   PMKTNM,MKTNM                                                     
         MVC   PTYPE,SVTYPE                                                     
         MVC   PPRDCML(8),SVCML1                                                
         MVC   PPTRCML(8),SVCML2                                                
         TM    SVSHPFLG,SHPISADI   TEST CMMLS ARE PACKED                        
         BZ    OP12                                                             
         GOTO1 VTRPACK,DMCB,(C'U',SVCML1),PPRDCML                               
         GOTO1 (RF),(R1),(C'U',SVCML2),PPTRCML                                  
*                                                                               
OP12     TM    SVSHPFLG,X'80'       TEST NO SHIP                                
         BZ    OP20                                                             
         MVC   PNOSHIP,=C'NS'                                                   
*                                                                               
OP20     TM    SVSHPFLG,X'40'       TEST MANUAL SHIP                            
         BZ    OP26                                                             
         MVC   PMSHIP,=C'MS'                                                    
*                                                                               
OP26     OC    SVSHPDT,SVSHPDT                                                  
         BZ    OP28                                                             
         GOTO1 DATCON,DMCB,(3,SVSHPDT),(5,PSHPDTE)                              
*                                                                               
OP28     GOTO1 DATCON,DMCB,(3,SVLTD),(5,PLSTTCDT)                               
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R7,SVNEXT           BUMP TO NEXT ENTRY                           
         OC    0(L'SVLREC,R7),0(R7)                                             
         BZ    EXIT                                                             
         L     R1,ATABLEX                                                       
         CLI   OFFLINE,C'Y'        MORE FOR OFFLINE                             
         BE    *+6                                                              
         AR    R1,R9                                                            
         CR    R7,R1               REACHED END OF TABLE ?                       
         BL    OP10                 NO, CONTINUE                                
         DC    H'0'                TABLE TOO SMALL                              
*                                                                               
         DROP  R7                                                               
         EJECT                                                                  
* FILTER ROUTINES - MARKET *                                                    
*                                                                               
FTRA     OC    BMKT,BMKT                 FILTERING ON MARKET                    
         BZR   RE                         NO                                    
         CLC   BMKT,KEY+SHPKMKT-SHPRECD  THIS REQUESTED MKT                     
         BR    RE                        CK ON RETURN                           
*                                                                               
* FILTER ROUTINES - DATES *                                                     
*                                                                               
         USING SHPDTAEL,R6                                                      
FTRB     OC    FTRSTR,FTRSTR       FILTERING ON DATES                           
         BZR   RE                   NO                                          
         CLC   FTRSTR,SHPSHPDT     THIS BEFORE START DATE                       
         BHR   RE                   BYPASS                                      
         CLC   FTREND,SHPSHPDT     THIS AFTER END                               
         BLR   RE                   BYPASS                                      
         CR    RB,RB               SET TO OK                                    
         BR    RE CK ON RETURN                                                  
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE FILTER ROUTINES - MARKET *                                           
*                                                                               
         DS    0H                                                               
VFTR     NTR1                                                                   
         XC    BMKT,BMKT                                                        
         XC    QMKT,QMKT                                                        
         XC    FILTERS,FILTERS                                                  
         MVI   BYTE,0                                                           
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    EXIT                NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    VFTR90              YES                                          
         CLI   5(R2),4                                                          
         BNH   VFTR02                                                           
         LA    R1,4                                                             
         B     VFTR04                                                           
VFTR02   ZIC   R1,5(R2)                                                         
VFTR04   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL4'HELP'                                               
         BE    VFTR90                                                           
         GOTO1 SCANNER,DMCB,(20,TRAFLTRH),(5,BLOCK)                             
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERR             NO                                           
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
VFTR10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         CLI   0(R5),X'4C'         LESS THAN                                    
         BE    VFTR12              YES, SAVE IT                                 
         CLI   0(R5),X'6E'         GREATER THAN                                 
         BNE   VFTR14              NO, NETHER                                   
VFTR12   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
VFTR14   EX    R1,VFTRCLCA         MARKET                                       
         BE    VFTR20                                                           
         EX    R1,VFTRCLCB         MARKET                                       
         BNE   VFTR30                                                           
*                                                                               
* CONSTRUCT FLDHDR FOR VALIMKT *                                                
*                                                                               
VFTR20   XC    ELEM,ELEM                                                        
         ZIC   RE,1(R4)                                                         
         STC   RE,ELEM+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+8(0),22(R4) *EXECUTED*                                      
         LA    RE,9(RE)                                                         
         STC   RE,ELEM                                                          
         PACK  ELEM+4(1),3(1,R4)   INVERT VALIDITY BYTE HALVES                  
*                                                                               
         MVI   ERROPT,C'Y'         SET 'RETURN ON ERROR'                        
         LA    R2,ELEM             POINT TO FLDHDR                              
*                                                                               
         GOTO1 VALIMKT                                                          
*                                                                               
         MVI   ERROPT,C'N'         RESET                                        
         LA    R2,TRAFLTRH         RESTORE R2                                   
         CLI   ERROR,0                                                          
         BNE   TRAPERR                                                          
         B     VFTR80                                                           
*                                                                               
VFTR30   EX    R1,VFTRCLCC         DATE                                         
         BNE   VFTR40                                                           
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),WORK                                        
*                                                                               
         ICM   R6,15,DMCB             GET LENGTH OF FIELD                       
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,WORK),(3,FTRSTR)                                  
         MVC   FTREND,FTRSTR                                                    
         CLM   R6,1,1(R4)          ONLY 1 DATE ENTERED                          
         BE    VFTR80              YES                                          
*                                                                               
         LA    R5,1(R6,R5)         POINT TO END DATE                            
         GOTO1 DATVAL,(R1),(R5),WORK                                            
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,WORK),(3,FTREND)                                  
*                                                                               
         B     VFTR80                                                           
*                                                                               
VFTR40   EX    R1,VFTRCLCD         SORT BY CML TYPE                             
         BNE   VFTR90                                                           
         CLC   =C'TYPE',22(R4)                                                  
         BNE   VFTR90                                                           
         MVI   BYTE,C'Y'           SORT BY CML TYPE                             
*                                                                               
VFTR80   LA    R4,42(,R4)          POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
         B     EXIT                                                             
VFTR90   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTRHELP),FTRHELP                                       
         GOTO1 ERREX2                                                           
*                                                                               
VFTRCLCA CLC   12(0,R4),=CL4'MKT '                                              
VFTRCLCB CLC   12(0,R4),=CL7'MARKET '                                           
VFTRCLCC CLC   12(0,R4),=CL5'DATE '                                             
VFTRCLCD CLC   12(0,R4),=CL5'SORT '                                             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
HDHK     NTR1                                                                   
         MVC   H2+10(L'MEDNM),MEDNM                                             
         MVC   H3+10(L'QCLT),QCLT                                               
         MVC   H3+15(L'CLTNM),CLTNM                                             
         MVC   H4+10(12),TRACID                                                 
         MVC   H4+24(15),TITLESV                                                
         OC    FTRSTR,FTRSTR                                                    
         BZ    HDHK10                                                           
         MVC   H3+39(4),=C'FROM'                                                
         GOTO1 DATCON,DMCB,(3,FTRSTR),(5,H3+44)                                 
         MVC   H3+53(2),=C'TO'                                                  
         GOTO1 (RF),(R1),(3,FTREND),(5,H3+56)                                   
*                                                                               
HDHK10   OC    BMKT,BMKT                                                        
         BZ    EXIT                                                             
         MVC   H4+45(8),=C'MARKET ='                                            
         MVC   H4+54(4),QMKT                                                    
         B     EXIT                                                             
*                                                                               
TABSIZER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TABSIZMS),TABSIZMS                                     
         LA    R2,TRAMEDH                                                       
         GOTO1 ERREX2                                                           
*                                                                               
BADCMML  MVC   GERROR,=Y(NOT812)                                                
         GOTO1 VTRAERR                                                          
*                                                                               
NFERR    MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         EJECT                                                                  
         LTORG                                                                  
TABSIZMS DC    C'* ERROR * TOO BIG FOR ONLINE, RUN SOON/OVERNIGHT *'            
*                                                                               
FTRHELP  DC    C'* FILTERS = MARKET/DATE/SORT=TYPE *'                           
         EJECT                                                                  
HEADING  SSPEC H1,37,C'C O M M E R C I A L   R E C A P'                         
         SSPEC H2,37,C'-------------------------------'                         
         SSPEC H2,77,AGYNAME                                                    
         SSPEC H3,77,AGYADD                                                     
         SSPEC H4,77,RUN                                                        
         SSPEC H5,77,REQUESTOR                                                  
         SSPEC H5,101,PAGE                                                      
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H3,3,C'CLIENT'                                                   
         SSPEC H4,3,C'COM ID'                                                   
         SSPEC H8,3,C'STATION'                                                  
         SSPEC H9,3,C'-------'                                                  
         SSPEC H8,13,C'MARKET-NAME'                                             
         SSPEC H9,13,C'------------------------'                                
         SSPEC H8,40,C'TYPE'                                                    
         SSPEC H9,40,C'----'                                                    
         SSPEC H8,46,C'COMML-ID'                                                
         SSPEC H9,46,C'--------'                                                
         SSPEC H8,60,C'PARTNER'                                                 
         SSPEC H9,60,C'--------'                                                
         SSPEC H8,78,C'SHIP DATE'                                               
         SSPEC H9,78,C'---------'                                               
         SSPEC H8,91,C'LAST TLCST'                                              
         SSPEC H9,91,C'----------'                                              
         SSPEC H8,105,C'SHIP'                                                   
         SSPEC H9,105,C'----'                                                   
         DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE SPTRSHIP                                                       
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRSTA                                                        
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
* INCLUDED DSECTS                                                               
* INCLUDE DDSPOOLD                                                              
* INCLUDE DDSPLWORKD                                                            
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*                                                                               
* SAVE SHIP RECAP RECORD INFORMATION                                            
*                                                                               
SVRECD   DSECT                                                                  
SVLREC   DS    0XL32               RECORD LENGTH                                
SVMKTSTA DS    0XL5                MARKET/STATION                               
SVMKT    DS    XL2                                                              
SVSTA    DS    XL3                                                              
SVCMLS   DS   0CL16                                                             
SVCML1   DS    CL8                 CML ID                                       
SVCML2   DS    CL8                 CML 2                                        
SVSHPDT  DS    XL3                 COMMERCIAL SHIP DATE                         
SVLTD    DS    XL3                 LAST TELECAST DATE                           
SVSHPFLG DS    XL1                 SHIP/NO SHIP FLAG                            
*                                  X'01' = CMML IS PACKED ADID                  
SVTYPE   DS    CL4                 CML TYPE                                     
SVNEXT   EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAD2D                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
*                                                                               
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
MYWORK   DS    0C                                                               
SVCMML   DS    CL8                                                              
SVCMMLP  DS    XL8                                                              
SEQSV    DS    XL3                                                              
TITLESV  DS    CL15                                                             
FILTERS  DS   0CL7                                                              
FTRSTR   DS    XL3                                                              
FTREND   DS    XL3                                                              
HOLDSIGN DS    CL1                                                              
ATABLE   DS    A                                                                
ATABLEX  DS    A                                                                
SPTR42RR DS    A                                                                
VTRPACK  DS    A                                                                
NEXTENT  DS    F                   SAVE DISPLACEMENT INTO THE TABLE             
SHPRNUM  EQU   125                 125 ENTRIES IN TABLE                         
SHPRTBL  DS    (SHPRNUM)CL(L'SVLREC) SHIP RECAP TABLE 125 ENTRIES               
TBLX     EQU   *                                                                
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTA     DS    CL7                                                              
         DS    CL2                                                              
LMKTNM   DS    CL20                                                             
         DS    CL1                                                              
LTYPE    DS    CL4                                                              
         DS    CL1                                                              
LPTRCML  DS    CL12                                                             
         DS    CL2                                                              
LSHPDTE  DS    CL8                                                              
         DS    CL2                                                              
LLSTTCDT DS    CL8                                                              
         DS    CL4                                                              
LMSHIP   DS   0CL2                                                              
LNOSHIP  DS    CL2                                                              
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL3                                                              
PMKTNM   DS    CL24                                                             
         DS    CL3                                                              
PTYPE    DS    CL4                                                              
         DS    CL2                                                              
PPRDCML  DS    CL12                                                             
         DS    CL2                                                              
PPTRCML  DS    CL12                                                             
         DS    CL6                                                              
PSHPDTE  DS    CL8                                                              
         DS    CL5                                                              
PLSTTCDT DS    CL8                                                              
         DS    CL7                                                              
PMSHIP   DS   0CL2                                                              
PNOSHIP  DS    CL2                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SPTRA42   09/03/09'                                      
         END                                                                    
