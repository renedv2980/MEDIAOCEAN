*          DATA SET SPTRA4B    AT LEVEL 056 AS OF 06/08/01                      
*PHASE T2164BA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: TRAFFIC STATION LABEL GENERATION                            *         
*                                                                     *         
*  COMMENTS:  THIS PROGRAM WILL GENERATE STATION LABELS FOR AN        *         
*             AGENCY BASED UPON THE INSTRUCTION RECAP RECORDS,        *         
*             SHIPPING RECAP RECORDS, OR A SELECTIONS FROM A          *         
*             USER-CREATED LIST.                                      *         
*                                                                     *         
*  CALLED FROM: TRAFFIC CONTROLLER (T21600), WHICH CALLS              *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  INPUTS: SEE SCREEN SPTRADB (T216DB)                                *         
*          SPTRAWORKD (SYSD)                                          *         
*          DDSPLWORKD (GEND)                                          *         
*                                                                     *         
*  OUTPUTS: STATION LABELS                                            *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER      *         
*                    (DDGENCON-T00A30)                                *         
*             AIO2 -                                                  *         
*             AIO3 -                                                  *         
*                                                                     *         
*        PROGRAM LABELS MEANING:                                      *         
*        V PREFIX = VALIDATE                                          *         
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                    *         
*        F PREFIX = FIND                                              *         
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST          *         
*                                                                     *         
***********************                                               *         
*  LOGIC:                                                             *         
*     1.  OBTAIN STATION NAME (FROM RECORD OR SCREEN)                 *         
*     2.  GET STATION ADDRESS RECORD FOR STATION                      *         
*     3.  PUT ADDRESS IN TEMPORARY PRINT LINES                        *         
*     4.  WHEN PRINT LINES ARE FILLED ACROSS, OUTPUT THEM             *         
*                                                                     *         
*  LEV 42    JAN08/87 ADD OFFICE PROFILE                              *         
*  LEV 43    JAN16/87 ADD OPTION TWX - PRINT TWX, NOT RETURN ADDR     *         
*  LEV 44    MAR31/87 DON'T PRINT F & M FROM FROM IF BLK RET ADDR     *         
*  LEV 45    APR23/87 FIX BUG TO READ PROFILE IF ALL STA              *         
*  LEV 46-48 JUN/90  ALLOW FOR SHORTER LABELS                         *         
*  LEV 49    JUN/90  ADD OPTION TO SUPPRESS CLIENT NAME                         
*  LEV 50    MAY13/92 ADD OPTION FOR PRODUCT POL TO GET 1 LABEL PER   *         
*                     STATION FOR ALL PRODUCTS, NOT 1 PER PRODUCT     *         
*  LEV 51    JUL01/92 BYPASS GETRECS FOR STATIONS ALREADY LISTED      *         
*  LEV 52    JUL22/92 ADD EST FILTER IF RUNNING FROM INSTR RECAP      *         
*  LEV 53    DEC02/92 CHANGE FOR MSUNPK                               *         
*  LEV 54    MAR22/93 ADD CABLE HEAD                                  *         
*  LEV 55    APR15/94 CHANGE OPERATIONS DESK TO TRAFFIC DESK          *         
*  LEV 56    JUN08/01 USE TRAFFIC OFFICE                              *         
*                                                                     *         
***********************************************************************         
         TITLE 'T2164B -- STATION LABEL GENERATION'                             
T2164B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T2164B*,RR=R3                                                 
         LA    R7,2048(,RB)                                                     
         LA    R7,2048(,R7)                                                     
         USING T2164B,RB,R7                                                     
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR4BRR                                                      
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    PR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    INVAL                                                            
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    INVAL                                                            
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    INVAL                                                            
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    INVAL                                                            
         CLI   MODE,RECADD         ADD RECORD                                   
         BE    INVAL                                                            
         CLI   MODE,RECPUT         PUT RECORD                                   
         BE    INVAL                                                            
         CLI   MODE,RECDEL         DEL RECORD                                   
         BE    INVAL                                                            
EXIT     XIT1                                                                   
         SPACE 5                                                                
INVAL    MVI   ERROR,INVACT        ERROR ROUTINE FOR INVALID ACTIONS            
         LA    R2,CONACTH                                                       
         GOTO1 ERREX                                                            
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       LA    R2,CONWHENH         PRINT (WHEN) FIELD                           
         XC    SCANBLK,SCANBLK     CLEAR SCANNER BLOCK                          
         GOTO1 SCANNER,DMCB,CONWHENH,(2,SCANBLK)                                
         ZIC   R6,DMCB+4           NUMBER OF BLOCKS                             
         CH    R6,=H'0'            TEST ANY BLOCKS FOUND                        
         BE    VK02                NEXT INPUT FIELD                             
         LA    R5,SCANBLK          ADDRESS OF FIRST BLOCK                       
         CLC   12(3,R5),=C'DDS'    TEST 'WHEN' VALUE AS DDS                     
         BE    NOTDDS                                                           
         LA    R5,32(R5)           NEXT SCANNER BLOCK ENTRY                     
         CLC   12(3,R5),=C'DDS'    TEST 'WHERE' VALUE AS DDS                    
         BE    NOTDDS                                                           
*                                                                               
VK02     LA    R2,TRAMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
         MVI   TWXSW,C'N'           YES                                         
         MVI   NOAGYSW,C'N'         YES                                         
         MVI   ALLSTASW,C'N'       DEFAULT -- DON'T DO ALL STATIONS             
         CLI   TRASTAH+5,3                                                      
         BNE   VK04                                                             
         CLC   TRASTA(3),=C'ALL'   TEST PRINT ALL STATIONS                      
         BNE   VK04                                                             
         MVI   ALLSTASW,C'Y'                                                    
*                                                                               
VK04     LA    R2,TRACLTH          CLIENT                                       
         XC    BCLT,BCLT                                                        
         XC    QCLT,QCLT                                                        
         XC    CLTNM,CLTNM                                                      
         CLI   5(R2),0             TEST CLIENT WAS ENTERED                      
         BE    VK06                NO                                           
         GOTO1 VALICLT                                                          
         B     VK07                                                             
         SPACE                                                                  
VK06     CLI   ALLSTASW,C'Y'       TEST STATION = ALL                           
         BNE   MISSERR                                                          
*                                                                               
VK07     MVI   LISTFLAG,C'N'       NO 'LIST=' OPTION FOUND YET                  
*                                                                               
         CLI   ALLSTASW,C'Y'       TEST STATION = ALL                           
         BNE   VK10                                                             
*                                                                               
         CLI   TRACLTH+5,0         MOST OTHER FIELDS MUST BE EMPTY              
         BE    *+12                                                             
         LA    R2,TRACLTH                                                       
         B     BADALLST                                                         
*                                                                               
         CLI   TRAPRDH+5,0                                                      
         BE    *+12                                                             
         LA    R2,TRAPRDH                                                       
         B     BADALLST                                                         
*                                                                               
         CLI   TRAMKTH+5,0                                                      
         BE    *+12                                                             
         LA    R2,TRAMKTH                                                       
         B     BADALLST                                                         
*                                                                               
         CLI   TRADATEH+5,0                                                     
         BE    *+12                                                             
         LA    R2,TRADATEH                                                      
         B     BADALLST                                                         
*                                                                               
         LA    R2,TRAFTRH                                                       
         MVI   TWXSW,C'N'                                                       
         CLI   5(R2),0                                                          
         BE    VK09                                                             
         CLC   =C'TWX',8(R2)       TEST PRINT TWX, NOT RETURN ADDRESS           
         BNE   VK08                                                             
         MVI   TWXSW,C'Y'           YES                                         
         B     VK09                                                             
*                                                                               
VK08     CLC   =C'NOAGY',8(R2)     TEST ALLOW NO AGY ADDR ON BLK LABELS         
         BNE   BADALLST                                                         
         MVI   NOAGYSW,C'Y'         YES                                         
VK09     BAS   RE,INITPROF         GET TL PROFILE                               
         B     VK90                                                             
*                                                                               
VK10     BAS   RE,INITPROF         GET TL PROFILE                               
         XC    SCANBLK,SCANBLK     CLEAR SCANNER BLOCK                          
         GOTO1 SCANNER,DMCB,TRAFTRH,(5,SCANBLK)                                 
         ZIC   R6,DMCB+4           NUMBER OF BLOCKS                             
         CH    R6,=H'0'            TEST ANY BLOCKS FOUND                        
         BE    VK40                NEXT INPUT FIELD                             
         LA    R5,SCANBLK          ADDRESS OF FIRST BLOCK                       
*                                                                               
VK20     CLI   0(R5),4             LENGTH OF 'LIST'                             
         BNE   VK30                                                             
         CLC   12(4,R5),=C'LIST'                                                
         BNE   VK30                                                             
         MVI   LISTFLAG,C'Y'                                                    
         B     VK40                                                             
*                                                                               
VK30     LA    R5,32(R5)           POINT TO NEXT BLOCK                          
         BCT   R6,VK20                                                          
*                                                                               
VK40     CLI   LISTFLAG,C'Y'       TEST 'LIST=' OPTION FOUND                    
         BE    VK50                YES -- REQUIRE PRODUCT                       
         CLI   TLPROF1,C'I'        TEST INSTRUCTION RECAPS USED                 
         BE    VK50                YES -- REQUIRE PRODUCT                       
*                                                                               
         LA    R2,TRAPRDH          NO PRODUCT FOR SHIPPING RECAPS               
         CLI   5(R2),0             TEST PRODUCT WAS ENTERED                     
         BNE   BADPRDER            IT WAS -- ERROR                              
         B     VK60                IT WAS NOT -- VALIDATE NEXT FIELD            
*                                                                               
         LA    R2,TRAESTH          NO ESTIMATE FOR SHIPPING RECAPS              
         CLI   5(R2),0             TEST ESTIMATE WAS ENTERED                    
         BNE   BADESTER            IT WAS -- ERROR                              
         B     VK60                IT WAS NOT -- VALIDATE NEXT FIELD            
*                                                                               
VK50     LA    R2,TRAPRDH          PRODUCT FOR INSTRUCTION RECAPS               
         MVI   BPRD,0                                                           
         CLI   5(R2),0             TEST PRODUCT WAS ENTERED                     
         BE    MISSERR                                                          
         GOTO1 VALIPRD                                                          
         MVC   QPRD,WORK                                                        
         MVC   BPRD,WORK+3                                                      
         SPACE                                                                  
         CLI   BPRD,X'FF'          THIS A POL (ALL PRODUCTS) REQUEST            
         BNE   VK54                 NO                                          
         SPACE                                                                  
         L     RF,ASYSD            CLEAR STATION TABLE                          
         A     RF,LSYSD                                                         
         LA    RE,STATABLE         START OF STATION TABLE                       
         SR    RF,RE                                                            
         XCEFL                                                                  
         SPACE                                                                  
VK54     LA    R2,TRAESTH          ESTIMATE FOR INSTRUCTION RECAPS              
         MVI   BEST,0                                                           
         CLI   5(R2),0             TEST PRODUCT WAS ENTERED                     
         BE    VK60                 NO                                          
         XC    WORK,WORK               READ TRAFFIC LABEL PROFILE               
         XC    SVT1PROF,SVT1PROF                                                
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVT1PROF,DATAMGR                               
         SPACE                                                                  
         CLI   SVT1PROF+10,C'E'       COPY CODE = ESTIMATE                      
         BNE   ESTNOTAL                                                         
         SPACE                                                                  
         GOTO1 VALINUM                                                          
         SPACE                                                                  
         MVC   BEST,ACTUAL                                                      
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ESTHDRD,R4                                                       
         MVC   EKEYAM(3),BAGYMD                                                 
         MVC   EKEYPRD,QPRD                                                     
         MVC   EKEYEST,BEST                                                     
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ESTCDER                                                          
         SPACE                                                                  
VK60     LA    R2,TRAMKTH          MARKET                                       
         XC    BMKT,BMKT                                                        
         CLI   5(R2),0                                                          
         BE    VK70                                                             
         GOTO1 VALIMKT                                                          
VK70     MVC   MYBMKT,BMKT         SAVE MARKET BEFORE IT'S OVERWRITTEN          
*                                                                               
         LA    R2,TRASTAH          STATION                                      
         XC    BSTA,BSTA                                                        
         CLI   5(R2),0                                                          
         BE    VK80                                                             
         GOTO1 VALISTA                                                          
         OC    MYBMKT,MYBMKT       TEST ANY MARKET ENTERED                      
         BZ    VK80                                                             
         CLC   MYBMKT,BMKT         SEE IF ENTERED MARKET IS VALID               
         BE    VK80                                                             
         MVI   ERROR,INVMKT        ERROR ROUTINE FOR INVALID MARKET             
         LA    R2,TRAMKTH                                                       
         GOTO1 ERREX                                                            
*                                                                               
VK80     CLI   LISTFLAG,C'Y'       TEST 'LIST=' OPTION FOUND                    
         BE    *+12                                                             
         LA    R2,TRADATEH         DATE                                         
         BAS   RE,VDATE                                                         
*                                                                               
         LA    R2,TRAFTRH          FILTERS                                      
         BAS   RE,VFTR                                                          
*                                                                               
VK90     MVC   CONOUT(3),=C'LBL'   SUPPRESS PAGE EJECTS                         
         OI    CONOUTH+6,X'80'     XMIT                                         
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* GET TRAFFIC LABEL PROFILE                                                     
*                                                                               
INITPROF NTR1                                                                   
*                                                                               
         XC    WORK,WORK               READ TRAFFIC LABEL PROFILE               
         XC    SVPROF,SVPROF                                                    
         MVC   WORK(4),=C'S0TL'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         OC    SVPROF,SVPROF           TEST PROFILE FOUND                       
         BZ    NOPROF                                                           
*                                                                               
         CLI   TLPROF5,22              OLD MINIMUM                              
         BNL   IP10                                                             
         CLI   TLPROF7,C'Y'           SEE IF SUPPRESSING AGY NAME               
         BE    IP10                                                             
         CLI   TLPROF5,14            LINES/LABEL MUST BE AT LEAST 14            
         BL    LINCTERR              IF NOT SUPPRESSING AGY NAME                
*                                                                               
*                                    IF NOT SUPPRESSING AGENCY NAME             
*                                    NEW MINIMUM IS 9                           
*                                                                               
IP10     CLI   TLPROF2,C'B'            TEST BLANK LABELS                        
         BNE   IPX                                                              
         CLI   NOAGYSW,C'Y'            OPT NOAGY ALLOWS NO AGY ON BLKS          
         BE    IPX                                                              
         CLI   TLPROF7,C'Y'            MUST HAVE AGENCY NAME ON LABEL           
         BE    SUPPERR                                                          
*                                                                               
IPX      B     EXIT                                                             
         EJECT                                                                  
* INITIALIZE AND FIND ROUTINE TO GET STATION NAMES                              
*                                                                               
PR       XC    CURDISP,CURDISP     BEGIN AT LEFT MARGIN                         
         MVC   CURLABEL,=F'1'      FIRST COLUMN OF LABELS                       
*                                                                               
         LA    R0,11               BLANK OUT PRINT LINES                        
         LA    R1,PX                                                            
         XC    0(132,R1),0(R1)                                                  
         LA    R1,132(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         CLI   ALLSTASW,C'Y'       TEST ALL STATIONS                            
         BE    ALLSTA                                                           
         CLI   LISTFLAG,C'Y'       TEST AGENCY PREPARED LIST IS USED            
         BE    AL                                                               
         CLI   TLPROF1,C'I'        TEST WHICH RECORDS TO USE                    
         BE    IR                  USE INSTRUCTION RECAPS                       
         B     SR                  USE SHIPPING RECAPS                          
         EJECT                                                                  
* GENERATE LABELS FOR EVERY STATION                                             
*                                                                               
ALLSTA   XC    KEY,KEY             FIND FIRST STATION ADDRESS RECORD            
         MVC   KEY(2),=X'0A28'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   ALLSTKEY,KEY                                                     
         GOTO1 HIGH                FIND KEY                                     
         CLC   ALLSTKEY,KEY                                                     
         BNE   NORECERR            THERE ARE NO LABELS                          
*                                                                               
ALLSTA10 MVC   QSTA,KEY+3                                                       
         BAS   RE,MAKELBL          CREATE A LABEL FOR THIS STATION              
*                                                                               
         GOTO1 SEQ                                                              
         CLC   ALLSTKEY,KEY                                                     
         BE    ALLSTA10                                                         
*                                                                               
ALLSTAX  CLI   ROWDONE,C'Y'        TEST IF AN UNPRINTED ROW REMAINS             
         BE    *+8                                                              
         BAS   RE,PRTLABEL                                                      
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* USE AGENCY-CREATED LIST OF STATIONS                                           
*                                                                               
AL       MVC   KEY(13),STLSTKEY    SAVED KEY OF THE STATION LIST                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     THE KEY MUST BE THERE                        
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         USING LSTDTAEL,R6                                                      
*                                                                               
         MVI   ELCODE,X'10'        STATION CODE ELEMENT                         
         BAS   RE,GETEL            FIRST ELEMENT                                
         BE    *+6                                                              
         DC    H'00'               MUST BE THERE                                
*                                                                               
AL10     XC    TMKT,TMKT                                                        
         MVC   TSTA,LSTSTA                                                      
         XC    STANET,STANET                                                    
         GOTO1 MSUNPK,DMCB,(X'80',TMKTSTA),QMKT,DUB                             
         MVC   QSTA,DUB                                                         
         SPACE                                                                  
         CLC   DUB+5(3),SPACES     CABLE HEAD                                   
         BE    *+14                                                             
         MVC   STANET,DUB                                                       
         MVI   STANET+4,C'/'                                                    
         SPACE                                                                  
         BAS   RE,MAKELBL          CREATE A LABEL FOR THIS STATION              
         BAS   RE,NEXTEL           NEXT STATION                                 
         BE    AL10                                                             
*                                                                               
         CLI   ROWDONE,C'Y'        TEST IF AN UNPRINTED ROW REMAINS             
         BE    *+8                                                              
         BAS   RE,PRTLABEL                                                      
*                                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
* USE INSTRUCTION RECAP RECORDS                                                 
*                                                                               
         USING INSKEY,R4                                                        
*                                                                               
IR       LA    R4,KEY                                                           
         XC    KEY,KEY             CONSTRUCT INSTRUCTION RECAP KEY              
         MVC   INSKID,=X'0A24'                                                  
         MVC   INSKAM,BAGYMD                                                    
         MVC   INSKCLT,BCLT                                                     
         SPACE                                                                  
         CLI   BPRD,X'FF'          THIS A POL REQUEST                           
         BE    *+10                 YES                                         
         MVC   INSKPRD,BPRD                                                     
         SPACE                                                                  
         MVC   INSKMKT,BMKT                                                     
         MVC   INSKSTA,BSTA                                                     
*                                                                               
         MVI   FOUNDSW,0                                                        
         GOTO1 HIGH                GET FIRST RECORD                             
         B     IR24                                                             
*                                                                               
IR20     GOTO1 HIGH                RESTORE READ POINTER                         
*                                                                               
IR22     GOTO1 SEQ                 GET NEXT RECORD                              
*                                                                               
IR24     CLC   INSKAM,BAGYMD       TEST SAME AGY/MED                            
         BNE   IR98                                                             
         CLC   INSKCLT,BCLT        TEST SAME CLIENT                             
         BNE   IR98                                                             
         SPACE                                                                  
         CLI   BPRD,X'FF'          THIS A POL REQUEST                           
         BE    IR26                 YES                                         
         SPACE                                                                  
         CLC   INSKPRD,BPRD        TEST SAME PRODUCT                            
         BNE   IR22                                                             
*                                                                               
IR26     OC    BMKT,BMKT           TEST MARKET ENTERED                          
         BZ    IR28                NO                                           
         CLC   BMKT,INSKMKT        TEST SAME MARKET                             
         BNE   IR22                NO                                           
*                                                                               
IR28     OC    BSTA,BSTA           TEST STATION ENTERED                         
         BZ    IR29                 NO                                          
         CLC   BSTA,INSKSTA        TEST SAME STATION                            
         BNE   IR22                 NO                                          
         SPACE                                                                  
IR29     CLI   BEST,0              TEST ESTIMATE ENTERED                        
         BE    IR30                 NO                                          
         CLC   BEST,INSKCOPY       TEST SAME ESTIMATE                           
         BNE   IR22                 NO                                          
         SPACE                                                                  
IR30     L     R0,ASYSD                                                         
         A     R0,LSYSD                                                         
         LA    R1,STATABLE         START OF STATION TABLE                       
         SPACE                                                                  
IR31     OC    0(3,R1),0(R1)       AT END OF TABLE                              
         BZ    IR32                                                             
         CLC   INSKSTA,0(R1)       ALREADY PRINTED                              
         BE    IR22                 YES, GET NEXT                               
         LA    R1,3(,R1)                                                        
         C     R1,R0                                                            
         BL    IR31                                                             
         DC    H'0'                                                             
IR32     L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         USING INSDTAEL,R6                                                      
*                                                                               
         MVI   ELCODE,X'10'        GET INSTRUCTION DATA ELEMENT                 
         BAS   RE,GETEL            TEST FIRST ELEMENT IS X'10'                  
         BNE   IR22                NO X'10' ELEMENTS                            
         B     IR36                                                             
*                                                                               
IR34     BAS   RE,NEXTEL                                                        
         BNE   IR22                                                             
*                                                                               
IR36     OC    EDATE,EDATE         TEST END DATE EXISTS                         
         BNZ   IR40                                                             
         CLC   INSDATE,SDATE       MUST EQUAL START DATE                        
         BNE   IR34                                                             
         B     IR50                                                             
*                                                                               
IR40     CLC   INSDATE,SDATE       COMPARE WITH START DATE                      
         BL    IR34                IT'S BEFORE START DATE                       
         CLC   INSDATE,EDATE       COMPARE WITH END DATE                        
         BH    IR34                IT'S AFTER END DATE                          
*                                                                               
IR50     MVI   FOUNDSW,X'FF'                                                    
         SPACE                                                                  
         CLI   BPRD,X'FF'          THIS A POL REQUEST                           
         BNE   IR60                 NO                                          
         L     R0,ASYSD                                                         
         A     R0,LSYSD                                                         
         LA    R1,STATABLE         START OF STATION TABLE                       
         SPACE                                                                  
IR54     OC    0(3,R1),0(R1)       AT END OF TABLE                              
         BZ    IR56                                                             
         CLC   INSKSTA,0(R1)       ALREADY PRINTED                              
         BE    IR22                 YES, GET NEXT                               
         LA    R1,3(,R1)                                                        
         C     R1,R0                                                            
         BL    IR54                                                             
         DC    H'0'                                                             
IR56     MVC   0(3,R1),INSKSTA                                                  
         SPACE                                                                  
IR60     GOTO1 MSUNPK,DMCB,(X'80',INSKMKT),QMKT,DUB                             
         MVC   QSTA,DUB                                                         
         SPACE                                                                  
         XC    STANET,STANET                                                    
         CLC   DUB+5(3),SPACES     CABLE HEAD                                   
         BE    *+14                                                             
         MVC   STANET,DUB                                                       
         MVI   STANET+4,C'/'                                                    
         SPACE                                                                  
         BAS   RE,MAKELBL          CREATE A LABEL FOR THIS STATION              
         SPACE                                                                  
         B     IR20                                                             
*                                                                               
IR98     CLI   FOUNDSW,0           TEST WERE ANY LABELS GENERATED               
         BE    NORECERR                                                         
         CLI   ROWDONE,C'Y'        TEST IF AN UNPRINTED ROW REMAINS             
         BE    IRX                                                              
         BAS   RE,PRTLABEL                                                      
*                                                                               
IRX      B     EXIT                                                             
*                                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
* USE SHIPPING RECAP RECORDS                                                    
*                                                                               
         USING SHPKEY,R4                                                        
*                                                                               
SR       LA    R4,KEY                                                           
         XC    KEY,KEY             CONSTRUCT SHIPPING RECAP KEY                 
         MVC   SHPKID,=X'0A25'                                                  
         MVC   SHPKAM,BAGYMD                                                    
         MVC   SHPKCLT,BCLT                                                     
         MVC   SHPKMKT,BMKT                                                     
         MVC   SHPKSTA,BSTA                                                     
*                                                                               
         MVI   FOUNDSW,0                                                        
         GOTO1 HIGH                GET FIRST RECORD                             
         B     SR24                                                             
*                                                                               
SR20     GOTO1 HIGH                RESTORE READ POINTER                         
*                                                                               
SR22     GOTO1 SEQ                 GET NEXT RECORD                              
*                                                                               
SR24     CLC   SHPKAM,BAGYMD       TEST SAME AGY/MED                            
         BNE   SR98                                                             
         CLC   SHPKCLT,BCLT        TEST SAME CLIENT                             
         BNE   SR98                                                             
*                                                                               
         OC    BMKT,BMKT           TEST MARKET ENTERED                          
         BZ    SR28                NO                                           
         CLC   BMKT,SHPKMKT        TEST SAME MARKET                             
         BNE   SR22                NO                                           
*                                                                               
SR28     OC    BSTA,BSTA           TEST STATION ENTERED                         
         BZ    SR32                NO                                           
         CLC   BSTA,SHPKSTA        TEST SAME STATION                            
         BNE   SR22                NO                                           
*                                                                               
SR32     L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         USING SHPDTAEL,R6                                                      
*                                                                               
         MVI   ELCODE,X'10'        GET SHIPPING DATA ELEMENT                    
         BAS   RE,GETEL            TEST FIRST ELEMENT IS X'10'                  
         BNE   SR22                NO X'10' ELEMENTS                            
         B     SR36                                                             
*                                                                               
SR34     BAS   RE,NEXTEL                                                        
         BNE   SR22                                                             
*                                                                               
SR36     CLI   SHPPIG,0            TEST PIGGYBACK ENTRY                         
         BNE   SR22                                                             
*                                                                               
         OC    SHPSHPDT,SHPSHPDT   TEST COMMERCIAL HAS BEEN SHIPPED             
         BZ    SR50                IT HAS NOT                                   
*                                                                               
         OC    EDATE,EDATE         TEST END DATE EXISTS                         
         BNZ   SR40                                                             
         CLC   SHPSHPDT,SDATE      MUST EQUAL START DATE                        
         BNE   SR34                                                             
         B     SR50                                                             
*                                                                               
SR40     CLC   SHPSHPDT,SDATE      COMPARE WITH START DATE                      
         BL    SR34                IT'S BEFORE START DATE                       
         CLC   SHPSHPDT,EDATE      COMPARE WITH END DATE                        
         BH    SR34                IT'S AFTER END DATE                          
*                                                                               
SR50     ZIC   R5,CMMLLEN          LENGTH OF COMMERCIAL FILTER                  
         LTR   R5,R5               TEST COMMERICAL FILTER ENTERED               
         BZ    SR70                IT WAS NOT                                   
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   SHPCMML(0),CMML     FILTER ON COMMERCIAL                         
         BE    SR70                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   SHPCMML2(0),CMML    FILTER ON COMMERCIAL                         
         BNE   SR34                DOESN'T MATCH -- NEXT ELEMENT                
*                                                                               
SR70     MVI   FOUNDSW,X'FF'                                                    
         GOTO1 MSUNPK,DMCB,(X'80',SHPKMKT),QMKT,DUB                             
         MVC   QSTA,DUB                                                         
         SPACE                                                                  
         XC    STANET,STANET                                                    
         CLC   DUB+5(3),SPACES     CABLE HEAD                                   
         BE    *+14                                                             
         MVC   STANET,DUB                                                       
         MVI   STANET+4,C'/'                                                    
         SPACE                                                                  
         BAS   RE,MAKELBL          CREATE A LABEL FOR THIS STATION              
         B     SR20                                                             
*                                                                               
SR98     CLI   FOUNDSW,0           TEST WERE ANY LABELS GENERATED               
         BE    NORECERR                                                         
         CLI   ROWDONE,C'Y'        TEST IF AN UNPRINTED ROW REMAINS             
         BE    SRX                                                              
         BAS   RE,PRTLABEL                                                      
*                                                                               
SRX      B     EXIT                                                             
*                                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
* CREATE LABELS                                                                 
*                                                                               
MAKELBL  NTR1                                                                   
*                                                                               
         MVI   ROWDONE,C'N'                                                     
         L     R5,CURDISP          DISPLACEMENT OF LABEL IN ROW                 
*                                                                               
         MVC   SVKEY(13),KEY       SAVE KEY                                     
*                                                                               
         CLI   QSTA+4,C' '                                                      
         BNE   *+8                                                              
         MVI   QSTA+4,C'T'                                                      
*                                                                               
         MVC   STAPRNT,SPACES      FILL IN 7-CHARACTER STATION CODE             
         MVC   STAPRNT(4),QSTA                                                  
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),QSTA+4      MOVE SUB-MEDIA                               
         MVI   3(RE),C'V'          ASSUME TV                                    
         CLI   QMED,C'T'                                                        
         BE    FL5                                                              
         MVI   3(RE),C'M'          ASSUME RADIO                                 
         CLI   QMED,C'R'                                                        
         BE    FL5                                                              
         MVI   3(RE),C' '                                                       
*                                                                               
FL5      XC    KEY,KEY             BUILD STATION ADDRESS KEY                    
         MVC   KEY(2),=X'0A28'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(5),QSTA                                                    
         GOTO1 HIGH                FIND KEY                                     
         CLC   KEY(8),KEYSAVE      TEST RECORD THERE                            
         BE    FL10                                                             
         MVI   STAFOUND,C'N'                                                    
         B     FL20                                                             
*                                                                               
FL10     L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC              GET STATION ADDRESS RECORD                   
*                                                                               
         MVI   ELCODE,X'10'        GET INSTRUCTION DATA ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    FL15                TEST ELEMENT THERE                           
         MVI   STAFOUND,C'N'                                                    
         B     FL20                                                             
*                                                                               
FL15     MVI   STAFOUND,C'Y'                                                    
FL20     BAS   RE,FILLPRNT         FILL PRINT LINES FOR A LABEL                 
*                                                                               
         L     R3,CURLABEL                                                      
         LA    R3,1(R3)                                                         
         CLM   R3,1,TLPROF3        TEST MORE LABELS ACROSS TO PRINT             
         BNH   FL30                                                             
*                                                                               
         MVC   CURLABEL,=F'1'                                                   
         XC    CURDISP,CURDISP                                                  
         MVI   ROWDONE,C'Y'                                                     
         BAS   RE,PRTLABEL         PRINT ROW OF LABELS                          
         B     FLX                                                              
*                                                                               
FL30     ST    R3,CURLABEL                                                      
         ZIC   R3,TLPROF4          WIDTH OF LABEL                               
         AR    R5,R3                                                            
         ST    R5,CURDISP          STARTING COLUMN OF NEXT LABEL                
*                                                                               
FLX      XC    KEY,KEY             RESTORE KEY                                  
         MVC   KEY(13),SVKEY                                                    
         B     EXIT                                                             
         EJECT                                                                  
         USING STADTAEL,R6                                                      
*                                                                               
* FILL PRINT LINES                                                              
*                                                                               
FILLPRNT NTR1                                                                   
*                                                                               
         LA    R4,PX                                                            
         LA    R3,0(R5,R4)                                                      
         MVI   0(R3),0                                                          
         CLI   TWXSW,C'Y'          TEST PRINT TWX STUFF                         
         BE    FP02                                                             
         CLI   TLPROF7,C'Y'        TEST SUPPRESS RETURN ADDRESS                 
         BE    FP02                                                             
         CLI   TLPROF2,C'B'                                                     
         BNE   *+8                                                              
         MVI   0(R3),C'F'          'F' OF 'FROM'                                
*                                                                               
FP02     LA    R4,132(R4)                                                       
         LA    R3,0(R5,R4)                                                      
         MVI   0(R3),0                                                          
         CLI   TWXSW,C'Y'                                                       
         BNE   FP04                                                             
         CLI   STADTALN,119                                                     
         BE    FP10                                                             
         MVC   3(L'STATWX,R3),STATWX                                            
         B     FP10                                                             
*                                                                               
FP04     CLI   TLPROF7,C'Y'        TEST SUPPRESS RETURN ADDRESS                 
         BE    FP10                                                             
         MVC   3(33,R3),USERNAME   AGENCY NAME                                  
         CLI   TLPROF2,C'B'                                                     
         BNE   FP10                                                             
         MVI   0(R3),C'R'          'R' OF 'FROM'                                
*                                                                               
FP10     LA    R4,132(R4)                                                       
         LA    R3,0(R5,R4)                                                      
         MVI   0(R3),0                                                          
         CLI   TWXSW,C'Y'                                                       
         BNE   FP15                                                             
         CLI   STADTALN,119                                                     
         BE    FP20                                                             
         MVC   3(L'STATWXAB,R3),STATWXAB                                        
         B     FP20                                                             
*                                                                               
FP15     CLI   TLPROF7,C'Y'        TEST SUPPRESS RETURN ADDRESS                 
         BE    FP20                                                             
         GOTO1 CHOPPER,DMCB,(33,USERADDR),(24,3(R3)),(C'P',2)                   
         CLI   TLPROF2,C'B'                                                     
         BNE   FP20                                                             
         MVI   0(R3),C'O'          'O' OF 'FROM'                                
*                                                                               
FP20     LA    R4,132(R4)                                                       
         LA    R3,0(R5,R4)                                                      
         MVI   0(R3),0                                                          
         CLI   TWXSW,C'Y'                                                       
         BE    FP24                                                             
         CLI   TLPROF7,C'Y'        TEST SUPPRESS RETURN ADDRESS                 
         BE    FP24                                                             
         CLI   TLPROF2,C'B'                                                     
         BNE   *+8                                                              
         MVI   0(R3),C'M'          'M' OF 'FROM'                                
*                                                                               
FP24     LA    R4,132(R4)                                                       
         LA    R3,0(R5,R4)                                                      
         MVI   0(R3),0                                                          
         CLI   TLPROF8,C'Y'        SEE IF SUPPRESSING CLIENT NAME               
         BE    *+10                                                             
         MVC   3(20,R3),CLTNM      CLIENT NAME                                  
*                                                                               
         LA    R4,132(R4)                                                       
         LA    R3,0(R5,R4)                                                      
         MVI   0(R3),0                                                          
         CLI   TLPROF2,C'B'                                                     
         BNE   *+8                                                              
         MVI   0(R3),C'T'          'T' OF 'TO'                                  
*                                                                               
         LA    R4,132(R4)                                                       
         LA    R3,0(R5,R4)                                                      
         CLI   STAFOUND,C'Y'                                                    
         BE    *+14                                                             
         MVC   3(24,R3),=C'******** WARNING *******'                            
         B     *+10                                                             
         MVC   3(24,R3),STALINE1   STATION CALL LETTERS                         
         CLI   TLPROF2,C'B'                                                     
         BNE   *+8                                                              
         MVI   0(R3),C'O'          'O' OF 'TO'                                  
*                                                                               
         CLI   TLPROF6,C'Y'        ADDRESS TO OPERATIONS DESK                   
         BNE   FP30                NO                                           
         LA    R4,132(R4)                                                       
         LA    R3,0(R5,R4)                                                      
         MVC   BYTE,BAGYMD         DETERMINE MEDIA                              
         NI    BYTE,X'0F'                                                       
         MVC   3(24,R3),=CL24'** TV TRAFFIC DESK **'                            
         CLI   QMED,C'T'           TEST MEDIA TV                                
         BE    FP30                                                             
         MVC   3(24,R3),=CL24'RADIO TRAFFIC DESK'                               
         CLI   QMED,C'R'           TEST RADIO                                   
         BE    FP30                                                             
         CLI   QMED,C'X'           NETWORK RADIO                                
         BE    FP30                                                             
         MVC   3(24,R3),=CL24'**  TRAFFIC DESK **'                              
*                                                                               
FP30     LA    R4,132(R4)                                                       
         LA    R3,0(R5,R4)                                                      
         CLI   STAFOUND,C'Y'                                                    
         BE    *+14                                                             
         MVC   3(24,R3),=C'*  NO STATION ADDRESS  *'                            
         B     *+10                                                             
         MVC   3(24,R3),STALINE2   STATION ADDRESS LINE 1                       
         MVI   0(R3),0                                                          
*                                                                               
         LA    R4,132(R4)                                                       
         LA    R3,0(R5,R4)                                                      
         CLI   STAFOUND,C'Y'                                                    
         BE    *+20                                                             
         MVC   3(24,R3),=C'*  RECORD FOR          *'                            
         MVC   17(7,R3),STAPRNT                                                 
         B     *+10                                                             
         MVC   3(24,R3),STALINE3   STATION ADDRESS LINE 2                       
         MVI   0(R3),0                                                          
*                                                                               
         LA    R4,132(R4)                                                       
         LA    R3,0(R5,R4)                                                      
         CLI   STAFOUND,C'Y'                                                    
         BE    *+14                                                             
         MVC   3(24,R3),=33C'*'                                                 
         B     *+10                                                             
         MVC   3(24,R3),STALINE4   STATION ADDRESS LINE 3                       
         MVI   0(R3),0                                                          
*                                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
* PRINT ROW OF LABELS                                                           
*                                                                               
PRTLABEL NTR1                                                                   
*                                                                               
         SR    R5,R5               NO LINES PRINTED SO FAR                      
*                                                                               
         CLI   TLPROF5,14          SEE IF BELOW 14 LINES/LABEL                  
         BL    PL5                 YES - I MUST BE SKIPPING AGY NAME            
         MVC   P1,PX1              PRINT AGENCY ADDRESS BLOCK                   
         MVC   P2,PX2                                                           
         MVC   P3,PX3                                                           
         MVC   P4,PX4                                                           
         LA    R5,4(R5)            4 MORE LINES                                 
         BAS   RE,SPOOLIT                                                       
*                                                                               
PL5      CLI   TLPROF5,22  SEE IF BELOW 22 LINES/LABEL                          
         BNL   PL10        NO - DO WHAT I DID BEFORE (SKIP 9 LINES)             
*                                                                               
         ZIC   R3,TLPROF5          FIND LINES TO SKIP                           
         SH    R3,=H'13'                                                        
         BNP   PL15                DON'T SKIP ANY LINES                         
         B     PL11                                                             
*                                                                               
PL10     LA    R3,9                SKIP 9 LINES                                 
PL11     MVI   P1,0                                                             
         LA    R5,1(R5)            9 MORE LINES                                 
         BAS   RE,SPOOLIT                                                       
         BCT   R3,*-12                                                          
*                                                                               
PL15     MVC   P1,PX5              PRINT CLIENT NAME                            
         MVI   P2,0                SKIP 2 LINES                                 
         MVI   P3,0                                                             
         MVC   P4,PX6              'T' IN 'TO'                                  
         LA    R5,4(R5)            4 MORE LINES                                 
         BAS   RE,SPOOLIT                                                       
*                                                                               
         MVC   P1,PX7              PRINT STATION ADDRESS BLOCK                  
         MVC   P2,PX8                                                           
         MVC   P3,PX9                                                           
         MVC   P4,PX10                                                          
         LA    R5,4(R5)            4 MORE LINES                                 
         BAS   RE,SPOOLIT                                                       
*                                                                               
         MVC   P1,PX11             LAST LINE                                    
         LA    R5,1(R5)            1 MORE LINE                                  
         BAS   RE,SPOOLIT                                                       
*                                                                               
         ZIC   R0,TLPROF5          NUMBER OF LINES PER LABEL                    
         SR    R0,R5               NUMBER OF LINES TO SKIP                      
         LTR   R0,R0                                                            
         BZ    PL20                DON'T SKIP ANY LINES                         
         BP    *+6                 CAN'T SKIP NEGATIVE NUMBER OF LINES          
         DC    H'00'                                                            
         MVI   P1,0                SKIP REMAINING LINES                         
         BAS   RE,SPOOLIT                                                       
         BCT   R0,*-8                                                           
*                                                                               
PL20     LA    R0,11               BLANK OUT PRINT LINES                        
         LA    R1,PX                                                            
         XC    0(132,R1),0(R1)                                                  
         LA    R1,132(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         B     EXIT                                                             
         SPACE 5                                                                
SPOOLIT  NTR1                                                                   
*                                                                               
         MVI   LINE,1              FORCE CONSTANT SINGLE SPACING                
         GOTO1 SPOOL,DMCB,(R8)     SPOOL THE OUTPUT LINE                        
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE DATE(S) *                                                            
*                                                                               
VDATE    NTR1                                                                   
         CLI   5(R2),0                       DATE IS REQUIRED                   
         BE    MISSERR                                                          
*                                                                               
         XC    SDATE,SDATE                   CLEAR OLD DATES                    
         XC    EDATE,EDATE                                                      
         XC    SPDATE,SPDATE                                                    
         XC    EPDATE,EPDATE                                                    
*                                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK    START DATE                         
         L     R5,0(R1)                      LENGTH OF START DATE               
         LTR   R5,R5                                                            
         BZ    DATERR                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,SDATE)                                   
         GOTO1 DATCON,DMCB,(3,SDATE),(0,SPDATE)                                 
*                                                                               
         LA    R3,9(R2,R5)                   END DATE                           
         OC    0(2,R3),0(R3)                 TEST END DATE GIVEN                
         BZ    VDATEX                        NO, SO LEAVE                       
*                                                                               
         GOTO1 DATVAL,DMCB,(0,0(R3)),WORK                                       
         OC    0(4,R1),0(R1)                 TEST ZERO LENGTH                   
         BZ    DATERR                        YES, SO ERROR                      
         GOTO1 DATCON,DMCB,(0,WORK),(3,EDATE)                                   
         CLC   SDATE,EDATE                   START NOT GREATER END              
         BH    DATERR                                                           
         GOTO1 DATCON,DMCB,(3,EDATE),(0,EPDATE)                                 
*                                                                               
VDATEX   B     EXIT                                                             
         EJECT                                                                  
* VALIDATE FILTERS                                                              
*                                                                               
VFTR     NTR1                                                                   
*                                                                               
         MVI   CMMLLEN,0           LENGTH OF COMMERCIAL FILTER                  
         MVI   NOAGYSW,0           SET NOAGYSW                                  
         MVC   TRAMESS,SPACES      CLEAR MESSAGE FIELD                          
*                                                                               
         ZIC   R3,TRAFTRH+5        LENGTH OF INPUT                              
         LTR   R3,R3                                                            
         BZ    VFTRX               NO FILTERS IN FIELD                          
*                                                                               
         CLI   TRAFTR,C'?'         TEST HELP CHARACTER                          
         BE    HELPFTR             DISPLAY VALID FILTERS                        
*                                                                               
         CH    R3,=H'4'            LENGTH OF 'HELP'                             
         BH    VFTR10              NOT A HELP REQUEST                           
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   TRAFTR(0),=C'HELP'                                               
         BE    HELPFTR             IT'S A HELP REQUEST                          
*                                                                               
VFTR10   XC    SCANBLK,SCANBLK     CLEAR SCANNER BLOCK                          
         GOTO1 SCANNER,DMCB,TRAFTRH,(5,SCANBLK)                                 
         ZIC   R6,DMCB+4           NUMBER OF BLOCKS                             
         CH    R6,=H'1'            THERE CAN ONLY BE ONE BLOCK                  
         BH    TOOMNYFL                                                         
         LA    R5,SCANBLK          ADDRESS OF FIRST BLOCK                       
*                                                                               
VFTR20   ZIC   R1,0(R5)            LENGTH OF FIRST SUBFIELD                     
         LA    R3,11(R1,R5)        LAST CHARACTER FOUND                         
         CLI   0(R3),X'4C'         LESS THAN SIGN                               
         BE    BADSIGN             NOT ALLOWED                                  
         CLI   0(R3),X'6E'         GREATER THAN SIGN                            
         BE    BADSIGN             NOT ALLOWED                                  
*                                                                               
         CLI   0(R5),4             LENGTH OF 'CMML'                             
         BH    FTRERR                                                           
         BL    VFTR30                                                           
         CLC   12(4,R5),=C'CMML'                                                
         BE    VFTR40                                                           
         CLC   12(4,R5),=C'LIST'                                                
         BE    VFTR50                                                           
         B     FTRERR                                                           
*                                                                               
VFTR30   CLI   0(R5),3             LENGTH OF 'CML','COM'                        
         BL    FTRERR                                                           
         CLC   12(3,R5),=C'CML'                                                 
         BE    VFTR40                                                           
         CLC   12(3,R5),=C'COM'                                                 
         BE    VFTR40                                                           
         B     VFTR60                                                           
*                                                                               
VFTR40   ZIC   R1,1(R5)            LENGTH OF SECOND SUBFIELD                    
         CH    R1,=H'8'            MAX LENGTH OF COMMERICAL NAME                
         BH    COMERR              COMMERCIAL NAME TOO LONG                     
         LTR   R1,R1                                                            
         BZ    COMERR              NO COMMERICAL NAME                           
*                                                                               
         STC   R1,CMMLLEN          LENGTH OF COMMERCIAL FILTER                  
         MVC   CMML,22(R5)         SAVE FILTER                                  
         BAS   RE,VCML             VALIDATE THE COMMERCIAL NAME                 
         B     VFTR98                                                           
*                                                                               
VFTR50   ZIC   R1,1(R5)            LENGTH OF SECOND SUBFIELD                    
         CH    R1,=H'7'            MAX LEN OF LIST RECORD DESCRIPTION           
         BH    LISTERR             LIST DESCRIPTION TOO LONG                    
         LTR   R1,R1                                                            
         BZ    LISTERR             NO LIST DESCRIPTION                          
*                                                                               
         XC    LISTDESC,LISTDESC   CLEAR DESCRIPTION                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LISTDESC(0),22(R5)  SAVE DESCRIPTION                             
         BAS   RE,VSTALST          VALIDATE STATION LIST DESCRIPTION            
         B     VFTR98                                                           
*                                                                               
VFTR60   CLI   0(R5),5             LENGTH OF 'NOAGY'                            
         BL    FTRERR                                                           
         CLC   12(5,R5),=C'NOAGY'                                               
         BNE   FTRERR                                                           
         MVI   NOAGYSW,C'Y'                                                     
*                                                                               
VFTR98   LA    R5,32(R5)           POINT TO NEXT BLOCK                          
         BCT   R6,VFTR20                                                        
*                                                                               
VFTRX    OI    TRAMESSH+6,X'80'                                                 
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE COMMERCIAL                                                           
*                                                                               
VCML     NTR1                                                                   
*                                                                               
         ZIC   R3,CMMLLEN          LENGTH OF COMMERICAL FILTER                  
         CH    R3,=H'8'            TEST FOR FULL COMMERICAL NAME                
         BNE   VCMLX                                                            
*                                                                               
         USING CMLRECD,R4                                                       
         LA    R4,KEY                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   CMLKID,=X'0A21'     COMMERICAL PROFILE RECORD                    
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,BCLT                                                     
         MVC   CMLKCML,CMML                                                     
*                                                                               
         GOTO1 HIGH                LOOK FOR COMMERCIAL                          
         CLC   KEY(13),KEYSAVE     WAS IT THERE                                 
         BNE   COMERR              NO                                           
*                                                                               
VCMLX    B     EXIT                                                             
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE THE STATION LIST DESCRIPTION                                         
*                                                                               
VSTALST  NTR1                                                                   
*                                                                               
         USING LSTRECD,R4                                                       
         LA    R4,KEY                                                           
*                                                                               
         XC    KEY,KEY             LOOK FOR EXACT MATCH                         
         MVC   LSTKID,=X'0A2F'                                                  
         MVC   LSTKAM,BAGYMD                                                    
         MVC   LSTKCLT,BCLT                                                     
         MVC   LSTKPRD,BPRD                                                     
         MVC   LSTKDESC,LISTDESC                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   *+14                                                             
         MVC   TRAMESS,SPACES                                                   
         B     VSTALSTX                                                         
*                                                                               
         XC    KEY,KEY             LOOK FOR PRODUCT-GENERIC RECORD              
         MVC   LSTKID,=X'0A2F'                                                  
         MVC   LSTKAM,BAGYMD                                                    
         MVC   LSTKCLT,BCLT                                                     
         MVC   LSTKDESC,LISTDESC                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   *+14                                                             
         MVC   TRAMESS(58),=C'(PRODUCT NOT FOUND -- CLIENT LEVEL LABEL +        
               LIST RECORD USED)'                                               
         B     VSTALSTX                                                         
*                                                                               
         XC    KEY,KEY             LOOK FOR CLIENT-GENERIC RECORD               
         MVC   LSTKID,=X'0A2F'                                                  
         MVC   LSTKAM,BAGYMD                                                    
         MVC   LSTKDESC,LISTDESC                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   LISTERR                                                          
         MVC   TRAMESS(56),=C'(CLIENT NOT FOUND -- MEDIA LEVEL LABEL LI+        
               ST RECORD USED)'                                                 
*                                                                               
VSTALSTX MVC   STLSTKEY,KEY        SAVE KEY FOR PROCESSING LATER                
         B     EXIT                                                             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
*                                                                               
NORECERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NORECMSG),NORECMSG                                     
         LA    R2,TRAMEDH                                                       
         B     STAEREX2                                                         
*                                                                               
BADPRDER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADPRDMS),BADPRDMS                                     
         B     STAEREX2                                                         
*                                                                               
BADESTER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADESTMS),BADESTMS                                     
         B     STAEREX2                                                         
*                                                                               
HELPFTR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'HELPFTRM),HELPFTRM                                     
         B     STAEREX2                                                         
*                                                                               
FTRERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTRERRMS),FTRERRMS                                     
         B     STAEREX2                                                         
*                                                                               
BADSIGN  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADSIGNM),BADSIGNM                                     
         B     STAEREX2                                                         
*                                                                               
COMERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'COMERRMS),COMERRMS                                     
         B     STAEREX2                                                         
*                                                                               
LISTERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'LISTERRM),LISTERRM                                     
         B     STAEREX2                                                         
*                                                                               
SUPPERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'SUPPERRM),SUPPERRM                                     
         B     STAEREX2                                                         
*                                                                               
LINCTERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'LINERRM),LINERRM                                       
         B     STAEREX2                                                         
*                                                                               
TOOMNYFL XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TOOMNYFM),TOOMNYFM                                     
         GOTO1 ERREX2                                                           
*                                                                               
NOTDDS   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOTDDSMS),NOTDDSMS                                     
         B     STAEREX2                                                         
*                                                                               
NOPROF   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOPROFMS),NOPROFMS                                     
         B     STAEREX2                                                         
*                                                                               
ESTCDER  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'ESTCDMS),ESTCDMS                                       
         B     STAEREX2                                                         
*                                                                               
ESTNOTAL XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'ESTNOTMS),ESTNOTMS                                     
         B     STAEREX2                                                         
*                                                                               
BADALLST XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADALLMS),BADALLMS                                     
STAEREX2 GOTO1 ERREX2                                                           
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
         GOTO1 ERREX                                                            
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
         EJECT                                                                  
         LTORG                                                                  
NORECMSG DC    C'* NOTE * NO LABELS GENERATED *'                                
BADPRDMS DC    C'* ERROR * NO PRODUCT IN SHIPPING RECAP KEY *'                  
BADESTMS DC    C'* ERROR * NO ESTIMATE IN SHIPPING RECAP KEY *'                 
HELPFTRM DC    C'VALID FILTERS - CML/COM/CMML/LIST/NOAGY='                      
FTRERRMS DC    C'* ERROR * VALID FILTERS - CML/COM/CMML/LIST= *'                
BADSIGNM DC    C'* ERROR * GREATER/LESS THAN NOT PERMITTED *'                   
COMERRMS DC    C'* ERROR * INVALID COMMERICAL NAME *'                           
LISTERRM DC    C'* ERROR * INVALID STATION LIST DESCRIPTION *'                  
SUPPERRM DC    C'* ERROR * MUST HAVE RETURN ADDRESS ON BLANK LABELS *'          
BADALLMS DC    C'* ERROR * NOT PERMITTED FOR STATION ''ALL'' *'                 
LINERRM  DC    C'* ERROR * LINES/LABEL MUST BE > 13 *'                          
TOOMNYFM DC    C'* ERROR * TOO MANY FILTERS SPECIFIED *'                        
NOTDDSMS DC    C'* ERROR * LABELS MAY ONLY BE PRINTED REMOTE *'                 
NOPROFMS DC    C'* ERROR * TRAFFIC LABELS PROFILE MISSING *'                    
ESTCDMS  DC    C'* ERROR * ESTIMATE CODE NOT FOUND *'                           
ESTNOTMS DC    C'* ERROR * T1 PROF 11 NOT SET TO RUN BY EST *'                  
         EJECT                                                                  
       ++INCLUDE SPTRINST                                                       
         EJECT                                                                  
       ++INCLUDE SPTRSHIP                                                       
         EJECT                                                                  
       ++INCLUDE SPTRSTA                                                        
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRLBLS                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
         PRINT OFF                                                              
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRADBD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SPTR4BRR DS    F                   RELOCATION FACTOR                            
*                                                                               
TLPROF1  EQU   SVPROF+0     A      INSTRUCTION/SHIPPING RECORDS                 
TLPROF2  EQU   SVPROF+1     A      PREPRINTED/BLANK LABELS                      
TLPROF3  EQU   SVPROF+2     N      NUMBER OF LABELS ACROSS                      
TLPROF4  EQU   SVPROF+3     N      WIDTH OF A LABEL                             
TLPROF5  EQU   SVPROF+4     N      LINES PER LABEL                              
TLPROF6  EQU   SVPROF+5     A      SEND TO "TV OPERATIONS DESK"                 
TLPROF7  EQU   SVPROF+6     A      SUPPRESS RETURN ADDRESS                      
TLPROF8  EQU   SVPROF+7     A      SUPPRESS CLIENT NAME                         
TLPROF9  EQU   SVPROF+8                                                         
TLPROF10 EQU   SVPROF+9                                                         
TLPROF11 EQU   SVPROF+10                                                        
TLPROF12 EQU   SVPROF+11                                                        
TLPROF13 EQU   SVPROF+12                                                        
TLPROF14 EQU   SVPROF+13                                                        
TLPROF15 EQU   SVPROF+14                                                        
TLPROF16 EQU   SVPROF+15                                                        
*                                                                               
PROD     DS    CL3                 ALPHA PRODUCT CODE                           
SDATE    DS    XL3                 START DATE (YYMMDD)                          
EDATE    DS    XL3                 END DATE   (YYMMDD)                          
SPDATE   DS    CL8                 START DATE (EBCDIC)                          
EPDATE   DS    CL8                 END DATE   (EBCDIC)                          
CMML     DS    CL8                 COMMERCIAL FILTER                            
CMMLLEN  DS    XL1                 LENGTH OF COMMERICAL FILTER                  
LISTDESC DS    CL7                 DESCRIPTION OF STATION LIST                  
LISTFLAG DS    CL1                 'Y' IF 'LIST=' OPTION WAS ENTERED            
ALLSTASW DS    CL1                 'Y' IF STATION = ALL                         
TWXSW    DS    CL1                 'Y' IF PRINT TWX, NOT RETURN ADDRESS         
NOAGYSW  DS    CL1                 'Y' ALLOW NO AGY ADDR ON BLK LABELS          
ALLSTKEY DS    XL3                 KEY FOR STATION ADDRESS RECORDS              
STLSTKEY DS    CL13                KEY OF STATION LIST RECORD                   
TMKTSTA  DS    0XL5                TEMPORARY STORAGE                            
TMKT     DS    XL2                 WILL BE X'0000'                              
TSTA     DS    XL3                 STATION IN USER LIST                         
HOLDSTA  DS    XL5                 MKT ZERO, WITH STATION                       
FOUNDSW  DS    XL1                 NON-ZERO IF ANY LABEL WAS GENERATED          
ROWDONE  DS    CL1                 'N' IF THERE'S AN INCOMPLETE ROW             
MYBMKT   DS    XL2                 SAVE BINARY MARKET                           
CURDISP  DS    F                   BEGINNING OF PRINT COLUMN                    
CURLABEL DS    F                   CURRENT COLUMN OF LABELS                     
STAFOUND DS    CL1                 'Y' IF STATION ADDRESS RECORD FOUND          
SCANBLK  DS    CL256               SCANNER BLOCK FOR INPUT FIELDS               
PX       DS    0CL132              TEMPORARY PRINT LINES                        
PX1      DS    CL132                                                            
PX2      DS    CL132                                                            
PX3      DS    CL132                                                            
PX4      DS    CL132                                                            
PX5      DS    CL132                                                            
PX6      DS    CL132                                                            
PX7      DS    CL132                                                            
PX8      DS    CL132                                                            
PX9      DS    CL132                                                            
PX10     DS    CL132                                                            
PX11     DS    CL132                                                            
STATABLE DS    0CL3                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056SPTRA4B   06/08/01'                                      
         END                                                                    
