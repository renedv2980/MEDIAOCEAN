*          DATA SET SPSFM28    AT LEVEL 110 AS OF 09/04/15                      
*PHASE T21728A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21728  -- MARKET  GROUP MAINTENANCE                 *         
*                                                                     *         
*  COMMENTS:     MAINTAINS OFFICE RECORDS ON SPFILE                   *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS SPSFMA8, AND SPSFMA9                         *         
*                                                                     *         
*  OUTPUTS:      UPDATED OFFICE RECORDS                               *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21728 - MARKET GROUP RECORD MAINTENANCE'                       
T21728   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1728**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BRAS  RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY RECORD                               
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS                                                    
         BE    LR                  LIST RECORDS                                 
         CLI   MODE,RECDEL         DELETE A RECORD?                             
         BE    DEL                 YES - MEDIA N/C NOT ALLOWED                  
         CLI   MODE,RECREST        RESTORE A RECORD?                            
         BE    DEL                 YES - MEDIA N/C NOT ALLOWED                  
         CLI   MODE,XRECDEL        JUST DELETED A MARKET GROUP?                 
         BNE   *+8                 NO                                           
         BRAS  RE,XDEL             YES - UPDATE PASSIVE & MGR ASSIGN            
         CLI   MODE,XRECREST       JUST RESTORED A MARKET GROUP?                
         BNE   *+12                NO                                           
         BAS   RE,DREC             YES - RE-DISPLAY THE RECORD                  
         BRAS  RE,XRES             YES - RESTORE CANADIAN N/C MGROUPS           
         CLI   MODE,XRECADD                                                     
         BE    XR                  ADD/DEL PASSIVE KEYS                         
         CLI   MODE,XRECPUT                                                     
         BE    XR                  ADD/DEL PASSIVE KEYS                         
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         L     R5,=A(HEDSPECS)                                                  
         A     R5,RELO                                                          
         ST    R5,SPECS                                                         
         B     LR                  PRINT REPORT BASE ON LISTED RECORDS          
*                                                                               
XIT      XIT1                                                                   
********************************************************************            
*                      VALIDATE KEY                                             
********************************************************************            
VK       DS    0H                                                               
         MVI   MEDIA,0                                                          
         XC    CLIENT,CLIENT                                                    
         XC    PGRPNUM,PGRPNUM                                                  
         XC    SVBCLT,SVBCLT                                                    
         MVI   SVBAGYMD,0                                                       
         MVI   BKNUM,0                                                          
         XC    STARTNO,STARTNO                                                  
         MVI   AGYHIGH,0                                                        
*                                                                               
         LA    R2,MGRMEDH               VALIDATE MEDIA                          
*                                                                               
         CLI   5(R2),0                  IS MEDIA ENTERED IN?                    
         BE    *+8                                                              
         B     VK65                                                             
*                                                                               
         CLI   ACTEQU,ACTLIST           IF NO MEDIA ITS OKAY IF                 
         BNE   ERRMIS                   ACTIONLIST                              
*                                                                               
         XC    FAKEFLD,FAKEFLD          DEFAULT IS TELEVISION                   
         MVI   FAKEFLD+8,C'T'                                                   
         MVI   FAKEFLD+5,1                                                      
         LA    R2,FAKEFLD                                                       
         GOTO1 VALIMED                  GET AGENCY                              
         MVC   SVBAGYMD,BAGYMD                                                  
         MVC   AGYHIGH,SVBAGYMD                                                 
*                                                                               
         OI    AGYHIGH,X'0F'             AGYHIGH IS HIGHEST MEDIA ALLOW         
         MVI   MGRMED,C'T'               FOR THIS AGENCY                        
         MVI   MGRMEDH+5,1                                                      
         OI    MGRMEDH+6,X'80'                                                  
*                                                                               
         B     VK67                     CHECK REST OF FIELDS!                   
*                                                                               
VK65     GOTO1 VALIMED                                                          
         MVC   SVBAGYMD,BAGYMD          SAVE AGENCY MEDIA CODE                  
         MVC   AGYHIGH,SVBAGYMD                                                 
         OI    AGYHIGH,X'0F'                                                    
         CLI   ACTEQU,ACTLIST                                                   
         BE    VK67                                                             
*                                                                               
         MVC   MGRMDN,MEDNM             DISPLAY MEDIA NAME                      
         OI    MGRMDNH+6,X'80'                                                  
         B     VK68                                                             
*                                                                               
VK67     MVC   LISMDN,MEDNM             IF ACTION LIST DISPLAY                  
         OI    LISMDNH+6,X'80'          MEDIA NAME ON LIST SCREEN               
*                                                                               
VK68     LA    R2,MGRCLTH               VALIDATE CLIENT                         
         CLI   5(R2),0                                                          
         BNE   VK70                                                             
*                                                                               
         CLI   ACTEQU,ACTLIST           LIST?                                   
         BNE   ERRINV                   IF NO CLIENT AND NOT LIST               
         XC    LISCLN,LISCLN            ITS NOT VALID                           
         OI    LISCLNH+6,X'80'                                                  
         CLI   MGRMKGNH+5,0             ANY MKTGRP INPUT?                       
         BE    VK200                    NOPE                                    
         MVC   MGRCLT,=C'ALL'           YES, ASSUME CLIENT 'ALL'                
         MVI   MGRCLTH+5,3                                                      
         OI    MGRCLTH+6,X'80'                                                  
*                                                                               
VK70     CLC   =C'ALL',MGRCLT                                                   
         BNE   VK75                                                             
*                                                                               
         XC    SVBCLT,SVBCLT            CLIENT = ALL                            
         MVC   CLIENT,=C'ALL'                                                   
         BNE   VK75                                                             
         XC    CLTNM,CLTNM                                                      
         B     VK78                                                             
*                                                                               
VK75     GOTO1 VALICLT                 VALIDATE CLIENT                          
         MVC   SVBCLT,BCLT             SAVE CLIENT CODE                         
         CLI   ACTEQU,ACTLIST                                                   
         BE    VK78                                                             
*                                                                               
         MVC   MGRCLN,CLTNM            DISPLAY CLIENT NAME                      
         OI    MGRCLNH+6,X'80'                                                  
         B     VK80                                                             
*                                                                               
VK78     MVC   LISCLN,CLTNM                                                     
         OI    LISCLNH+6,X'80'                                                  
*                                                                               
VK80     MVC   CLIENT,MGRCLT                                                    
*                                                                               
VK85     LA    R2,MGRPGPH               VALID PRODUCT GROUP                     
         CLI   5(R2),0                                                          
         BNE   VK88                                                             
         CLI   ACTEQU,ACTLIST           LIST?                                   
         BNE   ERRINV                   INVALID IF NOT LIST                     
         CLI   MGRMKGNH+5,0             ANY MKTGRP INPUT?                       
         BE    VK200                    NOPE                                    
         MVC   MGRPGP,=C'ALL'           YES, ASSUME PRDGRP 'ALL'                
         MVI   MGRPGPH+5,3                                                      
         OI    MGRPGPH+6,X'80'                                                  
*                                                                               
VK88     MVC   PGRPNUM(L'PGRPNUM),8(R2)  SAVE PRODUCT GROUP NUMBER              
         B     VALP00                    VALIDATE PRODUCT GROUP NAME            
*                                                                               
VK90     LA    R2,MGRMKGNH               VALIDATE MARKET  GROUP ID              
         CLI   5(R2),0                                                          
         BNE   VK100                                                            
         CLI   ACTEQU,ACTLIST           LIST?                                   
         BNE   ERRINV                   EXIT FOR NOW                            
         B     VK200                                                            
                                                                                
*==========================================================                     
*   VALIDATE MKTGRP ID AND NUMBER                                               
*==========================================================                     
                                                                                
VK100    SR    RE,RE                                                            
         IC    RE,5(R2)            GET INPUT LENGTH                             
*                                                                               
         CLI   ACTEQU,ACTLIST      LIST?                                        
         BNE   VK100Z              NO, REGULAR VALIDATION                       
         CLI   5(R2),2             INPUT LENGTH > 2?                            
         BH    VK100A              YES, DEFINITELY HAS MGROUP NUMBER            
         CLI   9(R2),C'Z'          HAVE MGROUP NUMBER?                          
         BH    VK100A              YES, 1 CHAR MGROUP + NUM USED                
         MVC   FULL(2),8(R2)                                                    
         OI    FULL+1,X'40'        SPACE PADDED                                 
         XC    QMGRP,QMGRP         GROUP NUMBER                                 
         MVI   QMGRPLN,0           LENGTH OF GROUP NUM                          
         B     VK103                                                            
*                                                                               
VK100A   CLI   8(R2),C'='          MARKET FILTER?                               
         BNE   VK100Z                                                           
         CLI   5(R2),2                                                          
         BL    ERRINV              MUST HAVE ATLEAST "=#"                       
         CLI   5(R2),5                                                          
         BH    ERRINV              AND CAN'T HAVE =#####                        
         LLC   R1,5(R2)                                                         
         LA    RF,7(R1,R2)                                                      
         LA    RE,8(R2)                                                         
VK100B   CR    RF,RE               AT BEGINNING?                                
         BE    VK100C                                                           
         CLI   0(RF),C'0'                                                       
         BL    ERRINV                                                           
         CLI   0(RF),C'9'                                                       
         BH    ERRINV                                                           
         BCT   RF,VK100B                                                        
         DC    H'0'       SHOULD NEVER GET HERE, BUT JUST IN CASE DIE           
*                                                                               
VK100C   AHI   R1,-2                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,9(0,R2)                                                      
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    ERRINV                                                           
         STCM  R0,3,QMARKET                                                     
         B     VK200                                                            
*                                                                               
VK100Z   MVC   FULL(1),8(R2)                                                    
         MVI   FULL+1,C' '                                                      
         LA    RF,9(R2)            POINT TO FIRST CHAR OF GRP                   
         BCTR  RE,0                ADJUST REMAINING LEN                         
*                                                                               
         LTR   RE,RE                                                            
         BNP   ERRINV                                                           
         STC   RE,QMGRPLN          SAVE LENGTH OF GRP NUM INPUT                 
*                                                                               
         CLI   9(R2),C'Z'          TEST SECOND CHAR IS ALPHA                    
         BH    VK102               NO                                           
*                                                                               
         MVC   FULL(2),8(R2)                                                    
         LA    RF,10(R2)                                                        
         BCTR  RE,0                                                             
*                                                                               
         LTR   RE,RE                                                            
         BNP   ERRINV                                                           
         STC   RE,QMGRPLN          SAVE LENGTH OF GRP NUM INPUT                 
*                                                                               
         MVC   DUB,=C'0000'                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),0(RF) *EXECUTED*                                          
         PACK  WORK(3),DUB(5)      PACK 1 EXTRA CHAR                            
         MVC   QMGRP,WORK          AND SAVE GROUP AS INPUT                      
         OC    QMGRP,QMGRP         CANNOT BE ZERO                               
         BZ    ERRINV                                                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,QMGRPLN                                                       
         LA    RF,DUB              DUB HAS EBCDIC INPUT                         
*                                                                               
VK101    CLI   0(RF),C'0'          MAKE SURE NUMERIC                            
         BL    ERRINV                                                           
         CLI   0(RF),C'9'                                                       
         BH    ERRINV                                                           
         LA    RF,1(RF)                                                         
         BCT   RE,VK101                                                         
         B     VK103                                                            
*                                                                               
* TEST FOR VALID GROUP CODE                                                     
*                                                                               
VK102    MVC   DUB,=C'0000'                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),0(RF) *EXECUTED*                                          
         PACK  WORK(3),DUB(5)      PACK 1 EXTRA CHAR                            
         MVC   QMGRP,WORK          AND SAVE GROUP AS INPUT                      
         OC    QMGRP,QMGRP         CANNOT BE ZERO                               
         BZ    ERRINV                                                           
*                                                                               
VK103    L     RE,=A(SPMGRTAB)                                                  
         A     RE,RELO                                                          
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
*                                                                               
VK104    CLC   FULL(2),0(RE)                                                    
         BE    VK106                                                            
         LA    RE,3(RE)                                                         
         BCT   RF,VK104                                                         
         B     ERRINV                                                           
*                                                                               
VK106    MVC   QMGR,2(RE)          SAVE MKTGRPID CHAR                           
*                                                                               
VK110    CLI   QMGR,C' '           01-3F ARE ALL CLIENT GROUPS                  
         BL    VK120                                                            
         CLI   QMGR,C'F'                                                        
         BH    VK120                                                            
* QMGR IS A-F, SO CLIENT REQ'D                                                  
         CLC   MGRCLT,=C'ALL'                                                   
         BE    ERRINV                                                           
*                                                                               
* IF QMGR NOT A-F, PRDGRP MUST BE ALL                                           
*                                                                               
VK120    LA    R2,MGRPGPH          VALID PRODUCT GROUP                          
         CLI   QMGR,C' '           01-3F ARE ALL CLIENT SCHEMES                 
         BL    VK122                                                            
         CLI   QMGR,C'F'                                                        
         BNH   VK124                                                            
*                                                                               
VK122    CLC   MGRPGP(3),=C'ALL'        PRDGRP MUST BE ALL                      
         BNE   ERRINV                                                           
*                                                                               
VK124    CLI   5(R2),1                  PGRPNUM MUST BE PRESENT                 
         BNL   VK130                                                            
         CLI   ACTEQU,ACTLIST           LIST?                                   
         BNE   ERRINV                   EXIT FOR NOW                            
         B     VK200                                                            
*                                                                               
*  BUILD KEY TO GET MARKET DEF BREAK LENGTHS                                    
*                                                                               
VK130    CLI   ACTEQU,ACTLIST           LIST?                                   
         BE    VK200                    EXIT FOR NOW                            
*                                                                               
         LA    R4,KEY                                                           
         USING MKGRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   MKGKTYP,=X'0D02'                                                 
         MVC   MKGKAGMD,SVBAGYMD                                                
         MVC   MKGKCLT,SVBCLT                                                   
         MVC   MKGKMID,QMGR                                                     
*                                                                               
*  IF EXCEPTION LIST FOR CLIENT ?  BUILT DIFF KEY WITH CLIENT=0000              
*                                                                               
         CLI   QMGR,C' '           IF 01-3F NO CLIENT                           
         BL    VK132                                                            
         CLI   QMGR,C'F'           OR MORE THAN F                               
         BNH   VK134                                                            
*                                                                               
VK132    XC    MKGKCLT,MKGKCLT     CLEAR CLIENT                                 
         DROP  R4                                                               
*                                                                               
VK134    GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE          IF MARKET DEFIN DOESN'T EXIST           
         BNE   NOMDEF                   MGROUP NOT VALID                        
*                                                                               
*    CHECK TO SEE IF CLIENT ENTERED IS IN EXECPTION LIST                        
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* NOTE: X'01'-X'3F'  IS FOR CLIENT EXCEPTION                                    
*       A-F QMGR     IS FOR CLIENT SPECIFIC                                     
*       G-Z QMGR     IS FOR CLIENT EXCEPTION                                    
*                                                                               
         CLI   QMGR,C' '           01-3F HAS NO CLIENT                          
         BL    *+12                NO CLIENT EXCEPTIONS                         
         CLI   QMGR,C'F'                                                        
         BNH   VK155                                                            
* NOT A-F                                                                       
         CLC   MGRCLT,=C'ALL'           IF CLIENT=ALL WE KNOW PGR =ALL          
         BE    VK170                                                            
*                                                                               
*  CHECK IF CLIENT ENTERED IS IN EXCEPTION LIST                                 
*                                                                               
VK135    L     R6,AIO                   THIS ELEMENT HAS THE EXCEPTIONS         
         USING MKGEL02,R6                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    VK150                                                            
         LA    R2,MGRCLTH                                                       
         B     ERRECP1                                                          
*                                                                               
VK140    BAS   RE,NEXTEL                                                        
         BE    VK150                                                            
         LA    R2,MGRCLTH                                                       
         B     ERRECP1                                                          
VK150    LLC   RE,MGRCLTH+5                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   2(0,R6),MGRCLT                                                   
         BNE   VK140                                                            
         B     VK170                                                            
*                                                                               
************  THIS PART IS FOR PRODUCT EXCEPTION CHECK  ***************         
*                                                                               
VK155    CLC   MGRPGP(3),=C'ALL'                                                
         BE    VK170                                                            
*  IF ONLY ID IS ENTERED SKIP EXECPTION LIST CHECK                              
         CLI   MGRPGPH+5,1                                                      
         BE    VK170                                                            
*  CHECK IF PRODUCT GROUP ENTERED IS IN EXECPTION LIST                          
         L     R6,AIO                                                           
         USING MKGEL02,R6                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    VK165                                                            
         LA    R2,MGRPGPH                                                       
         B     ERRECP2                                                          
*                                                                               
VK160    BAS   RE,NEXTEL                                                        
         BE    VK165                                                            
         LA    R2,MGRPGPH                                                       
         B     ERRECP2                                                          
*                                                                               
VK165    LLC   RE,MGRPGPH+5                                                     
         MVC   WORK(4),=C'0000'                                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
*                                                                               
         MVC   WORK(0),MGRPGP+1                                                 
         XC    DUB,DUB                                                          
         PACK  DUB(3),WORK(5)                                                   
         MVC   PGRPNO,DUB                                                       
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(1),MGRPGP                                                   
         MVC   FULL+1(2),PGRPNO                                                 
*                                                                               
         CLC   FULL(3),2(R6)       COMP TO SEE IF IN EXCEPTION LIST             
         BNE   VK160                                                            
*                                                                               
*  DISPLAY THE BREAK TITLES                                                     
*                                                                               
VK170    L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING MKGEL01,R6                                                       
*                                                                               
         LA    RF,1                                                             
         LLC   R0,MKGBK1LN                                                      
         LLC   R4,MKGBK2LN                                                      
         AR    R0,R4                                                            
         LTR   R4,R4                                                            
         BZ    VK180                                                            
*                                                                               
         LA    RF,1(RF)                                                         
         LLC   R4,MKGBK3LN                                                      
         AR    R0,R4                                                            
         LTR   R4,R4                                                            
         BZ    VK180                                                            
*                                                                               
         LA    RF,1(RF)                                                         
VK180    STC   RF,BKNUM                                                         
         MVC   MGRBK1,MKGBK1                                                    
         OI    MGRBK1H+6,X'80'                                                  
         MVC   MGRBK2,MKGBK2                                                    
         OI    MGRBK2H+6,X'80'                                                  
         MVC   MGRBK3,MKGBK3                                                    
         OI    MGRBK3H+6,X'80'                                                  
         DROP  R6                                                               
*                                                                               
         LA    R2,MGRMKGNH         POINT TO MKTGRP                              
*        CLI   9(R2),C'Z'          TEST SECOND CHAR IS ALPHA                    
*        BH    VK200               NO                                           
         CLM   R0,1,QMGRPLN        LENGTH INPUT CORRECT ?                       
         BNE   ERRMGLEN                                                         
*                                                                               
VK200    DS    0X                                                               
*                                                                               
         LA    R4,KEY                   BUILD MARKET GROUP KEY                  
         USING MKGRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   MKGKTYP,=X'0D02'                                                 
         MVC   MKGKAGMD,SVBAGYMD                                                
         MVC   MKGKCLT,SVBCLT           BINARY CLIENT CODE                      
         MVC   MKGKPID,PGRPID                                                   
         MVC   MKGKPGRP,PGRPNM                                                  
         MVC   MKGKMID,QMGR                                                     
         MVC   MKGKMGRP,QMGRP                                                   
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY,KEY              SAVE THE KEY                            
         DROP  R4                                                               
*                                                                               
VKX      MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
****************************************************************                
*              VALIDATE RECORD                                                  
*****************************************************************               
VR       CLI   SVAPROF+7,C'C'                                                   
         BNE   VR05                                                             
         LA    R2,MGRMEDH                                                       
         CLI   MGRMED,C'N'                                                      
         BE    ERRINV                                                           
         CLI   MGRMED,C'C'                                                      
         BE    ERRINV                                                           
*                                                                               
VR05     MVI   ELCODE,X'20'                                                     
         GOTO1 REMELEM                                                          
         LA    R4,ELEM                                                          
         USING MKGEL20,R4                                                       
         MVI   MKGEL20,X'20'                                                    
         MVI   MKGEL20+1,16                                                     
         CLI   MGRUSERH+5,0                                                     
         BE    VR7                                                              
         MVC   MKGLTACC,MGRUSER                                                 
         GOTO1 ADDELEM                                                          
                                                                                
*                                                                               
VR7      MVI   ELCODE,X'30'          MKGROUP ADDRESS                            
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING MKGEL30,R4                                                       
         MVI   MKGEL30,X'30'                                                    
         MVI   MKGEL30+1,122                                                    
*                                                                               
         LA    R2,MGRADD1H                                                      
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         MVC   MKGADDR1,8(R2)                                                   
         OC    MKGADDR1,SPACES                                                  
*                                                                               
         LA    R2,MGRADD2H                                                      
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         MVC   MKGADDR2,8(R2)                                                   
         OC    MKGADDR2,SPACES                                                  
*                                                                               
         LA    R2,MGRADD3H                                                      
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         MVC   MKGADDR3,8(R2)                                                   
         OC    MKGADDR3,SPACES                                                  
*                                                                               
         LA    R2,MGRADD4H                                                      
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         MVC   MKGADDR4,8(R2)                                                   
         OC    MKGADDR4,SPACES                                                  
*                                                                               
         OC    ELEM+2(120),ELEM+2        IS ELEM BLANK?                         
         BZ    VR10                                                             
         GOTO1 ADDELEM                  NO ADD THE ELEMENT                      
*                                                                               
VR10     MVI   ELCODE,X'10'                                                     
         GOTO1 REMELEM                                                          
*  JUST REMOVE ALL ELEMENTS AND REBUILD IT *                                    
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING MKGEL10,R4                                                       
         MVI   MKGEL10,X'10'                                                    
         MVI   MKGEL10+1,74                                                     
         LA    R2,MGRNM1H                                                       
         CLI   5(R2),0                                                          
         BE    ERRINV                                                           
         MVC   MKGNAM1,MGRNM1                                                   
*                                                                               
         LA    R2,MGRNM2H                                                       
         CLI   BKNUM,2                                                          
         BE    VR30                                                             
         BH    VR30                                                             
*                                                                               
         CLI   5(R2),0                IF  SECOND BREAK LENGTH IS 0              
         BH    ERRINV                                                           
         B     VR40                                                             
*                                                                               
VR30     CLI   5(R2),0                                                          
         BE    ERRINV                                                           
*                                                                               
VR40     MVC   MKGNAM2,MGRNM2                                                   
*                                                                               
         LA    R2,MGRNM3H                                                       
*                                                                               
         CLI   BKNUM,3                                                          
         BE    VR50                                                             
         BH    VR50                                                             
*                                                                               
         CLI   5(R2),0                IF  THIRD  BREAK LENGTH IS 0              
         BH    ERRINV                                                           
         B     VR60                                                             
*                                                                               
VR50     CLI   5(R2),0                                                          
         BE    ERRINV                                                           
VR60     MVC   MKGNAM3,MGRNM3                                                   
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
*                                                                               
*   THIS PART VALIDATES THE ADD MARKET CODES *                                  
         MVC   ADDMRT(22),=11X'FFFF'                                            
         LA    R3,ADDMRT                                                        
         LA    R2,MGRAM1H                                                       
         LA    R5,10                                                            
VR70     CLI   5(R2),0                                                          
         BE    VR100                                                            
*                                                                               
         TM    4(R2),X'08'         VALID NUMERIC                                
         BNO   ERRINV                                                           
*                                                                               
         LLC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         XC    DUB,DUB                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
*   BUILD KEY FOR PASSIVE KEY, TO SEE IF MARKET IS ALREADY DEFINED              
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MKGKEY,R4                                                        
         MVC   MKGPTYP,=X'0D82'                                                 
         MVC   MKGPAGMD,SVBAGYMD                                                
         MVC   MKGPCLT,SVBCLT                                                   
         MVC   MKGPPID,PGRPID                                                   
         MVC   MKGPPGRP,PGRPNM                                                  
         MVC   MKGPMID,QMGR                                                     
         MVC   MKGPMGRP,QMGRP                                                   
         MVC   MKGPMKT,HALF                                                     
*                                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE            IF MARKET ALREADY EXIST INVAL         
         BE    ERRMRKT                                                          
         DROP  R4                                                               
*                                                                               
*     TEST MULTIPLE MARKETS ON INPUT LINE                                       
         LA    R4,MGRAM1H                                                       
VR75     LR    RE,R4                                                            
*                                                                               
VR80     LLC   R0,0(RE)                                                         
         AR    RE,R0                                                            
         CR    RE,R2                                                            
         BH    VR85                                                             
*    IF THE TWO FIELDS ARE EQUAL INVALID                                        
         CLC   8(L'MGRAM1,R4),8(RE)                                             
         BNE   VR80                                                             
*  IF THE TWO ARE BLANKS ITS OK THAT BLANKS MATCH ELSE INVALID                  
         CLI   5(RE),0                                                          
         BE    VR80                                                             
         B     ERRDUPM                                                          
*                                                                               
VR85     CR    R4,R2                                                            
         BH    VR90                                                             
         LLC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         B     VR75                                                             
*                                                                               
*  AT THIS POINT THE MARKET IS DISTINCT AND INPUT FIELDS ARE VALID *            
*   JUST CHECK IF IT IS A VALID MARKET CODE                                     
*                                                                               
VR90     MVI   USEIONUM,2                                                       
         GOTO1 VALIMKT                                                          
*      STORE VALID MARKET CODE                                                  
         MVC   0(L'ADDMRT,R3),HALF                                              
         LA    R3,L'ADDMRT(R3)                                                  
*                                                                               
VR100    LLC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         BCT   R5,VR70                                                          
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         B     DR                                                               
*******************************************************************             
* DISPLAY RECORD                                                                
*                                                                               
DREC     NTR1                                                                   
DR       L     R6,AIO                                                           
*                                                                               
         MVI   ELCODE,X'10'        PROD GRP BREAK NAMES                         
         BAS   RE,GETEL                                                         
         BE    *+6                 REQUIRED ELEMENT                             
         DC    H'0'                DIE IF NOT PRESENT                           
*                                                                               
         BRAS  RE,CLRSCR                CLEAR SCREEN                            
*                                                                               
         USING MKGEL10,R6                                                       
         MVC   MGRNM1,MKGNAM1                                                   
         OI    MGRNM1H+6,X'80'                                                  
         MVC   MGRNM2,MKGNAM2                                                   
         OI    MGRNM2H+6,X'80'                                                  
         MVC   MGRNM3,MKGNAM3                                                   
         OI    MGRNM3H+6,X'80'                                                  
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR03                                                             
*   LIMITED ACCESS CODES                                                        
         USING MKGEL20,R6                                                       
         MVC   MGRUSER,MKGLTACC                                                 
         OI    MGRUSERH+6,X'80'                                                 
         DROP  R6                                                               
*   MKT GRP ADDRESSES                                                           
DR03     L     R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR05                                                             
*                                                                               
         USING MKGEL30,R6                                                       
         MVC   MGRADD1,MKGADDR1                                                 
         OI    MGRADD1H+6,X'80'                                                 
         MVC   MGRADD2,MKGADDR2                                                 
         OI    MGRADD2H+6,X'80'                                                 
         MVC   MGRADD3,MKGADDR3                                                 
         OI    MGRADD3H+6,X'80'                                                 
         MVC   MGRADD4,MKGADDR4                                                 
         OI    MGRADD4H+6,X'80'                                                 
*   MARKET CODES                                                                
DR05     LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING MKGKEY,R4                                                        
         MVC   MKGPTYP,=X'0D82'                                                 
         MVC   MKGPAGMD,SVBAGYMD                                                
         MVC   MKGPCLT,SVBCLT                                                   
         MVC   MKGPPID,PGRPID                                                   
         MVC   MKGPPGRP,PGRPNM                                                  
         MVC   MKGPMID,QMGR                                                     
         MVC   MKGPMGRP,QMGRP                                                   
         LA    R5,MGRML1H                                                       
*  GET BINARY VALUE OF MARKET START AT                                          
*                                                                               
         OC    MGRFIL,MGRFIL                                                    
         BZ    DR20                                                             
         TM    MGRFILH+4,X'08'                                                  
         BNZ   DR10                                                             
         LA    R2,MGRFILH                                                       
         B     ERRINV                                                           
*                                                                               
DR10     MVC   WORK(8),=C'00000000'                                             
         LLC   RE,MGRFILH+5                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
*                                                                               
         MVC   WORK(0),MGRFIL           MARKET NUMBER IS PACKED BINARY          
         XC    DUB,DUB                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK(0)                                                      
         CVB   RE,DUB                                                           
         STCM  RE,3,STARTNO                                                     
         B     DR30                                                             
*                                                                               
DR20     XC    STARTNO,STARTNO                                                  
*                                                                               
DR30     GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    DR60                                                             
         DC    H'0'                                                             
*                                                                               
DR40     GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    DR60                                                             
         DC    H'0'                                                             
*                                                                               
DR60     CLC   KEYSAVE(11),KEY                                                  
         BNE   DRX                                                              
         ZICM  R0,STARTNO,2                                                     
         ZICM  RE,MKGPMKT,2                                                     
*        CLC   STARTNO,MKGPMKT                                                  
         CR    R0,RE                                                            
         BH    DR80                                                             
         LA    R5,8(R5)                                                         
         EDIT  (B2,KEY+11),(4,(R5)),FILL=0                                      
         SHI   R5,8                                                             
         OI    6(R5),X'80'                                                      
         LLC   R0,0(R5)                                                         
         AR    R5,R0                                                            
         LA    R0,MGRENDH                                                       
         CR    R5,R0                                                            
         BH    DRX                                                              
DR80     B     DR40                                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
****************************************************************                
*               XRECPUT/XRECADD                                                 
****************************************************************                
*                                                                               
XR       CLI   SVAPROF+7,C'C'                                                   
         BNE   XR20                                                             
         CLI   QMED,C'T'                                                        
         BNE   XR20                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         NI    KEY+2,X'F0'                                                      
         OI    KEY+2,X'08'                                                      
         GOTO1 HIGH                                                             
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(L'MKGKEY),KEYSAVE                                            
         BNE   XR05                                                             
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO1                                                          
         L     R4,AIO2                                                          
         MVC   0(200,R4),0(R6)                                                  
         MVC   200(100,R4),200(R6)                                              
         NI    2(R4),X'F0'                                                      
         OI    2(R4),X'08'                                                      
         GOTO1 PUTREC                                                           
         B     XR10                                                             
*  RECORD NOT FOUND SO ADD NEW RECORD*                                          
XR05     L     R6,AIO1                                                          
         NI    2(R6),X'F0'                                                      
         OI    2(R6),X'08'                                                      
         GOTO1 ADDREC                                                           
*                                                                               
XR10     MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         NI    KEY+2,X'F0'                                                      
         OI    KEY+2,X'03'                                                      
         GOTO1 HIGH                                                             
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(L'MKGKEY),KEYSAVE                                            
         BNE   XR15                                                             
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO1                                                          
         L     R4,AIO2                                                          
         MVC   0(200,R4),0(R6)                                                  
         MVC   200(100,R4),200(R6)                                              
         NI    2(R4),X'F0'                                                      
         OI    2(R4),X'03'                                                      
         GOTO1 PUTREC                                                           
         B     XR20                                                             
*                                                                               
XR15     L     R6,AIO1                                                          
         NI    2(R6),X'F0'                                                      
         OI    2(R6),X'03'                                                      
         GOTO1 ADDREC                                                           
*                                                                               
XR20     MVC   AIO,AIO1                                                         
*                                                                               
         LA    R3,ADDMRT                                                        
XR25     CLC   0(L'ADDMRT,R3),=X'FFFF'                                          
         BE    XRX                                                              
*                                                                               
         LA    R4,KEY                                                           
         USING MKARECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   MKAKTYP,=X'0D03'                                                 
         MVC   MKAKAGMD,SVBAGYMD                                                
         MVC   MKAKCLT,SVBCLT                                                   
         MVC   MKAKMKT,0(R3)                                                    
***  READ MARKET RECORDS TO CHECK IF MARKET GROUP EXISTALREADY ***              
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(L'MKAKEY),KEYSAVE                                            
         BE    XR30                                                             
*                                                                               
***********  CREATE NEW MARKET RECORD AND IF IT DOESN'T EXISTS *******          
******       THIS WOULD BE THE 0D03 RECORD TYPE                *******          
*                                                                               
         MVC   AIO,AIO2                                                         
         L     R5,AIO                                                           
         XC    0(32,R5),0(R5)                                                   
         MVC   0(L'MKAKEY,R5),KEYSAVE                                           
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING MKAEL05,R6                                                       
         MVI   MKAEL05,X'05'                                                    
         MVI   MKAEL05+1,8                                                      
         MVC   MKAPGRP,PGRPNUM                                                  
         MVC   MKAMGRP(1),QMGR                                                  
         MVC   MKAMGRP+1(2),QMGRP                                               
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'05'                                                     
         GOTO1 ADDELEM                                                          
*                                                                               
         GOTO1 ADDREC                                                           
*                                                                               
         MVC   PASSADDR(4),KEY          SAVE ADDRESS OF NEW MED T REC           
         B     XR135                    GO PROCESS MEDIA C AND N 0D03           
*                                                                               
XR30     MVC   PASSADDR(4),MKAKMKT+3    GET ADDRESS OF EXISTING REC             
*                                                                               
*******************************************************************             
*  IF MARKET RECORD EXISTS, CHECK FOR MARKET ID TO FIND DUPLICATE *             
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         XC    ELEM,ELEM                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BE    XR50                                                             
         L     R6,AIO                                                           
         B     XR110                                                            
*                                                                               
         USING MKAEL05,R6                                                       
* FIRST COMP TO SEE IF SAME PRODUCT GROUP IF NOT NONEED TO                      
* CHECK FOR SAME MKT GROUP NUM, JUST GET NEXT 05 ELEMENT                        
XR50     CLC   MKAPGRP,PGRPNUM                                                  
         BNE   XR60                                                             
*XR50 WAS HERE BEFORE                                                           
* 4/17/03 AKAT BUG FIX FOR 2 CHAR MGROUP.  PREVENT FROM ADDING                  
* TWO X'0D82' RECS FOR SAME MGROUP                                              
*                                                                               
         MVC   HALF(1),MGRMKGN                                                  
         MVI   HALF+1,C' '                                                      
         CLI   MGRMKGN+1,C'Z'      TEST SECOND CHAR IS ALPHA                    
         BH    *+10                NO                                           
         MVC   HALF,MGRMKGN                                                     
*                                                                               
         L     RE,=A(SPMGRTAB)                                                  
         A     RE,RELO                                                          
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
*                                                                               
XR58     CLC   HALF(2),0(RE)                                                    
         BE    XR59                                                             
         LA    RE,3(RE)                                                         
         BCT   RF,XR58                                                          
         B     ERRINV                                                           
*                                                                               
XR59     CLC   2(1,RE),MKAMGRP                                                  
         BNE   XR60                                                             
         BE    XR70                                                             
XR60     BAS   RE,NEXTEL                                                        
         BE    XR50                                                             
         B     XR110                                                            
*                                                                               
XR70     XC    SVPGRP,SVPGRP                                                    
         XC    SVMGRP,SVMGRP                                                    
         MVC   SVPGRP,MKAPGRP            SAVE PREVIOUS VALUES FOR               
         MVC   SVMGRP,MKAMGRP            DELETING PASSIVE KEY                   
*                                                                               
* WE FOUND DUPLICATE MARKET ID SO LETS DELETE OLD VALUES                        
         MVC   MKAPGRP,PGRPNUM                                                  
         MVC   MKAMGRP(1),QMGR                                                  
         MVC   MKAMGRP+1(2),QMGRP                                               
         GOTO1 PUTREC                                                           
         LA    RE,MYMKTIO                                                       
         LHI   RF,L'MYMKTIO                                                     
         XCEFL                                                                  
****     XC    MYMKTIO,MYMKTIO                                                  
         LA    R0,MYMKTIO                                                       
         LHI   R1,L'MYMKTIO                                                     
         L     RE,AIO                                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
****     MVC   MYMKTIO,0(RE)                                                    
* IF CANADIAN SHIT AGAIN CHANGE MEDIA N AND C RECS                              
*                                                                               
*  NOW DELETE THAT PASSIVE POINTER OF THE ORIGINAL NUMBERS                      
XR75     LA    R5,SAVEKEY2                                                      
         XC    SAVEKEY2,SAVEKEY2                                                
         USING MKGKEY,R5                                                        
         MVC   MKGPTYP,=X'0D82'                                                 
         MVC   MKGPAGMD,SVBAGYMD                                                
         MVC   MKGPCLT,SVBCLT                                                   
         MVC   MKGPPID,SVPGRP                                                   
         MVC   MKGPPGRP,SVPGRP+1                                                
         MVC   MKGPMID,SVMGRP                                                   
         MVC   MKGPMGRP,SVMGRP+1                                                
         MVC   MKGPMKT,MKAKMKT                                                  
         MVC   KEY(L'SAVEKEY2),SAVEKEY2                                         
         OI    DMINBTS,X'88'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         NI    DMINBTS,X'FF'-X'88'                                              
*     IS THERE AN EXISTING PASSIVE KEY?                                         
XR80     CLC   KEY(L'SAVEKEY2),KEYSAVE                                          
         BNE   XR85                                                             
         CLI   DMCB+8,X'02'          IS OLD PASSIVE POINTER DELETED             
         BE    XR85                  IF ALREADY DELETED CHK OTHER KEYS          
*                                    PASSIVE KEY                                
         LA    R5,KEY                DELETE OLD PASSIVE KEY                     
         OI    MKGKEY+13,X'80'       MARKED FOR DELETION                        
         GOTO1 WRITE                                                            
*                                                                               
XR85     CLI   SVAPROF+7,C'C'                                                   
         BNE   XR130                                                            
         CLI   QMED,C'T'                                                        
         BNE   XR135                                                            
****------- DELETE NETWORK, AND COMBINED MEDIA PASSIVE KEYS----*                
         NI    KEY+2,X'F0'                                                      
         OI    KEY+2,X'08'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'MKGKEY),KEYSAVE                                            
         BNE   XR90                                                             
         CLI   DMCB+8,X'02'          IS OLD PASSIVE POINTER DELETED             
         BE    XR90                  IF ALREADY DELETED JUST OKAY               
         OI    MKGKEY+13,X'80'                                                  
         GOTO1 WRITE                                                            
*                                                                               
XR90     NI    KEY+2,X'F0'                                                      
         OI    KEY+2,X'03'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'MKGKEY),KEYSAVE                                            
         BNE   XR130                                                            
         CLI   DMCB+8,X'02'          IS OLD PASSIVE POINTER DELETED             
         BE    XR130                 IF ALREADY DELETED JUST OKAY               
         OI    MKGKEY+13,X'80'                                                  
         GOTO1 WRITE                                                            
*                                                                               
*   IF MARKET GROUP ALREADY EXISTS I SWITCH THE MARKET AND PROD NUMBERS         
         B     XR130                                                            
         DROP  R5,R6                                                            
*                                                                               
* WE DIDN'T FIND THE MARKET ID AS A DUPLICATE IN THE MARKET RECORD              
*SO ADD NEW ELEMENT WITH MARKET GROUP NUMBER AND PRODUCT GROUP NUMBER           
*                                                                               
XR110    LA    R5,ELEM                                                          
         USING MKAEL05,R5                                                       
         MVI   MKAEL05,X'05'                                                    
         MVI   MKAEL05+1,8                                                      
         MVC   MKAPGRP,PGRPNUM                                                  
         MVC   MKAMGRP(1),QMGR                                                  
         MVC   MKAMGRP+1(2),QMGRP                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         GOTO1 PUTREC                                                           
         LA    R0,MYMKTIO                                                       
         LHI   R1,L'MYMKTIO                                                     
         L     RE,AIO                                                           
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
****     MVC   MYMKTIO,0(RE)                                                    
         B     XR135                                                            
         DROP  R4,R5                                                            
******************************************************************              
* CREATE NEW 0D03 OR CHANGE 0D03 TO BE SAME AS 0D03 MEDIA T                     
******************************************************************              
XR130    CLI   SVAPROF+7,C'C'                                                   
         BNE   XR140                                                            
         CLI   QMED,C'T'                                                        
         BNE   XR140                                                            
*  REREAD 0D03 MARKET ASSIGN REC SINCE IT MIGHT HAVE BEEN BLOWN BY 0D82         
         LA    R4,KEY                                                           
         USING MKARECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   MKAKTYP,=X'0D03'                                                 
         MVC   MKAKAGMD,SVBAGYMD                                                
         MVC   MKAKCLT,SVBCLT                                                   
         MVC   MKAKMKT,0(R3)                                                    
***  READ MARKET RECORDS TO CHECK IF MARKET GROUP EXISTALREADY ***              
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
* THIS PART WILL READ FOR 0D03 MEDIA C AND MEDIA N TO DETERMINE IF              
* IT EXIST.  IT SO PUTREC, IF NOT ADDREC                                        
*                                                                               
XR135    CLI   SVAPROF+7,C'C'                                                   
         BNE   XR140                                                            
         CLI   QMED,C'T'                                                        
         BNE   XR140                                                            
         MVC   AIO,AIO3                                                         
         L     R5,AIO2                                                          
         L     R6,AIO3                                                          
         MVC   0(200,R6),0(R5)                                                  
         MVC   200(100,R6),200(R5)                                              
         LA    R4,KEY                                                           
         USING MKARECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'MKAKEY),0(R5)                                              
         NI    KEY+8,X'F0'                                                      
         OI    KEY+8,X'03'                                                      
         NI    8(R6),X'F0'                                                      
         OI    8(R6),X'03'                                                      
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(L'MKAKEY),KEYSAVE                                            
         BNE   XR135CA                                                          
         MVC   PASSADDC,MKAKMKT+3                                               
         GOTO1 GETREC                                                           
*        MVC   0(200,R6),0(R5)      HAVE TO MKTIO FOR SOME REASON ELEM          
         MVC   0(200,R6),MYMKTIO    WASN'T SET FOR 0D03 T REC YET               
         MVC   200(100,R6),MYMKTIO+200                                          
         NI    8(R6),X'F0'                                                      
         OI    8(R6),X'03'                                                      
         GOTO1 PUTREC                                                           
         B     XR135N                                                           
XR135CA  GOTO1 ADDREC                                                           
         MVC   PASSADDC(4),KEY                                                  
XR135N   DS    0X           N MEDIA 0D03 REC                                    
         L     R6,AIO3                                                          
         MVC   AIO,AIO3                                                         
*        MVC   0(200,R6),0(R5)                                                  
         XC    KEY,KEY                                                          
*        MVC   KEY(L'MKAKEY),0(R5)                                              
         MVC   KEY(L'MKAKEY),0(R6)                                              
         NI    KEY+8,X'F0'                                                      
         OI    KEY+8,X'08'                                                      
         NI    8(R6),X'F0'                                                      
         OI    8(R6),X'08'                                                      
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(L'MKAKEY),KEYSAVE                                            
         BNE   XR135NA                                                          
         MVC   PASSADDN,MKAKMKT+3                                               
         GOTO1 GETREC                                                           
*        MVC   0(200,R6),0(R5)                                                  
         MVC   0(200,R6),MYMKTIO                                                
         MVC   200(100,R6),MYMKTIO+200                                          
         NI    8(R6),X'F0'                                                      
         OI    8(R6),X'08'                                                      
         GOTO1 PUTREC                                                           
         B     XR135XX                                                          
XR135NA  GOTO1 ADDREC                                                           
         MVC   PASSADDN(4),KEY                                                  
*        DROP  R4                                                               
XR135XX  DS    0X                                                               
         MVC   AIO,AIO1                                                         
******************************************************************              
*     BUILD  THE NEW PASSIVE KEY                                                
XR140    LA    R4,KEY                                                           
         USING MKGKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVC   MKGPTYP,=X'0D82'                                                 
         MVC   MKGPAGMD,SVBAGYMD                                                
         MVC   MKGPCLT,SVBCLT                                                   
         MVC   MKGPPID,PGRPID                                                   
         MVC   MKGPPGRP,PGRPNM                                                  
         MVC   MKGPMID,QMGR                                                     
         MVC   MKGPMGRP,QMGRP                                                   
         MVC   MKGPMKT,0(R3)                                                    
         XC    SAVEKEY2,SAVEKEY2                                                
         MVC   SAVEKEY2,KEY                                                     
         OI    DMINBTS,X'88'                                                    
*                                                                               
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'88'                                              
         CLC   KEY(13),KEYSAVE                                                  
         BNE   XR160                 DOES ALREADY EXISTS A DELETED KEY          
* FOUND PASSIVE KEY                                                             
         NI    KEY+13,X'FF'-X'80'    TURN OFF DELETEION BIT                     
         GOTO1 WRITE                                                            
*                                                                               
         CLI   SVAPROF+7,C'C'                                                   
         BNE   XR500                                                            
         CLI   QMED,C'T'                                                        
         BNE   XR500                                                            
*                                                                               
         NI    KEY+2,X'F0'                                                      
         OI    KEY+2,X'03'                                                      
         OI    DMINBTS,X'88'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'MKGKEY),KEYSAVE                                            
         BNE   XR150                                                            
         NI    KEY+13,X'FF'-X'80'    TURN OFF DELETEION BIT                     
         GOTO1 WRITE                                                            
*                                                                               
XR150    NI    KEY+2,X'F0'                                                      
         OI    KEY+2,X'08'                                                      
         OI    DMINBTS,X'88'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'MKGKEY),KEYSAVE                                            
         BNE   XR500                                                            
         NI    KEY+13,X'FF'-X'80'    TURN OFF DELETEION BIT                     
         GOTO1 WRITE                                                            
         NI    DMINBTS,X'FF'-X'88'                                              
*                                                                               
         B     XR500                                                            
*                                                                               
XR160    XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY2),SAVEKEY2       ADD NEW PASSIVE KEY               
         MVC   KEY+14(4),PASSADDR                                               
         MVI   DMOUTBTS,0                                                       
         GOTO1 ADD                            ADD MEDIA T PASSIVE               
         TM    DMOUTBTS,X'20'                                                   
         BO    DUPERR                                                           
*                                                                               
*   IF NOT CANADIAN T SKIP                                                      
         CLI   SVAPROF+7,C'C'                                                   
         BNE   XR500                                                            
         CLI   QMED,C'T'                                                        
         BNE   XR500                                                            
*                                                                               
         NI    KEY+2,X'F0'                                                      
         OI    KEY+2,X'08'                                                      
         MVC   KEY+14(4),PASSADDN                                               
         MVI   DMOUTBTS,0                                                       
         GOTO1 ADD                            ADD MEDIA COMBINE PASS            
         TM    DMOUTBTS,X'20'                                                   
         BO    DUPERR                                                           
         NI    KEY+2,X'F0'                                                      
         OI    KEY+2,X'03'                                                      
         MVC   KEY+14(4),PASSADDC                                               
         MVI   DMOUTBTS,0                                                       
         GOTO1 ADD                            ADD MEDIA NETWORK PASS            
         TM    DMOUTBTS,X'20'                                                   
         BO    DUPERR                                                           
*                                                                               
         DROP  R4                                                               
XR500    LA    R3,2(R3)                                                         
         B     XR25                                                             
XRX      NI    DMINBTS,X'FF'-X'88'                                              
         MVC   AIO,AIO1                                                         
*                                                                               
         B      DR                                                              
****************************************************************                
*                    DISPLAY KEY                                                
****************************************************************                
DK       L      R5,AIO                                                          
         USING  MKGRECD,R5                                                      
*                                                                               
         CLI    THISLSEL,C'C'                                                   
         BNE    DK10                                                            
         CLI    T217FFD+1,C'*'      TEST DDS TERM                               
         BE     DK10                                                            
         TM     T217FFD+12,X'01'                                                
         BO     ERRSEC2             ON = NO CHANGE                              
*                                                                               
DK10     LAY    R6,MEDTAB                                                       
         MVC    MGRMED,MKGKAGMD               MOVE MEDIA TO SCREEN              
         NI     MGRMED,X'0F'                                                    
*                                                                               
DK15     CLC    MGRMED,1(R6)                                                    
         BE     *+12                                                            
         LA     R6,MTABLQ(R6)                                                   
         B      DK15                                                            
         MVC    MGRMED,0(R6)                                                    
         OI     MGRMEDH+6,X'80'                                                 
         MVI    MGRMEDH+5,1                                                     
         LA     R2,MGRMEDH                                                      
         MVI    USEIONUM,2                                                      
*                                                                               
         GOTO1  VALIMED                                                         
         MVC    MGRMDN,MEDNM               MOVE MEDIA NAME TO SCREEN            
         OI     MGRMDNH+6,X'80'                                                 
         MVC    AIO,AIO1                                                        
*                                                                               
         CLI    SVAPROF+7,C'C'                                                  
         BNE    DK30                                                            
         CLI    MGRMED,C'N'                                                     
         BE     *+12                                                            
         CLI    MGRMED,C'C'                                                     
         BNE    DK30                                                            
*                                                                               
         CLI    THISLSEL,C'A'                                                   
         BE     ERRINV                                                          
*                                                                               
         CLI    THISLSEL,C'C'                                                   
         BE     ERRINV                                                          
*                                                                               
DK30     MVC    QMGR,MKGKMID               SAVE KEY FIELDS FOR DISPREC          
         MVC    QMGRP,MKGKMGRP                                                  
         MVC    SVBAGYMD,MKGKAGMD                                               
*                                                                               
         OC     MKGKCLT,MKGKCLT            =X'0000' CLIENT=ALL                  
         BNZ    DK50                                                            
*                                          IF CLIENT =ALL CLEAR                 
         XC     SVBCLT,SVBCLT              CLEAR CLIENT FIELD                   
         MVC    MGRCLT,=C'ALL'                                                  
         OI     MGRCLTH+6,X'80'                                                 
*                                                                               
         XC     MGRCLN,MGRCLN              CLEAR CLIENT NAME FIELD              
         OI     MGRCLNH+6,X'80'                                                 
         B      DK60                                                            
*                                                                               
DK50     GOTO1  CLUNPK,DMCB,MKGKCLT,CLIENT    CLIENT IS NOT "ALL"               
         MVC    SVBCLT,MKGKCLT                SAVE KEY FIELD FOR DISREC         
         MVC    MGRCLT,CLIENT                 MOVE TO SCREEN                    
         OI     MGRCLTH+6,X'80'                                                 
         MVI    MGRCLTH+5,3                   STICK IN THE LENGTH               
         LA     R2,MGRCLTH                                                      
         MVI    USEIONUM,2                                                      
         GOTO1  VALICLT                                                         
*                                                                               
         GOTO1  CLUNPK,DMCB,(SVCPROF+6,MKGKCLT),CLIENT                          
         MVC    MGRCLT,CLIENT                 MOVE TO SCREEN                    
         MVC    MGRCLN,CLTNM                  MOVE IN CLIENT NAME               
         OI     MGRCLNH+6,X'80'                                                 
         MVC    AIO,AIO1                                                        
*                                                                               
DK60     XC     MGRMKGN,MGRMKGN            MOVE IN MGRPID GET                   
         L      RE,=A(SPMGRTAB)                                                 
         A      RE,RELO                                                         
         LHI    RF,(SPMGRTBX-SPMGRTAB)/3                                        
*                                                                               
DK62     CLC    2(1,RE),MKGKMID                                                 
         BE     DK64                                                            
         LA     RE,3(RE)                                                        
         BCT    RF,DK62                                                         
         DC     H'0'                                                            
*                                                                               
DK64     MVC    MGRMKGN(2),0(RE)           PACK NUMBER LATER                    
*                                                                               
         OC     MKGKPRD,MKGKPRD            IF PRODUCT GROUP =ALL                
         BNZ    DK70                                                            
*                                                                               
         MVC    MGRPGP(3),=C'ALL'                                               
         XC     PGRPNUM,PGRPNUM                                                 
         B      DK75                                                            
*                                                                               
DK70     MVC    MGRPGP(1),MKGKPID          MOVE IN PRODUCT ID FIRST             
         MVC    PGRPID,MKGKPID             SAVE KEY FIELDS                      
         MVC    PGRPNM,MKGKPGRP            SAVE KEY FIELDS                      
*                                                                               
DK75     OI     MGRPGPH+6,X'80'                                                 
*                                                                               
* ***********  GET BREAK LENGTH **********************                          
         XC     KEY,KEY                                                         
         MVC    KEY,0(R5)        BUILD KEY WITH PREVIOUS KEY                    
         XC     KEY+6(2),KEY+6   PRODUCT GROUP AS NULLS                         
         XC     KEY+8(3),KEY+8   CLEAR MARKET GROUP                             
*                                                                               
***   PRODUCT DEFINITION RECORD FOR BREAK LENGTHS ****                          
*                                                                               
         CLC    MGRPGP(3),=C'ALL'                                               
         BE     DK80                                                            
*                                                                               
* IF ONLY PRODUCT GROUP ID                                                      
         OC     MKGKPRD+1(2),MKGKPRD+1                                          
         BZ     DK80                                                            
*                                                                               
         MVC    KEY(2),=X'0D01'  PRODUCT DEFINTION RECORD                       
*                                                                               
         GOTO1  HIGH                                                            
         MVC    AIO,AIO2                                                        
         GOTO1  GETREC                                                          
         CLI    DMCB+8,0                                                        
         BE     *+6                                                             
         DC     H'0'                                                            
*                                                                               
         L      R6,AIO                                                          
         MVI    ELCODE,X'01'                                                    
         BAS    RE,GETEL                                                        
         BE     *+6                                                             
         DC     H'0'                                                            
*                                                                               
         USING  PRGEL01,R6                                                      
         LLC    R4,PRGBK1LN                                                     
         LLC    R0,PRGBK2LN                                                     
         AR     R4,R0                                                           
         LLC    R0,PRGBK3LN                                                     
         AR     R4,R0                                                           
         UNPK   DUB,PGRPNM(3)                                                   
         BCTR   R4,0                                                            
         EX     R4,*+8                                                          
         B      *+10                                                            
         MVC    MGRPGP+1(0),DUB+3                                               
         OI     MGRPGPH+6,X'80'                                                 
*                                                                               
***   MARKET DEFINITION RECORD FOR BREAK LENGTHS ****                           
*                                                                               
DK80     CLI    MKGKMID,C'G'                                                    
         BL     *+10                                                            
         XC     KEY+3(2),KEY+3   IF G OR GREATER CLIENT EXCEPTION               
*                                                                               
         MVC    KEY(2),=X'0D02'               MOVE IN RECORD TYPE               
         XC     KEY+5(1),KEY+5                CLEAR THE PRODUCT ID              
         MVC    KEY+8(1),8(R5)                MOVE IN MGRPID                    
         GOTO1  HIGH                                                            
         CLI    DMCB+8,0                                                        
         BE     *+6                                                             
         DC     H'0'                                                            
*                                                                               
         MVC    AIO,AIO2                                                        
         GOTO1  GETREC                                                          
         CLI    DMCB+8,0                                                        
         BE     *+6                                                             
         DC     H'0'                                                            
*                                                                               
         L      R6,AIO                                                          
         MVI    ELCODE,X'01'                                                    
         BAS    RE,GETEL                                                        
         BE     *+6                                                             
         DC     H'0'                                                            
*                                                                               
         USING  MKGEL01,R6                                                      
*                                                                               
         MVC   MGRBK1,MKGBK1                                                    
         OI    MGRBK1H+6,X'80'                                                  
         MVC   MGRBK2,MKGBK2                                                    
         OI    MGRBK2H+6,X'80'                                                  
         MVC   MGRBK3,MKGBK3                                                    
         OI    MGRBK3H+6,X'80'                                                  
*                                                                               
         LHI    RF,1                                                            
         LLC    R0,MKGBK1LN                                                     
         LLC    R4,MKGBK2LN                                                     
         AR     R0,R4                                                           
         LTR    R4,R4                                                           
         BZ     DK100                                                           
         AHI    RF,1                                                            
*                                                                               
         LLC    R4,MKGBK3LN                                                     
         AR     R0,R4                                                           
         LTR    R4,R4                                                           
         BZ     DK100                                                           
         AHI    RF,1                                                            
*                                                                               
DK100    STC    RF,BKNUM           RF=NUM BREAKS/R0=TOTAL LEN                   
*                                                                               
         LA     RF,MGRMKGN+1       POINT TO SECOND CHAR OF MGRPID               
         CLI    0(RF),C' '         TEST ANYTHING THERE                          
         BH     *+6                                                             
         BCTR   RF,0                                                            
*                                                                               
         UNPK   DUB,QMGRP(3)       UNPACK GROUP CODE                            
         LR     RE,R0              GET LENGTH OF GROUP                          
         BCTR   RE,0                                                            
         EX     RE,*+8                                                          
         B      *+10                                                            
         MVC    1(0,RF),DUB+3                                                   
         OI     MGRMKGNH+6,X'80'                                                
         DROP   R5                                                              
*                                                                               
DKX      MVC   AIO,AIO1                                                         
*                                                                               
         LA    R4,KEY               BUILD MARKET GROUP KEY                      
         USING MKGRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   MKGKTYP,=X'0D02'                                                 
         MVC   MKGKAGMD,SVBAGYMD                                                
         MVC   MKGKCLT,SVBCLT           BINARY CLIENT CODE                      
         MVC   MKGKPID,PGRPID                                                   
         MVC   MKGKPGRP,PGRPNM                                                  
         MVC   MKGKMID,QMGR                                                     
******   MVC   MKGKMGRP,MGRPNO                                                  
         MVC   MKGKMGRP,QMGRP           I DON'T SEE WHO SET MGRPNO!!            
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY,KEY              SAVE THE KEY                            
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
****************************************************************                
*                 LIST REC                                                      
*****************************************************************               
LR       MVI    FIRSTLST,0                                                      
         OC     KEY,KEY                  FIRST TIME THROUGH?                    
         BZ     LR01                     YES                                    
         CLC    KEY(13),KEYSAVE          JUST RETURNED FROM LIST?               
         BNE    LR30                     NOPE, OTHERWISE KEY=KEYSAVE            
         XC     KEY,KEY                                                         
         MVC    KEY(13),FIRSTKEY         LAST SCREEN'S FIRST KEY                
         MVC    MGRFILTR,KEY+8           DONT GET FILTERED OUT!                 
         MVC    LASTMED,KEY+2            DONT GET FILTERED OUT!                 
         B      LR10                     AND READ HIGH                          
*                                                                               
**********   RESTORE ORIGINAL KEY FROM VALKEY  ***********                      
LR01     MVI    LASTMED,0                CLEAR LAST MEDIA                       
         BRAS   RE,FRSTMGRP              FIND FIRST MGROUP TO LOOK FOR          
         MVC    KEY(L'SAVEKEY),SAVEKEY                                          
         MVC    KEY+8(1),MGRFILTR        START FROM THIS MGROUP                 
         XC     KEY+9(2),KEY+9           START FROM LOWEST MGROUP NUM           
         B      LR10                                                            
*                                                                               
LR08     CLI    KEYSAVE+8,C'Z'           LAST ONE A C'Z'?                       
         BNE    LR09                     NOPE                                   
         BRAS   RE,FRSTMGRP              FIND FIRST MGROUP TO LOOK FOR          
         MVC    KEY+8(1),MGRFILTR        START FROM THIS MGROUP                 
         XC     KEY+9(2),KEY+9           START FROM LOWEST MGROUP NUM           
         B      LR10                                                            
*                                                                               
LR08A    CLI    KEYSAVE+8,C'Z'           LAST ONE A C'Z'?                       
         BNE    LR09                     NOPE                                   
*                                                                               
         MVC    KEY,KEYSAVE              TESTED ALL POSSIBILITIES               
         MVC    KEY+8(3),=X'FFFFFF'      FIND NEXT CLT/PRDGROUP                 
         GOTO1  HIGH                     CAUSE THIS ONES ALL WASHED UP          
         CLC    KEY(2),KEYSAVE           DID WE GET MGROUP REC?                 
         BNE    LRX                      NOPE, DONE                             
         CLC    AGYHIGH,KEY+2            SAME AGY/MAX MED?                      
         BL     LRX                      NOPE, DONE                             
LR08C    BRAS   RE,FRSTMGRP              FIND FIRST MGROUP TO LOOK FOR          
         B      LR09B                                                           
*                                                                               
LR09     LA    R1,MGRSORT                                                       
*                                                                               
LR09A    CLC    KEYSAVE+8(1),2(R1)       LAST MGROUP WE LOOKED FOR?             
         LA     R1,3(R1)                 NEXT ENTRY                             
         BNE    LR09A                    NO                                     
*                                                                               
         MVC    KEY,KEYSAVE              REREAD KEY WE WERE LOOKING FOR         
         MVC    MGRFILTR,2(R1)                                                  
LR09B    MVC    KEY+8(1),MGRFILTR        FIND KEY WITH THIS MGROUP              
         XC     KEY+9(2),KEY+9           START FROM LOWEST MGROUP NUM           
*                                                                               
LR10     GOTO1  HIGH                                                            
         B     LR30                                                             
*                                                                               
LR20     GOTO1  SEQ                                                             
*                                                                               
LR30     CLC    KEY(3),KEYSAVE     MGROUP RECORD/A-M?                           
         BE     LR31               YES                                          
***                                                                             
* BEFORE WE EXIT WE MUST BE SURE THAT WE CHECKED ALL POSSIBILITIES              
* OF MGROUPS FOR THE PREV CLT/PRDGRP.  IF WE DID A HIGH ON MGROUP               
* C'Z' THEN THAT IS THE LAST ONE (ALPHABETICALLY) THAT WE CAN FIND              
***                                                                             
         CLI    KEYSAVE+8,C'Z'     CHECK ALL POSSIBILITIES OF PREV MGRP         
         BNE    LR09               NO, THERE MIGHT BE MORE                      
         B      LRX                OK, YOU CAN EXIT NOW                         
*                                                                               
LR31     CLC    AGYHIGH,KEY+2      DID WE PASS THIS AGY/MAX MED?                
         BNL    LR31AA             NOT YET                                      
         CLI    KEYSAVE+8,C'Z'     CHECK ALL POSSIBILITIES OF PREV MGRP         
         BNE    LR09               NO, THERE MIGHT BE MORE                      
         B      LRX                OK, YOU CAN EXIT NOW                         
*                                                                               
LR31AA   CLI    LASTMED,0          FIRST TIME THROUGH?                          
         BNE    LR31A              NO                                           
         MVC    LASTMED,KEY+2      SAVE LAST MEDIA                              
         B      LR31B              YES                                          
*                                                                               
LR31A    CLC    LASTMED,KEY+2       MEDIA MATCH?                                
         BE     LR31B               YES                                         
         MVC    LASTMED,KEY+2       SAVE LAST MEDIA                             
         B      LR08C               RE-READ KEY WITH FIRST MGROUP               
*                                                                               
LR31B    CLI    MGRCLTH+5,0         CLIENT ENTERED?                             
         BE     LR32                NOPE                                        
         CLC    =C'ALL',MGRCLT      CLIENT 'ALL'?                               
         BE     LR32                YES                                         
         CLC    SVBCLT,KEY+3        FILTER ON CLIENT                            
         BE     LR32                MATCH                                       
         CLI    KEYSAVE+8,C'Z'      DID WE TRY EVERY MARKET GROUP?              
         BNE    LR09                NOPE                                        
         B      LRX                                                             
*                                                                               
LR32     CLC    KEY(8),KEYSAVE     SAME A/M CLT PRDGRP?                         
         BNE    LR08               NOPE                                         
         CLC    KEY+8(1),MGRFILTR  MGROUP WE WANT?                              
         BNE    LR08A              NOPE                                         
*                                                                               
LR33     XC     SAVEKEY2,SAVEKEY2                                               
         MVC    SAVEKEY2,KEY                                                    
*                                                                               
         XC     KEY+6(2),KEY+6                                                  
         XC     KEY+8(3),KEY+8                                                  
*                                                                               
********************** PRODUCT DEFINTION RECORD ***************                 
*                                                                               
         OC     SAVEKEY2+5(3),SAVEKEY2+5  IF PGRP=ALL SKIP BREAK                
         BZ     LR70                                                            
* IF JUST PRDGROUP ID SKIP BREAK                                                
         OC     SAVEKEY2+6(2),SAVEKEY2+6                                        
         BZ     LR70                                                            
         CLC    PDEFKEY,KEY                                                     
         BE     LR70                                                            
*                                                                               
         MVC    KEY(2),=X'0D01'                                                 
*                                                                               
LR40     GOTO1  HIGH                                                            
         CLC    KEY(13),KEYSAVE                                                 
         BE     LR50                                                            
         XC     KEY,KEY                                                         
         MVC    KEY(L'SAVEKEY2),SAVEKEY2                                        
         GOTO1  HIGH                                                            
         B      LR20                                                            
*                                                                               
LR50     MVC    AIO,AIO2                                                        
         CLC    KEY(9),KEYSAVE                                                  
         BE     *+6                                                             
         DC     H'0'                                                            
         GOTO1  GETREC                                                          
         CLI    DMCB+8,0                                                        
         BE     *+6                                                             
         DC     H'0'                                                            
*                                                                               
         L      R6,AIO                                                          
         USING  PRGEL01,R6                                                      
         MVI    ELCODE,X'01'                                                    
         BAS    RE,GETEL                                                        
         BE     *+8                                                             
         B      LR20                                                            
*                                                                               
         LLC    RE,PRGBK1LN                                                     
         LLC    R0,PRGBK2LN                                                     
         AR     RE,R0                                                           
         LLC    R0,PRGBK3LN                                                     
         AR     RE,R0                                                           
         XC     PBRKLEN,PBRKLEN                                                 
         STC    RE,PBRKLEN                                                      
         XC     PDEFKEY,PDEFKEY                                                 
         MVC    PDEFKEY,KEY                                                     
*                                                                               
********************** MARKET DEFINTION RECORD ***************                  
*                                                                               
LR70     XC     KEY,KEY                                                         
         MVC    KEY(L'SAVEKEY2),SAVEKEY2                                        
         CLI    KEY+8,C'G'                                                      
         BL     *+10                                                            
         XC     KEY+3(2),KEY+3            CLIENT EXCEPTION                      
         XC     KEY+5(3),KEY+5            PRDG NUM                              
         XC     KEY+9(2),KEY+9                                                  
* LR70                                                                          
         MVC    KEY(2),=X'0D02'                                                 
         XC     KEY+5(1),KEY+5       CLEAR PRODUCT GROUP ID                     
         MVC    KEY+8(1),SAVEKEY2+8   RESTORE MRAKET ID                         
         CLC    MDEFKEY,KEY                                                     
         BE     LR80                                                            
*                                                                               
         GOTO1  HIGH                                                            
         CLI    DMCB+8,0                                                        
         BE     *+6                                                             
         DC     H'0'                                                            
*                                                                               
*        CLC    KEY(6),KEYSAVE                                                  
         CLC    KEY(13),KEYSAVE                                                 
         BE     LR75                                                            
         XC     KEY,KEY                                                         
         MVC    KEY(L'SAVEKEY2),SAVEKEY2                                        
         GOTO1  HIGH                                                            
         CLI    DMCB+8,0                                                        
         BE     *+6                                                             
         DC     H'0'                                                            
         B      LR20                                                            
*                                                                               
LR75     MVC    AIO,AIO2                                                        
*                                                                               
         GOTO1  GETREC                                                          
         CLI    DMCB+8,0                                                        
         BE     *+6                                                             
         DC     H'0'                                                            
*                                                                               
         L      R6,AIO                                                          
         USING  MKGEL01,R6                                                      
         MVI    ELCODE,X'01'                                                    
         BAS    RE,GETEL                                                        
         BE     *+8                                                             
         B      LR20                                                            
*                                                                               
         LLC    RE,MKGBK1LN                                                     
         LLC    R0,MKGBK2LN                                                     
         AR     RE,R0                                                           
         LLC    R0,MKGBK3LN                                                     
         AR     RE,R0                                                           
         XC     MBRKLEN,MBRKLEN                                                 
         STC    RE,MBRKLEN                                                      
         XC     MDEFKEY,MDEFKEY                                                 
         MVC    MDEFKEY,KEY                                                     
*                                                                               
********* RESTORE ORIGINAL KEY AND READ  ********                               
*                                                                               
LR80     XC     KEY,KEY                                                         
         MVC    KEY(L'SAVEKEY2),SAVEKEY2                                        
         GOTO1  HIGH                                                            
         CLI    DMCB+8,0                                                        
         BE     *+6                                                             
         DC     H'0'                                                            
*                                                                               
         CLC    KEY(13),KEYSAVE                                                 
         BE     *+6                                                             
         DC     H'0'                                                            
*                                                                               
LR100    MVC    AIO,AIO1                                                        
         GOTO1  GETREC                                                          
         CLI    DMCB+8,0                                                        
         BE     *+6                                                             
         DC     H'0'                                                            
*                                                                               
         L      R4,AIO                                                          
         USING  MKGRECD,R4                                                      
*                                                                               
         MVC    LISTAR,SPACES                                                   
*                                                                               
**** SKIP MARKET DEF RECORDS                                                    
*                                                                               
         CLC    MKGKMGRP,=X'0000'                                               
         BE     LR20                                                            
*                                                                               
         MVC    LSMED,MKGKAGMD                                                  
         NI     LSMED,X'0F'                                                     
         LAY    R6,MEDTAB                                                       
*   GET MEDIA NAME  , AND PUT IT INTO LIST                                      
LR110    CLC    LSMED,1(R6)                                                     
         BE     *+12                                                            
         LA     R6,MTABLQ(R6)                                                   
         B      LR110                                                           
         MVC    LSMED,0(R6)                                                     
*                                                                               
         OC     MKGKCLT,MKGKCLT                                                 
         BNZ    LR130                                                           
         XC     LSCLT,LSCLT                                                     
         MVC    LSCLT,=C'ALL'                                                   
*                                                                               
         OC     LISCLT,LISCLT                                                   
         BZ     LR160                                                           
*  CHECK TO SEE IF FILTER IS WHAT I WANT IN THE LIST                            
         LLC    RE,LISCLTH+5                                                    
         BCTR   RE,0                                                            
         EX     RE,*+8                                                          
         B      *+10                                                            
         CLC    LISCLT(0),LSCLT                                                 
         BNE    LR20                                                            
         B      LR160                                                           
*                                                                               
LR130    CLC    MKGKCLT,SVBCLI      SAME AS PREVIOUS CLIENT?                    
         BE     LR132                                                           
         BRAS   RE,AAN                                                          
         MVC    SVBCLI,MKGKCLT                                                  
*                                                                               
LR132    GOTO1  CLUNPK,DMCB,(CLTAAN,MKGKCLT),CLIENT                             
*                                                                               
         XC     LSCLT,LSCLT                                                     
         MVC    LSCLT,CLIENT                                                    
         OC     LISCLT,LISCLT                                                   
         BZ     LR160                                                           
*  CHECK TO SEE IF FILTER IS WHAT I WANT IN THE LIST                            
         CLC    MKGKCLT,SVBCLT                                                  
         BE     LR160                                                           
***                                                                             
* PLEASE DON'T DO AN EXTRA 5,000 I/O'S                                          
* THE CLIENT DOES NOT MATCH THE FILTER, SO JUST CHECK THE NEXT MEDIA!           
***                                                                             
         MVC    BYTE,KEY+2       GET A/M                                        
         NI     BYTE,X'0F'       GET RID OF AGY                                 
         LLC    R1,BYTE          MEDIA CODE                                     
         AHI    R1,1             NEXT POSSIBLE MEDIA CODE                       
         STC    R1,BYTE          NEXT MEDIA IN BYTE                             
         NI     KEY+2,X'F0'      GET RID OF MEDIA IN KEY                        
         OC     KEY+2(1),BYTE    PUT NEW MEDIA INTO KEY                         
         MVC    KEY+3(2),SVBCLT  WERE FILTERING ON THIS CLT AFTER ALL!          
         XC     KEY+5(8),KEY+5   CLEAR THE REST                                 
         BRAS   RE,FRSTMGRP      FIND FIRST MGROUP TO LOOK FOR                  
         MVC    KEY+8(1),MGRFILTR     FIND KEY WITH THIS MGROUP                 
         GOTO1  HIGH                                                            
         CLC    KEY(2),KEYSAVE   HAVE MGROUP RECORD?                            
         BNE    LRX              NOPE, DONE                                     
***                                                                             
* NOW MAKE SURE WE HAVE THE SAME AGENCY!                                        
***                                                                             
         MVC    HALF(1),KEY+2                                                   
         MVC    HALF+1(1),KEYSAVE+2                                             
         NI     HALF,X'F0'       ISOLATE THIS AGENCY                            
         NI     HALF+1,X'F0'     ISOLATE LAST AGENCY                            
         CLC    HALF(1),HALF+1   SAME AGENCY AS WE READ FOR                     
         BNE    LRX              NOPE, YOU AINT IN KANSAS NO MORE!              
         B      LR10             AND READ HIGH FOR IT                           
*        BNE    LR20                                                            
*                                                                               
************ PRODUCT GROUP *****************                                    
LR160    OC     MKGKPRD,MKGKPRD                                                 
         BNZ    LR170                                                           
         XC     LSPGRP,LSPGRP                                                   
         MVC    LSPGRP,=C'ALL'                                                  
         B      LR180                                                           
*                                                                               
*   IF PRODUCT GROUP NOT = TO "ALL"                                             
LR170    XC     LSPGRP,LSPGRP                                                   
         MVC    LSPGRPID,MKGKPID                                                
*IF JUST PGRPID THAN JUST PRINT ID                                              
         OC     MKGKPRD+1(2),MKGKPRD+1                                          
         BZ     LR180                                                           
*                                                                               
         XC     FULL,FULL                                                       
         GOTO1  HEXOUT,DMCB,MKGKPRD+1,FULL,2,=C'TOG',0                          
         LLC    RE,PBRKLEN                                                      
         BCTR   RE,0                                                            
         EX     RE,*+8                                                          
         B      *+10                                                            
         MVC    LSPGRPNM(0),FULL                                                
*   DISPLAY THE GOURP NUMBER BY BREAK LENGTH IN PRODUCT DEF RECORD              
         OC     LISPGP,LISPGP                                                   
         BZ     LR180                                                           
         CLC    MKGKPID,LISPGP                                                  
         BL     LR20                                                            
         BH     LR180          IF ID IS HIGHER NO NEED TO CHECK NUM             
         MVC    WORK(4),=C'0000'                                                
         EX     RE,*+8                                                          
         B      *+10                                                            
*   THIS PART USES PRODUCT AS A START AT FIELD                                  
         MVC    WORK(0),LISPGP+1    MARKET NUMBER IS PACKED BINARY              
         XC     DUB,DUB                                                         
         PACK   DUB(3),WORK(5)                                                  
         MVC    HALF,DUB                                                        
         CLC    HALF,MKGKPGRP                                                   
         BH     LR20                                                            
*   MOVE MARKET GROUP TO LIST                                                   
LR180    L     RE,=A(SPMGRTAB)                                                  
         A     RE,RELO                                                          
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
*                                                                               
LR182    CLC   2(1,RE),MKGKMID                                                  
         BE    LR184                                                            
         LA    RE,3(RE)                                                         
         BCT   RF,LR182                                                         
         DC    H'0'                                                             
*                                                                               
LR184    MVC   LSMGRP(2),0(RE)                                                  
*                                                                               
         GOTO1 HEXOUT,DMCB,MKGKMGRP,FULL,2,=C'TOG',0                            
*                                                                               
         LA    RF,LSMGRP+2                                                      
         CLI   LSMGRP+1,C' '                                                    
         BH    *+6                                                              
         BCTR  RF,0                                                             
         SR    RE,RE                                                            
         IC    RE,MBRKLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),FULL                                                     
*                                                                               
         OC     LISMKGN,LISMKGN            ANY MGROUP FILT ON SCREEN?           
         BZ     LR190                      NOPE                                 
         CLI   LISMKGN,C'='        MARKET FILTER?                               
         BNE   LR184C                                                           
         MVC   SAVEKEY2,KEY                                                     
         LA    R4,KEY                                                           
         USING MKGKEY,R4                                                        
         MVC   MKGPTYP,=X'0D82'                                                 
         MVC   MKGPAGMD(MKGPMKT-MKGPAGMD),SAVEKEY2+(MKGKAGMD-MKGKEY)            
         MVC   MKGPMKT,QMARKET                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
*                                                                               
         CLC   KEY(13),KEYSAVE            IF MARKET ALREADY EXIST INVAL         
         BE    *+12                                                             
         BRAS  RE,LRRSKEY                                                       
         B     LR20                                                             
         BRAS  RE,LRRSKEY                                                       
         B     LR190                                                            
*                                                                               
LRRSKEY  NTR1  LABEL=*             RESTORE KEY AND REC SEQ                      
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY2),SAVEKEY2                                         
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         JNE   *+26                                                             
                                                                                
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
                                                                                
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         JE    XIT                                                              
         DC    H'0'                                                             
*                                                                               
LR184C   MVC    FULL(1),QMGR               FILTER FROM SCREEN                   
         BRAS   RE,MGRNUM                  GET INDEX INTO SORTED TABLE          
         MVC    FULL+1(1),FULL             SAVE THAT INDEX                      
         MVC    FULL(1),MKGKMID            MGROUP IN RECORD                     
         BRAS   RE,MGRNUM                  GET INDEX INTO SORTED TABLE          
         CLC    FULL+1(1),FULL             THIS REC BE4 ALPHABETICALLY?         
         BH     LR20                       YES, GET NEXT REC                    
         BL     LR190                      NO, IT IS LESS                       
*                                                                               
         CLI    MGRMKGN+1,X'40'            IS THERE A SECOND CHAR/NUM?          
         BNH    LR190                      NO                                   
         CLI    MGRMKGN+1,C'Z'             IS SECOND CHAR NUMERIC?              
         BH     LR185                      YES, CHECK IT                        
         CLI    MGRMKGN+2,X'40'            IS A NUM IN THIRD POSITION?          
         BNH    LR190                      NO                                   
* THEY ARE EQUAL SO CHECK MKTGRP NUMBER                                         
LR185    CLC    QMGRP,MKGKMGRP             DID WE PASS NUM WE WANT?             
         BH     LR20                                                            
*                                                                               
*    GET X'10 ELEMENT                                                           
*                                                                               
LR190    L      R6,AIO                                                          
         USING  MKGEL10,R6                                                      
         MVI    ELCODE,X'10'                                                    
         BAS    RE,GETEL                                                        
         BE     *+8                                                             
         B      LR220                                                           
*        B      LR195                                                           
*  MOVE BREAK TITLES TO LIST                                                    
         MVC    LSBKNM1,MKGNAM1                                                 
         MVC    LSBKNM2,MKGNAM2                                                 
         MVC    LSBKNM3,MKGNAM3                                                 
         DROP   R6                                                              
*                                                                               
LR195    CLI    MODE,PRINTREP                                                   
         BNE    LR220                                                           
         CLC    =C'SOON',CONWHEN    SKIP IO CHECK IF RUNNING SOON               
         BE     LR196                                                           
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         ICM   R3,3,FATMAXIO       MAXIO COUNT IS "SOFT"                        
         MHI   R3,9                                                             
         D     R2,=F'10'                                                        
         CLM   R3,3,FATIOCNT       COMPARE IT TO CURRENT COUNT OF IO'S          
         BH    LR196                                                            
         DROP  R1                                                               
*                                                                               
         LA    R2,CONWHENH                                                      
         B     ERRIO                                                            
*                                                                               
LR196    MVC    P,SPACES                                                        
         MVC    PMED,LSMED                                                      
         MVC    PCLT,LSCLT                                                      
         MVC    PPRGP(L'LSPGRP),LSPGRP                                          
         MVC    PMKGP(L'LSMGRP),LSMGRP                                          
         MVC    PBKNM1(L'LSBKNM1),LSBKNM1                                       
         MVC    PBKNM2(L'LSBKNM2),LSBKNM2                                       
         MVC    PBKNM3(L'LSBKNM3),LSBKNM3                                       
*                                                                               
         XC     FAKEFLD,FAKEFLD                                                 
         LA     R2,FAKEFLD                                                      
         MVI    5(R2),1                                                         
         MVC    FAKEFLD+8(L'LSMED),LSMED                                        
         MVI    USEIONUM,3                                                      
         XC     SAVEKEY2,SAVEKEY2                                               
         MVC    SAVEKEY2,KEY                                                    
         CLC    LSMED,SVMED         SAME MEDIA AS BEFORE?                       
         BNE    LR197               NO                                          
         MVC    PMEDNM(L'MEDNM),SVMEDNM                                         
         B      LR198                                                           
*                                                                               
LR197    MVC    SVMED,LSMED                                                     
         GOTO1  VALIMED                                                         
         MVC    PMEDNM(L'MEDNM),MEDNM                                           
         MVC    SVMEDNM,MEDNM                                                   
*                                                                               
LR198    XC     FAKEFLD,FAKEFLD                                                 
         CLC    LSCLT,=C'ALL'                                                   
         BNE    *+14                                                            
         XC     PCLTNM,PCLTNM                                                   
         B      LR200                                                           
*                                                                               
         LA     R2,FAKEFLD                                                      
         MVI    5(R2),3                                                         
         MVC    FAKEFLD+8(L'LSCLT),LSCLT                                        
         MVI    USEIONUM,3                                                      
         GOTO1  VALICLT                                                         
         MVC    PCLTNM,CLTNM                                                    
                                                                                
*    RESTORE KEY                                                                
LR200    MVC    AIO,AIO1                                                        
         XC     KEY,KEY                                                         
         MVC    KEY(L'SAVEKEY2),SAVEKEY2                                        
         GOTO1  READ                                                            
         CLI    DMCB+8,0                                                        
         BE     *+6                                                             
         DC     H'0'                                                            
*                                                                               
         GOTO1  SPOOL,DMCB,SPOOLD                                               
         B      LR20                                                            
*                                                                               
LR220    GOTO1  LISTMON                                                         
         CLI    FIRSTLST,0               FIRST TIME ON THIS PAGE?               
         BNE    LR20                     NOPE                                   
         MVC    FIRSTKEY,KEY             SAVE THIS IN CASE OF SELECT            
         MVI    FIRSTLST,1               NOT FIRST KEY ANYMORE FLAG             
         B      LR20                     AND READ SEQ                           
         DROP   R4                                                              
LRX      B      XIT                                                             
*                                                                               
DEL      CLI    SVAPROF+7,C'C'           CANADIAN AGENCY?                       
         BNE    XIT                      NO                                     
         CLI    QMED,C'N'                MEDIA N?                               
         BE     ERRNODEL                 YES - ERROR                            
         CLI    QMED,C'C'                MEDIA C?                               
         BE     ERRNODEL                 YES - ERROR                            
         B      XIT                      EXIT                                   
*************************************                                           
* ERRORS                                                                        
*************************************                                           
ERRMRKT  MVC   ERRNUM,=AL2(BADMRKT)                                             
         B     SPERREX                                                          
ERRDUPM  MVC   ERRNUM,=AL2(MRKTDUP)                                             
         B     SPERREX                                                          
ERRPGRG  MVC   ERRNUM,=AL2(PGRNILIS)                                            
         B     SPERREX                                                          
ERRCLT   MVC   ERRNUM,=AL2(CLTNILIS)                                            
         B     SPERREX                                                          
ERRLIS1  MVC   ERRNUM,=AL2(LISTERR1)                                            
         B     SPERREX                                                          
ERRLIS2  MVC   ERRNUM,=AL2(LISTERR2)                                            
         B     SPERREX                                                          
ERRECP1  MVC   ERRNUM,=AL2(NOTNECP1)                                            
         B     SPERREX                                                          
ERRECP2  MVC   ERRNUM,=AL2(NOTNECP2)                                            
         B     SPERREX                                                          
ERRSEC2  MVC   ERRNUM,=AL2(NOTAUTH)                                             
         B     SPERREX                                                          
DELMSG   MVC   ERRNUM,=AL2(1368)                                                
         B     SPERREX                                                          
RESMSG   MVC   ERRNUM,=AL2(1369)                                                
         B     SPERREX                                                          
ERRNODEL MVC   ERRNUM,=AL2(1370)    CAN'T DEL/RES MEDIA N/C RECORDS             
         B     SPERREX                                                          
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRIO    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(29),=C'REPORT TOO BIG TO PROCESS NOW'                    
         GOTO1 ERREX2                                                           
NOPDEF   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(27),=C'NO PRODUCT DEFINTION RECORD'                      
         GOTO1 ERREX2                                                           
NOMDEF   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(26),=C'NO MARKET DEFINTION RECORD'                       
         GOTO1 ERREX2                                                           
ERRMGLEN XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(29),=C'MARKET GROUP BREAK LENGTH IS '                    
         EDIT  (R0),(1,CONHEAD+29)                                              
         GOTO1 ERREX2                                                           
DUPERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(22),=C'ERROR, PLEASE CALL DDS'                           
         OI    CONHEADH+6,X'80'                                                 
         DC    H'0',C'$ABEND'                                                   
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*                                                                               
NOTAUTH  EQU   175                 NOT AUTHORIZED FOR THIS FUNCTION             
BADMRKT  EQU   453                                                              
MRKTDUP  EQU   454                                                              
PGRNILIS EQU   455                                                              
NOTNECP1 EQU   461                                                              
NOTNECP2 EQU   462                                                              
CLTNILIS EQU   456                                                              
LISTERR1 EQU   457                                                              
LISTERR2 EQU   458                                                              
DUPADDQ  EQU   472                                                              
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
*****************************************************************               
*         VALIDATE PRODUCT GROUP NUMBER                                         
*****************************************************************               
VALP00   CLC   CLIENT,=C'ALL'                                                   
         BNE   VALP10                                                           
         CLC   =C'ALL',MGRPGP                                                   
         BNE   ERRINV                                                           
         XC    SVBCLT,SVBCLT                                                    
VALP05   XC    PGRPNUM,PGRPNUM                                                  
         B     VALPX                                                            
*                                                                               
VALP10   CLC   =C'ALL',MGRPGP                                                   
         BE    VALP05                                                           
*****  A TO Z ALL VALID BESIDES C'N'                                            
**       CLI   PGRPID,C'V'                                                      
**       BL    ERRINV                                                           
**       CLI   PGRPID,C'Z'                                                      
**       BH    ERRINV                                                           
         CLI   PGRPID,C'A'                                                      
         BL    ERRINV                                                           
         CLI   PGRPID,C'Z'                                                      
         BH    ERRINV                                                           
         CLI   PGRPID,C'N'         C'N' IS NOT VALID                            
         BE    ERRINV                                                           
*                                                                               
         LLC   RE,5(R2)                                                         
         CLI   5(R2),1            IF NO PRODGROUP NUM JUST ALLOW                
         BE    VALP80             IT TO MEAN ALL UNDER THAT                     
*                                                                               
VALP50   LA    R4,9(R2)                                                         
         BCTR  RE,0                                                             
*                                                                               
VALP60   OC    0(1,R4),0(R4)            THIS PART CHECKS THAT DIGITS            
         BZ    VALP70                   ENTER FOR PRODUCT GROUP NUMBER          
         CLI   0(R4),C'0'               ARE VALID NUMERIC DIGITS                
         BL    ERRINV                                                           
         CLI   0(R4),C'9'                                                       
         BH    ERRINV                                                           
VALP70   LA    R4,1(R4)                                                         
         BCT   RE,VALP60                                                        
*  BUILD PRODUCT GROUP KEY TO SEE IF THE PRODUCT GROUP EXIST                    
VALP80   LA    R5,KEY                                                           
         USING PRGRECD,R5                                                       
         XC    KEY,KEY                                                          
         MVC   PRGKTYP,=X'0D01'                                                 
         MVC   PRGKAGMD,SVBAGYMD                                                
         MVC   PRGKCLT,SVBCLT                                                   
         MVC   PRGKID,PGRPID                                                    
         LLC   R4,5(R2)                                                         
*                                                                               
         CLI   5(R2),1            IF NO PRODGROUP NUM JUST ALLOW                
         BNE   *+14                                                             
         XC    PGRPNO,PGRPNO      PRDGROUP ID.                                  
         B     VALP90                                                           
*                                                                               
         MVC   WORK(4),=C'0000'                                                 
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
*                                                                               
         MVC   WORK(0),9(R2)       MARKET NUMBER IS PACKED BINARY               
         XC    DUB,DUB                                                          
         PACK  DUB(3),WORK(5)                                                   
         MVC   PGRPNO,DUB                                                       
* THIS PART IS FOR PRODUCT ID FOR ALL PRODUCTS OF ID                            
**       CLC   PGRPNO,=X'0000'    SKIP PRODUCT DEFINITION                       
**       BE    ERRINV                                                           
*                                                                               
VALP90   MVC   PRGKGRP,PGRPNO                                                   
         DROP  R5                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   KEY(13),KEYSAVE    IF NOT IN PRODUCT GROUP RECORDS               
         BNE   NOPDEF             THEN ITS INVALID                              
* IF VALID PRODUCT GROUP STORE BINARY VALUE OF PRODUCT GROUP                    
         XC    PGRPNM,PGRPNM                                                    
         MVC   PGRPNM,PGRPNO      KEY FIELD                                     
VALPX    B     VK90                                                             
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
**** CONSTANTS AND TABLES                                                       
*                                                                               
**** TABLE FOR MEDIA AND ELEMENT CODES ****                                     
MEDTAB   DS    0H                                                               
         DC    CL1'T',XL1'01'                                                   
MTABLQ   EQU   *-MEDTAB                                                         
         DC    CL1'R',XL1'02'                                                   
         DC    CL1'N',XL1'03'                                                   
         DC    CL1'X',XL1'04'                                                   
         DC    CL1'C',XL1'08'                                                   
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE SPMGRTAB                                                       
         EJECT                                                                  
***********************************************************************         
*                FIND THE FIRST MGROUP ID TO READ FOR                 *         
***********************************************************************         
FRSTMGRP NTR1   BASE=*,LABEL=*                                                  
*                                                                               
         LA     R1,MGRSORT               SORTED MGROUP TABLE                    
         CLC    MGRMKGN,SPACES           ANY MARKET GROUP ON SCREEN?            
         JNH    FMGX                     NOPE                                   
*                                                                               
         CLI   MGRMKGN,C'='        MARKET FILTER?                               
         JE    FMGX                                                             
*                                                                               
         LA     R2,0                     FOR EX (COMPARE FOR 1)                 
         CLI    MGRMKGN+1,X'40'          USING 2-CHAR MARKET GROUPS?            
         JNH    FMG01                    DEFINITELY NOT                         
         CLI    MGRMKGN+1,C'Z'           USING 2-CHAR MARKET GROUPS?            
         JH     FMG01                    NO                                     
         AHI    R2,1                     YES, SO COMPARE FOR 2                  
*                                                                               
FMG01    CLI    0(R1),0                  END OF TABLE?                          
         JE     *+2                                                             
         EX     R2,*+8                                                          
         J      *+10                                                            
         CLC    MGRMKGN(0),0(R1)         MATCH?                                 
         JE     FMGX                     YES                                    
         LA     R1,3(R1)                 NEXT ENTRY                             
         J      FMG01                                                           
*                                                                               
FMGX     MVC    MGRFILTR,2(R1)           FIRST SORTED MGROUP                    
         J      XIT                                                             
         EJECT                                                                  
***********************************************************************         
*                FIND INDEX INTO SORTED TABLE                         *         
* INPUT : MGROUP TO LOOK FOR (BINARY) IS IN FULL(1)                   *         
* OUTPUT: RETURNS INDEX IN FULL(1)                                    *         
***********************************************************************         
MGRNUM   NTR1   BASE=*,LABEL=*                                                  
*                                                                               
         LA     R1,MGRSORT                                                      
         SR     R3,R3                                                           
*                                                                               
MNUM05   CLI    0(R1),0                                                         
         JE     *+2                                                             
         CLC    FULL(1),2(R1)                                                   
         JE     MNUMX                                                           
         LA     R1,3(R1)                                                        
         AHI    R3,1                                                            
         J      MNUM05                                                          
*                                                                               
MNUMX    STC    R3,FULL                                                         
         J      XIT                                                             
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                                  
***********************************************************************         
*                                                                               
SETUP    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         NI    DMINBTS,X'FF'-X'08' TURN OFF READ FOR DELETES                    
         LA    R2,CONACTH          POINT TO ACTION                              
         CLI   T217FFD+1,C'*'      TEST DDS TERM                                
         BE    SETUP01                                                          
         TM    T217FFD+12,X'01'                                                 
         BNO   SETUP01             NOT ON = ALL OK                              
         CLI   ACTNUM,ACTCHA                                                    
         BE    ERRSEC2             CHANGE NOT ALLOWED                           
         CLI   ACTNUM,ACTADD                                                    
         BE    ERRSEC2             ADD NOT ALLOWED                              
*                                                                               
SETUP01  OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         OI    GENSTAT4,NODELLST   NO DELETION ALLOWED                          
         OI    GENSTAT2,DISTHSPG                                                
         OI    GENSTAT4,NODUPDIE                                                
                                                                                
*============================================================                   
* CREATE SORTED ID TABLE                                                        
*============================================================                   
                                                                                
         LA    R0,MGRSORT                                                       
         LHI   R1,MGRSORTX-MGRSORT                                              
         L     RE,=A(SPMGRTAB)                                                  
         A     RE,RELO                                                          
         LHI   RF,SPMGRTBX-SPMGRTAB                                             
         MVCL  R0,RE                                                            
*                                                                               
         LHI   R0,(SPMGRTBX-SPMGRTAB)/3                                         
         GOTO1 QSORT,DMCB,MGRSORT,(R0),3,2,0                                    
*                                                                               
SETUPX   J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CHECK TO SEE IF CLIENT HAS THE "AAN" OPTION ON                         
***********************************************************************         
AAN      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   CLTAAN,C'N'         DEFAULT TO NO!                               
         XC    SVKEY1,SVKEY1                                                    
         MVC   SVKEY1,KEY                                                       
         MVC   SVAIO,AIO                                                        
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),SVKEY1+2                                                
         MVC   KEY+2(2),SVKEY1+3                                                
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BNE   AANX                                                             
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING CLTHDR,R6                                                        
         MVC   CLTAAN,CPROF+6                                                   
         DROP  R6                                                               
         MVC   AIO,SVAIO           RESTORE AIO                                  
         MVC   KEY,SVKEY1          RESTORE KEY                                  
         GOTO1 HIGH                RESTORE READ SEQUENCE                        
         GOTO1 GETREC                                                           
AANX     J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CLEAR  FIELDS ON THE SCREEN                                            
***********************************************************************         
*                                                                               
CLRSCR   NTR1  BASE=*,LABEL=*                                                   
         XC    MGRUSER,MGRUSER                                                  
         OI    MGRUSERH+6,X'80'                                                 
* IF CLIENT =ALL OR BLANK THAN CLEAR SCREEN                                     
         CLC   MGRCLT,=C'ALL'                                                   
         BNE   CLRS10                                                           
*                                                                               
         XC    MGRCLN,MGRCLN                                                    
         OI    MGRCLNH+6,X'80'                                                  
         B     CLRS10                                                           
*                                                                               
         CLI   MGRCLTH+5,0                                                      
         BNZ   CLRS10                                                           
*                                                                               
         XC    MGRCLN,MGRCLN                                                    
         OI    MGRCLNH+6,X'80'                                                  
         B     CLRS10                                                           
*                                                                               
CLRS10   LA    R2,MGRAM1H                                                       
         LA    R3,MGRAMLH                                                       
CLRS30   CR    R2,R3                                                            
         BH    CLRS40                                                           
*                                                                               
         LLC   R4,0(R2)                                                         
         SHI   R4,8                      SUBTRACT HEADER 8 BYTES                
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
*                                                                               
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
         LLC   R4,0(R2)                                                         
         AR    R2,R4                                                            
         B     CLRS30                                                           
*                                                                               
*   CLEAR THE MARKET CODES *                                                    
CLRS40   LA    R2,MGRML1H                                                       
         LA    R3,MGRENDH                                                       
*                                                                               
CLRS60   CR    R2,R3                                                            
         BH    CLRS80                                                           
*                                                                               
         LLC   R4,0(R2)                                                         
         AHI   R4,-8                     SUBTRACT HEADER 8 BYTES                
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
*                                                                               
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
         LLC   R4,0(R2)                                                         
         AR    R2,R4                                                            
         B     CLRS60                                                           
*                                                                               
CLRS80   MVC   MGRADD1,SPACES           CLEAR ADDRESS FIELDS                    
         OI    MGRADD1H+6,X'80'                                                 
         MVC   MGRADD2,SPACES                                                   
         OI    MGRADD2H+6,X'80'                                                 
         MVC   MGRADD3,SPACES                                                   
         OI    MGRADD3H+6,X'80'                                                 
         MVC   MGRADD4,SPACES                                                   
         OI    MGRADD4H+6,X'80'                                                 
*                                                                               
         J     XIT                                                              
         LTORG                                                                  
***********************************************************************         
* JUST DELETED A MARKET GROUP RECORD SO...                            *         
* 1) DELETE MEDIA N/C MGROUP RECORD IF CANADIAN TV                    *         
* 2) DELETE THE X'05' ELEMS FROM THE MGROUP ASSGNMNT (N/C FOR CANADA) *         
* 3) DELETE THE X'0D82' PASSIVES (N/C FOR CANADA)                     *         
***********************************************************************         
XDEL     NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2             DON'T CLOBBER AIO1!                         
         CLI   SVAPROF+7,C'C'       CANADIAN AGENCY?                            
         BNE   XDEL20               NO                                          
         CLI   QMED,C'T'            MEDIA T?                                    
         BNE   XDEL20               NO                                          
         MVC   THISAM,SAVEKEY+2     A/M                                         
         NI    THISAM,X'F0'         STRIP MEDIA BITS                            
         OI    THISAM,X'03'         START WITH MEDIA N                          
         MVC   KEY(13),SAVEKEY      MARKET GROUP KEY                            
         L     R6,AIO               A(AIO2)                                     
*                                                                               
XDEL05   MVC   KEY+2(1),THISAM      NEW A/M                                     
         GOTO1 HIGH                 READ HIGH                                   
         CLC   KEY(13),KEYSAVE      FOUND MGROUP FOR NEW A/M?                   
         BNE   XDEL10               NO - NOTHING TO DELETE                      
*                                                                               
         GOTO1 GETREC               GET THE RECORD                              
         OI    15(R6),X'80'         DELETE THE RECORD                           
         GOTO1 PUTREC               WRITE IT BACK                               
         OI    KEY+13,X'80'         DELETE THE KEY                              
         GOTO1 WRITE                WRITE IT BACK                               
*                                                                               
XDEL10   TM    THISAM,X'08'         JUST PROCESSED MEDIA C?                     
         BNZ   XDEL20               YES                                         
         NI    THISAM,X'F0'         STRIP MEDIA BITS                            
         OI    THISAM,X'08'         SET FOR MEDIA C                             
         B     XDEL05               GO PROCESS MEDIA C                          
*                                                                               
XDEL20   MVC   THISAM,SVBAGYMD      A/M                                         
*                                                                               
XDEL21   LA    R4,KEY               R4 = KEY                                    
         USING MKGPTYP,R4           R4 = X'0D82' PASSIVE DSECT                  
         XC    KEY,KEY              CLEAR THE KEY                               
         MVC   MKGPTYP,=X'0D82'     X'0D82' MGROUP ASSIGNMENT RECS              
         MVC   MKGPAGMD,THISAM      A/M                                         
         MVC   MKGPCLT(8),SAVEKEY+3 CLIENT/PGROUP/MGROUP                        
         NI    DMINBTS,X'FF'-X'08'  DON'T WANT DELETED 0D82'S!                  
         GOTO1 HIGH                 READ HIGH                                   
         B     XDEL30                                                           
*                                                                               
XDEL25   GOTO1 SEQ                  READ SEQ                                    
*                                                                               
XDEL30   CLC   KEY(11),KEYSAVE      HAVE PASSIVE FOR A MARKET?                  
         BNE   XDEL40               NO - DONE                                   
*                                                                               
         GOTO1 GETREC               GET X'0D03' RECORD                          
         L     R6,AIO               A(X'0D03' RECORD)                           
         MVI   ELCODE,X'05'         LOOK FOR X'05' ELEM                         
         BAS   RE,GETEL             GET THE ELEMENT                             
         B     *+8                  BRANCH OVER NEXTEL                          
XDEL35   BAS   RE,NEXTEL            GET THE NEXT ELEMENT                        
         BE    *+6                  HAVE ONE?                                   
         DC    H'0'                 NO - DEATH!                                 
*                                                                               
         USING MKAEL05,R6           X'05' ELEMENT DSECT                         
         CLC   MKAPGRP(6),MKGKPID   MATCH ON PGROUP AND MGROUP?                 
         BNE   XDEL35               NO                                          
*                                                                               
         GOTO1 RECUP,DMCB,AIO,(R6)  YES - DELETE THE ELEMENT                    
         GOTO1 PUTREC               WRITE THE RECORD BACK                       
         OI    KEY+13,X'80'         MARK PASSIVE KEY DELETED                    
         GOTO1 WRITE                WRITE KEY BACK                              
         B     XDEL25                                                           
*                                                                               
XDEL40   CLI   SVAPROF+7,C'C'       CANADIAN AGENCY?                            
         BNE   XDELX                NO - DONE                                   
         CLI   QMED,C'T'            MEDIA T?                                    
         BNE   XDELX                NO - DONE                                   
         TM    THISAM,X'08'         ALREADY PROCESSED MEDIA C?                  
         BNZ   XDELX                YES - DONE                                  
         TM    THISAM,X'02'         ALREADY PROCESSED MEDIA N?                  
         BNZ   XDEL45               YES - GO PROCESS MEDIA C                    
         NI    THISAM,X'F0'         STRIP MEDIA BITS                            
         OI    THISAM,X'03'         START WITH MEDIA N                          
         B     XDEL21               GO PROCESS MEDIA N RECORDS                  
*                                                                               
XDEL45   NI    THISAM,X'F0'         STRIP MEDIA BITS                            
         OI    THISAM,X'08'         PROCESS MEDIA C                             
         B     XDEL21               GO PROCESS MEDIA C RECORDS                  
*                                                                               
XDELX    MVC   AIO,AIO1             RESTORE AIO1                                
         J     DELMSG               GIVE DELETE MESSAGE                         
         DROP  R4,R6                                                            
         LTORG                                                                  
***********************************************************************         
* JUST RESTORED A MARKET GROUP RECORD SO...                           *         
* 1) RESTORE MEDIA N/C MGROUP RECORD IF CANADIAN TV                   *         
***********************************************************************         
XRES     NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2             DON'T CLOBBER AIO1!                         
         OI    DMINBTS,X'08'        READ DELETED RECORDS                        
         CLI   SVAPROF+7,C'C'       CANADIAN AGENCY?                            
         BNE   XRES20               NO                                          
         CLI   QMED,C'T'            MEDIA T?                                    
         BNE   XRES20               NO                                          
         MVC   THISAM,SAVEKEY+2     A/M                                         
         NI    THISAM,X'F0'         STRIP MEDIA BITS                            
         OI    THISAM,X'03'         START WITH MEDIA N                          
         MVC   KEY(13),SAVEKEY      MARKET GROUP KEY                            
         L     R6,AIO               A(AIO2)                                     
*                                                                               
XRES05   MVC   KEY+2(1),THISAM      NEW A/M                                     
         GOTO1 HIGH                 READ HIGH                                   
         CLC   KEY(13),KEYSAVE      FOUND MGROUP FOR NEW A/M?                   
         BNE   XRES10               NO - NOTHING TO RESTORE                     
*                                                                               
         GOTO1 GETREC               GET THE RECORD                              
         NI    15(R6),X'FF'-X'80'   RESTORE THE RECORD                          
         GOTO1 PUTREC               WRITE IT BACK                               
         NI    KEY+13,X'FF'-X'80'   RESTORE THE KEY                             
         GOTO1 WRITE                WRITE IT BACK                               
*                                                                               
XRES10   TM    THISAM,X'08'         JUST PROCESSED MEDIA C?                     
         BNZ   XRES20               YES                                         
         NI    THISAM,X'F0'         STRIP MEDIA BITS                            
         OI    THISAM,X'08'         SET FOR MEDIA C                             
         B     XRES05               GO PROCESS MEDIA C                          
*                                                                               
XRES20   MVC   AIO,AIO1             RESTORE AIO1                                
         NI    DMINBTS,X'FF'-X'08'  TURN OFF READ FOR DELETES                   
         J     RESMSG               GIVE RESTORE MESSAGE                        
         LTORG                                                                  
***********************************************************************         
*                                                                               
HEDSPECS DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,30,C'REP LIST'                                                
         SSPEC H2,30,C'--------'                                                
         SPACE 1                                                                
         SSPEC H4,1,C'MEDIA'                                                    
         SSPEC H5,1,C'-----'                                                    
         SSPEC H4,17,C'CLIENT'                                                  
         SSPEC H5,17,C'------'                                                  
         SSPEC H4,47,C'PGRP'                                                    
         SSPEC H5,47,C'-----'                                                   
         SSPEC H4,54,C'MKGPGRP'                                                 
         SSPEC H5,54,C'-------'                                                 
         SSPEC H4,66,C'BREAK NAME1'                                             
         SSPEC H5,66,C'-----------'                                             
         SSPEC H4,86,C'BREAK NAME2'                                             
         SSPEC H5,86,C'-----------'                                             
         SSPEC H4,106,C'BREAK NAME3'                                            
         SSPEC H5,106,C'-----------'                                            
         DC    X'00'                                                            
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMA8D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMA9D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKG                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENPRG                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
PGRPNO   DS    XL2                                                              
STATUS   DS    XL1                      STATUS BYTE FLAG                        
KEYCHG   EQU   X'80'                    KEY HAS CHANGED                         
CLIENT   DS    CL3                      CLIENT STORAGE                          
MEDIA    DS    CL1                      MEDIA STORAGE                           
BKNUM    DS    X                        NUMBER OF BREAKS                        
PGRPNUM  DS    0CL5                      PRODUCT GROUP                          
PGRPID   DS    CL1                      PRODUCT GROUP ID                        
PGRPNM   DS    CL4                      PRODUCT GROUP NUMBER                    
SAVEKEY  DS    CL13                                                             
SAVEKEY2 DS    CL13                                                             
MDEFKEY  DS    CL13                                                             
PDEFKEY  DS    CL13                                                             
PASSADDR DS    A                        ADDRESS STORAGE                         
PASSADDC DS    A                        ADDRESS STORAGE                         
PASSADDN DS    A                        ADDRESS STORAGE                         
ADDMRT   DS    11XL2                    TABLE FOR STORING VALID MARKETS         
SVBAGYMD DS    XL1                      SAVED AGENCY/MEDIA CODE                 
SVBCLT   DS    XL2                      SAVED CLIENT CODE                       
SVBKLNS  DS    XL2                      SAVED BREAK LENGTHS                     
INPRD    DS    CL3                      INPUT PRODUCT                           
SVPGRP   DS    XL3                                                              
SAVEADDR DS    A                                                                
SVMGRP   DS    XL3                                                              
MRKTIO   DS    CL32                                                             
MBRKLEN  DS    X                                                                
PBRKLEN  DS    X                                                                
AGYHIGH  DS    X                                                                
STARTNO  DS    XL2                                                              
FAKEFLD  DS    XL11                                                             
QMGRPLN  DS    X                                                                
QMGR     DS    X                                                                
QMGRP    DS    XL2                                                              
QMARKET  DS    XL2                                                              
*                                                                               
ERRNUM   DS    XL2                                                              
SAVESEL  DS    CL1                                                              
MYMKTIO  DS    CL300                NEW ADDR ELEM->NEEDS TO BE BIGGER           
*                                                                               
SVAIO    DS    F                                                                
SVKEY1   DS    XL20                                                             
CLTAAN   DS    CL1                                                              
SVBCLI   DS    XL2                                                              
MGRFILTR DS    XL1                  ALPHABETICAL ORDER FOR MGROUP IN LR         
LASTMED  DS    CL1                  LAST MEDIA                                  
FIRSTKEY DS    XL13                 FIRST KEY ON PAGE (SEL ON LST)              
FIRSTLST DS    XL1                  FIRST KEY ON LIST FLAG                      
*                                                                               
MGRSORT  DS    XL768               SORTED MGRTAB                                
MGRSORTX EQU   *                                                                
*                                                                               
SVMED    DS    CL1                                                              
SVMEDNM  DS    CL10                                                             
*                                                                               
THISAM   DS    XL1                  THIS A/M                                    
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
*                                                                               
**** ONLINE LIST LINE                                                           
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL1                                                              
LSMED    DS    CL1                                                              
         DS    CL4                                                              
LSCLT    DS    CL3                                                              
         DS    CL4                                                              
LSPGRP   DS    0CL5                                                             
LSPGRPID DS    CL1                                                              
LSPGRPNM DS    CL4                                                              
         DS    CL2                                                              
LSMGRP   DS    0CL6                                                             
LSMGRPID DS    CL2                                                              
LSMGRPNM DS    CL4                                                              
         DS    CL2                                                              
LSBKNM1  DS    CL15                                                             
         DS    CL1                                                              
LSBKNM2  DS    CL15                                                             
         DS    CL1                                                              
LSBKNM3  DS    CL15                                                             
         EJECT                                                                  
*                                                                               
SPOOLD   DSECT                                                                  
         ORG  P                                                                 
         DS   CL1                                                               
PMED     DS   CL1                                                               
         DS   CL2                                                               
PMEDNM   DS   CL10                                                              
         DS   CL2                                                               
PCLT     DS   CL3                                                               
         DS   CL2                                                               
PCLTNM   DS   CL20                                                              
         DS   CL5                                                               
PPRGP    DS   CL5                                                               
         DS   CL2                                                               
PMKGP    DS   CL6                                                               
         DS   CL6                                                               
PBKNM1   DS   CL18                                                              
         DS   CL2                                                               
PBKNM2   DS   CL18                                                              
         DS   CL2                                                               
PBKNM3   DS   CL18                                                              
         DS   CL2                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'110SPSFM28   09/04/15'                                      
         END                                                                    
