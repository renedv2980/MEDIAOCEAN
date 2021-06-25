*          DATA SET PRSFM12    AT LEVEL 028 AS OF 11/06/00                      
*PHASE T41C12A,*                                                                
*                                                                     *         
*                                                                     *         
*        CHANGE LOG                                                   *         
*                                                                     *         
* SMYE  11/02/00  ADDED CLIENT VALIDATION (SECURITY) TO DK                      
*                                                                     *         
*                                                                     *         
*  TITLE        T41C12 - BUDGET RECORDS MAINT/LIST/REPORT             *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41C00 (SFM PRINT CONTROLLER)              *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, CHANGE, LIST, REPORT, TOTALS   *         
*                                                                     *         
*  INPUTS       SCREEN T41CD1 (MAINTENANCE)                           *         
*               SCREEN T41CC1 (LIST)                                  *         
*                                                                     *         
*  OUTPUTS      UPDATED BUDGET RECORDS                                *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - GETEL REGISTER/WORK                                   *         
*          R7 - SECOND BASE                                           *         
*          R8 - SPOOLD/WORK                                           *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM/WORK                                           *         
*          RF - SYSTEM/WORK                                           *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         TITLE 'T41C12  BUDGET RECORDS'                                         
T41C12   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C12,R7                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
         CLI   MODE,RECDEL                                                      
         BNE   EXIT                                                             
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       DS    0H                                                               
*                                                                               
         BAS   RE,CLRNAME                                                       
         BAS   RE,CLRSAVE                                                       
*                                                                               
         LA    R2,BUDMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         CLI   MEDNM,0              MEANS MEDIA NOT FOUND                       
         BE    VKDERR                                                           
         FOUT  BUDMEDNH,MEDNM,10                                                
         MVC   SVMED,QMED                                                       
***      OI    4(R2),X'20'                                                      
***      FOUT  BUDMEDH                                                          
*                                                                               
VK1      XC    QCLT,QCLT                                                        
         LA    R6,MYKEY                                                         
         USING BUDHDRD,R6                                                       
         XC    PBUDREC(31),PBUDREC        CLEAR KEY FIELDS                      
         MVC   PBUDKAGY,AGENCY  CREATE KEY  -- AGENCY                           
         MVC   PBUDKMED,QMED                   MEDIA CODE                       
         MVI   PBUDKRCD,X'18'                  ID                               
*                                                                               
         LA    R2,BUDCLTH          CLIENT                                       
         CLI   5(R2),0                                                          
         BNE   VK2                                                              
         CLI   ACTNUM,ACTLIST       NO CLT  - OK IF LIST                        
         BE    VK3                                                              
*                                                                               
VKMISS   MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VK2      DS    0H                                                               
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   VK2D                                                             
         MVC   CLTNM(11),=C'ALL CLIENTS'   "ALL" ENTERED AS CLIENT CODE         
         FOUT  BUDCLTNH,CLTNM,11                                                
         CLI   ACTNUM,ACTLIST               OK IF LIST                          
         BE    VK3                                                              
         MVI   ERROR,INVCLI                 ERROR IF NOT                        
         B     TRAPERR                                                          
*                                                                               
VK2D     GOTO1 VALICLT                                                          
         CLI   CLTNM,0              MEANS CLIENT NOT FOUND                      
         BE    VKDERR                                                           
         FOUT  BUDCLTNH,CLTNM,20                                                
         MVC   PBUDKCLT,QCLT                                                    
         MVC   SVCLT,QCLT                                                       
         MVC   SVCPROFL,SVCPROF    CLIENT PROFILE                               
         CLI   SVCPROF+5,C'1'      MASTER CLIENT?                               
         BNE   VK2X                NO                                           
         MVI   ERROR,INVCLI                                                     
         B     TRAPERR                                                          
VK2X     DS    0H                                                               
***      OI    4(R2),X'20'                                                      
***      FOUT  BUDCLTH                                                          
*                                                                               
VK3      DS    0H                                                               
         XC    QPRD,QPRD                                                        
         LA    R2,BUDPRDH            PRODUCT                                    
         CLI   5(R2),0                                                          
         BNE   VK3E                                                             
         CLI   ACTNUM,ACTLIST       NO PRD - OK IF LIST                         
         BE    VK4                                                              
         CLI   ACTNUM,ACTREP        OK IF REPORT                                
         BE    VK4                                                              
         B     VKMISS              MUST HAVE PRODUCT                            
*                                                                               
VK3E     DS    0H                                                               
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   VK3K                                                             
         MVC   PRDNM(12),=C'ALL PRODUCTS'  "ALL" ENTERED AS PROD CODE           
         FOUT  BUDPRDNH,PRDNM,12                                                
         CLI   ACTNUM,ACTLIST               OK IF LIST                          
         BE    VK4                                                              
         CLI   ACTNUM,ACTREP                OK IF REPORT                        
         BE    VK4                                                              
         MVI   ERROR,INVPRD                 ERROR IF NOT                        
         B     TRAPERR                                                          
*                                                                               
VK3K     GOTO1 VALIPRD                                                          
         CLI   PRDNM,0              MEANS PRODUCT NOT FOUND                     
         BE    VKDERR                                                           
         FOUT  BUDPRDNH,PRDNM,20                                                
         MVC   PBUDKPRD,QPRD                   PRODUCT                          
         MVC   SVPRD,QPRD                                                       
         MVC   SVDIV,HOLDDIV       DIV CODE FROM PRODUCT RECORD                 
VK3X     DS    0H                                                               
***      OI    4(R2),X'20'                                                      
***      FOUT  BUDPRDH                                                          
*                                                                               
VK4      DS    0H                                                               
         XC    QEST,QEST                                                        
         LA    R2,BUDESTH            ESTIMATE                                   
         CLI   5(R2),0                                                          
         BNE   VK4E                                                             
         CLI   ACTNUM,ACTLIST       NO EST - OK IF LIST                         
         BE    VK5                                                              
         CLI   ACTNUM,ACTREP        OK IF REPORT                                
         BE    VK5                                                              
         B     VKMISS              MUST HAVE ESTIMATE                           
*                                                                               
VK4E     DS    0H                                                               
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   VK4K                                                             
         MVC   ESTNM(13),=C'ALL ESTIMATES'  "ALL" ENTERED AS ESTIMATE           
         FOUT  BUDESTNH,ESTNM,13                                                
         CLI   ACTNUM,ACTLIST               OK IF LIST                          
         BE    VK5                                                              
         CLI   ACTNUM,ACTREP                OK IF REPORT                        
         BE    VK5                                                              
         MVI   ERROR,INVEST                 ERROR IF NOT                        
         B     TRAPERR                                                          
*                                                                               
VK4K     GOTO1 VALIEST                                                          
         CLI   ESTNM,0              MEANS ESTIMATE NOT FOUND                    
         BE    VKDERR                                                           
*                                                                               
         FOUT  BUDESTNH,ESTNM,20                                                
         MVC   PBUDKEST,QEST        ESTIMATE                                    
         MVC   SVEST,QEST                                                       
         MVC   SVESTST,ESTDTST      ESTIMATE START DATE (YYMMDD)                
         MVC   SVESTEND,ESTDTEND    ESTIMATE END   DATE (YYMMDD)                
         GOTO1 DATCON,DMCB,(0,SVESTST),(5,BUDESTD)                              
         GOTO1 DATCON,DMCB,(0,SVESTEND),(5,BUDESTD+11)                          
         MVI   BUDESTD+9,C'-'                                                   
         FOUT  BUDESTDH                                                         
VK4X     DS    0H                                                               
***      OI    4(R2),X'20'                                                      
***      FOUT  BUDESTH                                                          
*                                                                               
VK5      DS    0H                                                               
         XC    QREG,QREG                                                        
         XC    SVREG,SVREG                                                      
         LA    R2,BUDREGH            REGION                                     
         CLI   5(R2),0                                                          
         BNE   VK5E                                                             
         CLI   ACTNUM,ACTLIST       NO REG - OK IF LIST                         
         BE    VK6                                                              
         CLI   ACTNUM,ACTREP        OK IF REPORT                                
         BE    VK6                                                              
         B     VKMISS              MUST HAVE REGION                             
*                                                                               
VK5E     DS    0H                                                               
         XC    REGNM,REGNM                                                      
         MVI   ERROR,INVREG                                                     
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   VK5F                                                             
         CLI   ACTNUM,ACTTOT       "ALL" ONLY FOR TOTALS  MODE                  
         BNE   TRAPERR                                                          
         MVC   SVREG,=C'ALL'                                                    
         B     VK6                                                              
VK5F     TM    4(R2),X'08'                                                      
         BO    VK5G                                                             
         B     TRAPERR                                                          
VK5G     ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         OI    DUB+7,X'0F'                                                      
         UNPK  QREG,DUB                                                         
         CLC   QREG,=3C'0'                                                      
         BE    VK5K                DO NOT VALIDATE 000 REGION                   
         MVC   QDIV,SVDIV          DIVISION FROM PRODUCT RECORD                 
         GOTO1 VALIREG                                                          
         CLI   REGNM,0              MEANS REGION NOT FOUND                      
         BE    VKDERR                                                           
*                                                                               
VK5K     FOUT  BUDREGNH,REGNM,20                                                
         MVC   PBUDKREG,QREG                   REGION                           
         MVC   SVREG,QREG                                                       
         FOUT  BUDREGH,QREG,3                                                   
VK5X     DS    0H                                                               
***      OI    4(R2),X'20'                                                      
***      FOUT  BUDREGH                                                          
*                                                                               
VK6      DS    0H                                                               
         XC    DSTNM,DSTNM                                                      
         MVI   ERROR,INVDST                                                     
         XC    QDST,QDST                                                        
         LA    R2,BUDDSTH          DISTRICT                                     
         CLC   SVREG,=C'ALL'                                                    
         BNE   VK6A                                                             
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   TRAPERR      INVALID DST - MUST BE ALL IF REG IS ALL             
         MVC   SVDST,=C'ALL'                                                    
         B     VK6X                                                             
VK6A     CLI   5(R2),0             TEST ANY INPUT                               
         BNE   VK6B                                                             
         CLI   ACTNUM,ACTLIST      NO DST - OK IF LIST                          
         BE    VK7                                                              
         CLI   ACTNUM,ACTREP       OK IF REPORT                                 
         BE    VK7                                                              
         B     VKMISS              MUST HAVE DISTRICT                           
*                                                                               
VK6B     DS    0H                                                               
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   VK6C                                                             
         CLI   ACTNUM,ACTTOT       "ALL" ONLY FOR TOTALS  MODE                  
         BNE   TRAPERR                                                          
         MVC   SVDST,=C'ALL'                                                    
         B     VK700               DONE WITH FIELD VALIDATION                   
VK6C     CLI   ACTNUM,ACTTOT                                                    
         BE    TRAPERR       INVALID DST - MUST BE ALL FOR TOTALS MODE          
VK6D     TM    4(R2),X'08'                                                      
         BO    VK6G                                                             
         B     TRAPERR             MUST BE NUMERIC                              
VK6G     ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         OI    DUB+7,X'0F'                                                      
         UNPK  QDST,DUB                                                         
         CLC   QDST,=3C'0'                                                      
         BE    VK6K                DO NOT VALIDATE 000 DISTRICT                 
         MVC   QDIV,SVDIV                SVDIV FROM PRODUCT RECORD              
         GOTO1 VALIDST                                                          
         CLI   DSTNM,0              MEANS DISTRICT NOT FOUND                    
         BE    VKDERR                                                           
*                                                                               
VK6K     FOUT  BUDDSTNH,DSTNM,20                                                
         MVC   PBUDKDST,QDST                   DISTRICT                         
         MVC   SVDST,QDST                                                       
         FOUT  BUDDSTH,QDST,3                                                   
VK6X     DS    0H                                                               
***      OI    4(R2),X'20'                                                      
***      FOUT  BUDDSTH                                                          
*                                                                               
VK7      DS    0H                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK800               IF LIST, DONE                                
         CLI   ACTNUM,ACTREP                                                    
         BE    VK800               IF REPORT, DONE                              
*                                                                               
VK8      DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VK700               IF NOT ADD, GO FINISH VALKEY                 
*                                                                               
*                          OUTPUT MONTH FIELDS FOR ADD SCREEN                   
*                                                                               
         L     R6,AIO                                                           
         USING BUDHDRD,R6                                                       
         XC    PBUDAMTS,PBUDAMTS   CLEAR BUD REC AREAS FOR FMT                  
         XC    PBUDST(12),PBUDST                                                
*                                                                               
         BAS   RE,TSTANY           IF DATA ENTERED                              
         LA    R2,BUDLN1H          LEAVE IT THERE                               
         BNZ   VK700               DON'T SEND MONTH LIST                        
*                                                                               
         BAS   RE,BLDMOS           NO DATA ENTERED                              
         BAS   RE,FMT              SEND MONTH LIST                              
         LA    R2,BUDLN1H                                                       
*                                                                               
VK700    DS    0H                                                               
         BAS   RE,GETBDNM     GET "$ CARRIED AT" (NET, GROSS OR COST)           
         MVC   BUDGN,WORK                                                       
VK700X   DS    0H                                                               
         FOUT  BUDGNH                                                           
*                                                                               
         CLC   SVDST,=C'ALL'       OUTPUT TOTALS BY REG AND/OR DST ??           
         BE    TOT                 YES                                          
*                                                                               
VK800    XC    KEY,KEY                                                          
         MVC   KEY(25),MYKEY       SET KEY                                      
*                                                                               
VK900    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
VKDERR   MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
*                                                                               
         LA    R2,BUDREGH                                                       
         CLC   BUDREG(3),=C'ALL'     MAKE SURE NOT COMING FROM TOT              
         BE    VR05                                                             
         LA    R2,BUDDSTH                                                       
         CLC   BUDDST(3),=C'ALL'                                                
         BNE   VR10                                                             
VR05     MVI   ERROR,ALLERR          ALL ONLY FOR REG OR DST TOTALS             
         B     TRAPERR                                                          
*                                                                               
VR10     DS    0H                                                               
         MVI   ACTELOPT,C'N'       SET FOR NO ACTIVITY ELEMS                    
*                                                                               
         MVC   SVKEY,KEY           SAVE THE RECORD KEY                          
         L     R6,AIO                                                           
         USING PBUDREC,R6                                                       
         CLI   ACTNUM,ACTADD       SEE IF ADDING                                
         BNE   VR500               MUST BE CHANGE                               
*                                                                               
*                                                                               
*            CHECK ADD RESTRICTIONS ON REG AND DST **********                   
*                                                                               
         MVI   ERROR,52            DUPLICATE                                    
         LA    R2,BUDMEDH                                                       
         MVC   KEY(25),SVKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    TRAPERR             RECORD ALREADY THERE (DUP)                   
         CLC   KEYSAVE+13(3),=3C'0'     TEST ADDING REG 0                       
         BNE   VR20                     NO                                      
         CLC   KEY(13),KEYSAVE          YES - CANNOT BE ANY OTHER REG           
         BNE   VR20                                                             
         LA    R2,BUDREGH                                                       
         MVI   ERROR,ADDERR2     REG 000 CAN'T BE ADDED WITH OTHER REGS         
         B     TRAPERR                                                          
*                                                                               
VR20     CLC   KEYSAVE+16(3),=3C'0'     TEST ADDING DIST 0                      
         LA    R2,BUDDSTH                                                       
         BNE   VR30                                                             
         CLC   KEY(16),KEYSAVE          YES - CANNOT BE ANY OTHER DIST          
         BNE   VR30                                                             
         MVI   ERROR,ADDERR3     DST 000 CAN'T BE ADDED WITH OTHER DSTS         
         B     TRAPERR                                                          
*                                                                               
VR30     CLC   KEYSAVE+16(3),=3C'0'     TEST ADDING DIST 0                      
         BE    VR40                                                             
         XC    KEY,KEY                  NO - CANNOT BE A DIST 0                 
         MVC   KEY(16),KEYSAVE                                                  
         MVC   KEY+16(3),=3C'0'                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VR40                                                             
         MVI   ERROR,ADDERR4     OTHER DSTS CAN'T BE ADDED WITH DST 000         
         B     TRAPERR                                                          
*                                                                               
VR40     CLC   KEYSAVE+13(3),=3C'0'     TEST ADDING REG 0                       
         BE    VR50                                                             
         XC    KEY,KEY                  NO - CANNOT BE A REG 0                  
         MVC   KEY(13),KEYSAVE                                                  
         MVC   KEY+13(3),=3C'0'                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(16),KEYSAVE     AGY,MED,CLT,PRD,EST,REG                      
         BNE   VR50                                                             
         MVI   ERROR,ADDERR5     OTHER REGS CAN'T BE ADDED WITH REG 000         
         B     TRAPERR                                                          
*                                                                               
VR50     DS    0H                                                               
*                                                                               
VR140    LA    RE,PBUDREC                                                       
         LA    RF,1000                                                          
         XCEF                                                                   
*                                                                               
*                                                                               
         BAS   RE,TSTAMT           TEST IF AMOUNTS ENTERED                      
         LA    R2,BUDLN1H                                                       
         CLI   ADDSW,1             AMOUNTS FOUND                                
         BE    VR200               GO TO OUTPUT RECORD                          
*                                                                               
         MVI   ERROR,NOCSHERR      AT LEAST ONE AMOUNT MUST BE ENTERED          
         B     TRAPERR                                                          
*                                                                               
VR200    DS    0H                                                               
         L     R6,AIO                                                           
         USING PBUDREC,R6                                                       
*                                       BUILD RECORD                            
         MVC   KEY(PBUDLEN-PBUDREC),SVKEY                                       
         MVC   PBUDREC(PBUDLEN-PBUDREC),KEY                                     
         MVC   PBUDLEN,=H'133'                                                  
         MVC   PBUDELEM(2),=X'0164'                                             
         MVC   PBUDST(12),SVESTST                                               
*                                                                               
         BAS   RE,BLDMOS                                                        
         BAS   RE,EDIT                                                          
*                                                                               
*****    B     EXIT                                                             
         B     VR900                                                            
         SPACE 3                                                                
*                                  CHANGE ROUTINES                              
VR500    DS    0H                                                               
*                                                                               
         MVC   PBUDST(12),SVESTST                                               
         BAS   RE,BLDMOS                                                        
         BAS   RE,EDIT                                                          
*                                                                               
VR900    DS    0H                  FALL THRU TO DISPLAY RECORD                  
*****    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       DS    0H                                                               
*                                                                               
         CLC   BUDREG(3),=C'ALL'   ALL MEANS TOTAL JUST EXECUTED                
         BE    EXIT                                                             
         CLC   BUDDST(3),=C'ALL'                                                
         BE    EXIT                                                             
         XC    BUDMSG,BUDMSG                                                    
         XC    BUDMSG2,BUDMSG2                                                  
         FOUT  BUDMSGH                                                          
         FOUT  BUDMSG2H                                                         
         L     R6,AIO                                                           
         USING PBUDREC,R6                                                       
         BAS   RE,BLDMOS                                                        
         BAS   RE,FMT                                                           
         XC    BMOLST,BMOLST                                                    
         XC    EMOLST,EMOLST                                                    
         CLC   PBUDST(12),SVESTST                                               
         BE    EXIT                                                             
         MVC   BUDMSG(L'PERWRN),PERWRN                                          
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DK       DS    0H                                                               
*                                                                               
         MVC   AIO1,AIO                                                         
         MVC   SAVEKEY,KEY         SAVE KEYS FOR LISTREC                        
         MVC   SAVEKEYS,KEYSAVE                                                 
         L     R6,AIO                                                           
         USING BUDHDRD,R6                                                       
         FOUT  BUDMEDH,PBUDKMED,1                                               
         FOUT  BUDCLTH,PBUDKCLT,3                                               
         FOUT  BUDPRDH,PBUDKPRD,3                                               
         FOUT  BUDESTH,PBUDKEST,3                                               
         FOUT  BUDREGH,PBUDKREG,3                                               
         FOUT  BUDDSTH,PBUDKDST,3                                               
         MVC   SVBUDKEY,BUDHDRD    SET FIELDS NEEDED FOR GET RTNS.              
*                                                                               
         LA    R2,BUDMEDH                                                       
         MVI   5(R2),1                                                          
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,BUDCLTH                                                       
         MVI   5(R2),3                                                          
         GOTO1 VALICLT                                                          
*                                  RESTORE KEY AND RECORD                       
         MVC   KEY(25),SAVEKEY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(25),SAVEKEY     SAME RECORD ?                                
         BE    *+6                 YES - OK                                     
         DC    H'0'                SOMETHING WRONG                              
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   AIO,AIO2                                                         
*NOP*    BAS   RE,GETCLT           GET CLIENT RECORD                            
         BAS   RE,GETPRD           GET PRODUCT RECORD                           
         XC    SVESTST(12),SVESTST   EST START & END DATES (YYMMDD)             
         BAS   RE,GETEST           GET ESTIMATE RECORD                          
*                                                                               
         OC    SVESTST,SVESTST      ANY DATA?                                   
         BZ    DKREG                NO                                          
         GOTO1 DATCON,DMCB,(0,SVESTST),(5,BUDESTD)                              
         GOTO1 DATCON,DMCB,(0,SVESTEND),(5,BUDESTD+11)                          
         MVI   BUDESTD+9,C'-'                                                   
*                                                                               
DKREG    BAS   RE,GETREG           GET REGION RECORD                            
         BAS   RE,GETDST           GET DISTRICT RECORD                          
         BAS   RE,GETBDNM     GET "$ CARRIED AT" (NET, GROSS OR COST)           
         MVC   BUDGN,WORK                                                       
*                                                                               
         FOUT  BUDMEDNH,MEDNM,10                                                
         FOUT  BUDCLTNH,CLTNM,20                                                
         FOUT  BUDPRDNH,PRDNM,20                                                
         FOUT  BUDESTNH,ESTNM,20                                                
         FOUT  BUDESTDH                                                         
         FOUT  BUDREGNH,REGNM,20                                                
         FOUT  BUDDSTNH,DSTNM,20                                                
         FOUT  BUDGNH                                                           
*                                                                               
DKX      DS    0H                  RESET AIO TO BUDGET REC                      
         MVC   AIO,AIO1            FOR DR (DISPLAY REC)                         
         MVC   KEY,SAVEKEY         RESTORE KEYS FOR LISTREC                     
         MVC   KEYSAVE,SAVEKEYS                                                 
*                          SEQ RESTORE NOT NEEDED                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* ACCUMULATE AND DISPLAY REGION OR DISTRICT TOTALS                              
*                                                                               
TOT      DS    0H                                                               
         L     R6,AIO                                                           
         USING BUDHDRD,R6                                                       
         XC    PBUDAMTS,PBUDAMTS                                                
         MVI   ERRSW,0                                                          
TOT2     BAS   RE,BLDMOS                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGENCY                                                    
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'18'                                                      
         MVC   KEY+4(3),SVCLT                                                   
         MVC   KEY+7(3),SVPRD                                                   
         MVC   KEY+10(3),SVEST                                                  
         CLC   SVREG,=C'ALL'                                                    
         BE    *+10                                                             
         MVC   KEY+13(3),SVREG                                                  
         CLC   SVDST,=C'ALL'                                                    
         BE    *+10                                                             
         MVC   KEY+16(3),SVDST                                                  
         GOTO1 HIGH                                                             
         B     TOT4C                                                            
TOT4     GOTO1 SEQ                                                              
TOT4C    CLC   KEY(13),KEYSAVE          A/M/C/P/E                               
         BNE   TOT20                                                            
         OC    KEYSAVE+13(3),KEYSAVE+13      REG                                
         BZ    TOT6                                                             
         CLC   KEY+13(3),KEYSAVE+13          REG                                
         BNE   TOT20                                                            
TOT6     OC    KEYSAVE+16(3),KEYSAVE+16      DST                                
         BZ    TOT8                                                             
         CLC   KEY+16(3),KEYSAVE+16          DST                                
         BE    TOT8                                                             
         CLC   KEY+13(3),KEYSAVE+13          REG                                
         BE    TOT20                                                            
*                                                                               
TOT8     DS    0H                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         CLC   PBUDST(12),SVESTST                                               
         BE    *+8                                                              
         OI    ERRSW,X'20'         PERIOD ERROR                                 
         XC    BMOLST,BMOLST                                                    
         BAS   RE,BLDMOS                                                        
*                                                                               
         OC    PBUDAMTS(48),PBUDAMTS                                            
         BNZ   TOT9                                                             
         OI    ERRSW,X'80'         NON-MONTHLY                                  
         MVC   FULL,PBUDAMTS+48                                                 
         L     R0,FULL      IN NON-MONTHLY USE TOTAL                            
         A     R0,AMTS+48                                                       
         ST    R0,AMTS+48                                                       
         B     TOT4                                                             
TOT9     DS    0H                                                               
         OI    ERRSW,X'40'         MONTHLY                                      
*                                                                               
         LA    R3,EMOLST                                                        
         LA    R4,AMTS                                                          
         LA    R5,BMOLST                                                        
         LA    R6,PBUDAMTS         R6 MUST BE RESET TO AIO I/P                  
*                                                                               
TOT10    CLC   0(2,R5),0(R3)       BUD DATE VS EST                              
         BL    TOT12                                                            
         BH    TOT13                                                            
*                                                                               
TOT11    CLI   0(R5),X'FF'                                                      
         BE    TOT4                GET NEW REC                                  
         CLI   0(R5),0                                                          
         BE    TOT11A                                                           
*                                                                               
         L     R0,0(R4)                                                         
         A     R0,0(R6)                                                         
         ST    R0,0(R4)                                                         
TOT11A   DS    0H                                                               
         LA    R3,2(R3)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,2(R5)                                                         
         LA    R6,4(R6)                                                         
         B     TOT10                                                            
*                                                                               
TOT12    LA    R5,2(R5)                                                         
         LA    R6,4(R6)                                                         
         B     TOT10                                                            
*                                                                               
TOT13    LA    R3,2(R3)                                                         
         LA    R4,4(R4)                                                         
         B     TOT10                                                            
*                                                                               
TOT20    LA    R3,12                                                            
         LA    R4,AMTS                                                          
         LA    R5,AMTS+48                                                       
TOT21    L     R0,0(R4)                                                         
         A     R0,0(R5)                                                         
         ST    R0,0(R5)                                                         
         LA    R4,4(R4)                                                         
         BCT   R3,TOT21                                                         
*                                                                               
         L     R6,AIO              RESET POINTER TO BUD REC                     
*                                                                               
         MVC   PBUDAMTS,AMTS                                                    
         BAS   RE,FMT                                                           
         XC    BUDMSG,BUDMSG                                                    
         FOUT  BUDMSGH                                                          
         TM    ERRSW,X'C0'         TEST BOTH TYPES                              
         BO    TOT22                                                            
         TM    ERRSW,X'20'                                                      
         BNZ   TOT23                                                            
         B     TOTXIT                                                           
*                                                                               
TOT22    XC    BUDMSG2,BUDMSG2                                                  
         MVC   BUDMSG2(L'TYPWRN),TYPWRN                                         
         FOUT  BUDMSG2H                                                         
*                                                                               
         TM    ERRSW,X'20'                                                      
         BZ    TOTXIT                                                           
*                                                                               
TOT23    DS    0H                                                               
         MVC   BUDMSG(L'PERWRN),PERWRN                                          
         DROP  R6                                                               
*                                                                               
TOTXIT   DS    0H             AS TOT WAS BRANCHED TO FROM VALKEY                
         XC    CONHEAD,CONHEAD     DISPLAY "TOTALS DISPLAYED, ETC." MSG         
         LA    R2,CONACTH                AND                                    
         MVC   CONHEAD(L'TOTMSG),TOTMSG                                         
         GOTO1 ERREX2              EXIT WITH MESSAGE IN PLACE                   
*                                                                               
         EJECT                                                                  
*                                                                               
* LIST RECORDS                                                                  
*                                                                               
LR       DS    0H                                                               
*                                                                               
         LA    R2,BUDREGH                                                       
         CLC   BUDREG(3),=C'ALL'     MAKE SURE NOT COMING FROM TOT              
         BE    LR002                                                            
         LA    R2,BUDDSTH                                                       
         CLC   BUDDST(3),=C'ALL'                                                
         BNE   LR005                                                            
LR002    MVI   ERROR,ALLERR          ALL ONLY FOR REG OR DST TOTALS             
         B     TRAPERR                                                          
*                                                                               
LR005    LA    R6,KEY                                                           
         USING PBUDREC,R6                                                       
         MVC   AIO,AIO1                                                         
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR030                KEY IS LAST RECORD READ                     
*                                   CHECK VS. KEYSAVE                           
*                                                                               
         MVC   KEY(31),MYKEY       MYKEY IS VALIDATED KEY                       
*                                                                               
LR010    GOTO1 HIGH                                                             
         B     LR030                                                            
*                                                                               
LR020    GOTO1 SEQ                                                              
*                                                                               
LR030    CLC   KEY(4),KEYSAVE     TEST FOR ALL DONE                             
         BNE   LR900                                                            
         CLI   KEY+3,X'18'         SEE IF BUDGET REC                            
         BNE   LR900                                                            
         OC    QCLT,QCLT         SEE IF CLIENT GIVEN                            
         BZ    LR032                                                            
         CLC   KEY+4(3),QCLT                                                    
         BNE   LR900                                                            
*                                                                               
LR032    OC    QPRD,QPRD         SEE IF PRODUCT GIVEN                           
         BZ    LR034                                                            
         CLC   KEY+7(3),QPRD                                                    
         BNE   LR900                                                            
*                                                                               
LR034    OC    QEST,QEST         SEE IF ESTIMATE GIVEN                          
         BZ    LR036                                                            
         CLC   KEY+10(3),QEST                                                   
         BNE   LR900                                                            
*                                                                               
LR036    OC    QREG,QREG         SEE IF REGION GIVEN                            
         BZ    LR040                                                            
         CLC   KEY+13(3),QREG                                                   
         BNE   LR900                                                            
*                                                                               
LR040    OC    QDST,QDST         SEE IF DISTRICT GIVEN                          
         BZ    LR050                                                            
         CLC   KEY+16(3),QDST                                                   
         BNE   LR900                                                            
*                                                                               
LR050    DS    0H                                                               
         GOTO1 GETREC              GET THE BUDGET REC                           
         L     R6,AIO                                                           
*                                                                               
         LA    R5,LISTAR           LIST AREA                                    
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         MVC   LISTAR,SPACES                                                    
         DROP  R8                                                               
         MVC   SELHED(38),=C'CLT  PRD  EST  REG  DST   BUDGET TOTAL'            
         MVC   0(3,R5),PBUDKCLT                                                 
         MVC   5(3,R5),PBUDKPRD                                                 
         MVC   10(3,R5),PBUDKEST                                                
         MVC   15(3,R5),PBUDKREG                                                
         MVC   20(3,R5),PBUDKDST                                                
         LA    R3,PBUDAMTS+48                                                   
         L     R0,0(R3)                                                         
         EDIT  (R0),(15,23(R5)),2,COMMAS=YES,FLOAT=-                            
DDISP    DS    0H                                                               
         FOUT  SELHEDH                                                          
*                                                                               
LR080    GOTO1 LISTMON                                                          
         B     LR020                                                            
*                                                                               
LR900    DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* PRINT BUDGET RECORD REPORT                                                    
*                                                                               
PR       DS    0H                                                               
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   KEY(31),MYKEY       MYKEY IS VALIDATED KEY                       
         MVC   KEYSAVE,KEY                                                      
         XC    SAVEKEY,SAVEKEY                                                  
         XC    SAVEKEYS,SAVEKEYS                                                
*                                                                               
         MVI   FIRSTT,0                       SET FOR FIRST READ                
         MVI   RECFOUND,0                                                       
*                                                                               
         GOTO1 HIGH                                                             
         B     PR30                                                             
*                                                                               
PR20     DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
*                                                                               
PR30     CLC   KEY(4),KEYSAVE     TEST FOR ALL DONE                             
         BNE   PR900                                                            
         CLI   KEY+3,X'18'         SEE IF BUD REC                               
         BNE   PR900                                                            
         OC    QCLT,QCLT         SEE IF CLIENT GIVEN                            
         BNZ   PR30B                                                            
         LA    R2,BUDCLTH          CLIENT IS REQUIRED                           
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
PR30B    CLC   KEY+4(3),QCLT                                                    
         BNE   PR900                                                            
*                                                                               
PR30C    OC    QPRD,QPRD         SEE IF PRODUCT GIVEN                           
         BZ    PR30D                                                            
         CLC   KEY+7(3),QPRD                                                    
         BNE   PR900                                                            
*                                                                               
PR30D    OC    QEST,QEST         SEE IF ESTIMATE GIVEN                          
         BZ    PR30E                                                            
         CLC   KEY+10(3),QEST                                                   
         BNE   PR900                                                            
*                                                                               
PR30E    OC    QREG,QREG         SEE IF REGION GIVEN                            
         BZ    PR30G                                                            
         CLC   KEY+13(3),QREG                                                   
         BNE   PR900                                                            
*                                                                               
PR30G    OC    QDST,QDST         SEE IF DISTRICT GIVEN                          
         BZ    PR30K                                                            
         CLC   KEY+16(3),QDST                                                   
         BNE   PR900                                                            
*                                                                               
PR30K    DS    0H                                                               
         CLI   FIRSTT,0            SEE IF FIRST TIME                            
         BE    PR30X                                                            
         B     PR31                                                             
*                                                                               
PR30X    MVI   FORCEHED,C'Y'                                                    
         MVC   KEYSAVE,KEY         FORCE EQUAL 1ST COMPARE                      
         MVC   SVBUDKEY,KEY        SET KEY FOR USE IN HOOK "GETS"               
*                                                                               
PR31     DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         USING PBUDREC,R6                                                       
         MVC   AMTS,PBUDAMTS       STORE AMTS AND START AND END DATES           
         MVC   BRECDTES,PBUDST     FOR USE IN HOOK AND PDST ROUTINES            
         DROP  R6                                                               
         MVI   RECFOUND,C'Y'       YES, FOUND BUDGET REC                        
*                                                                               
         CLI   FIRSTT,0            SEE IF FIRST TIME                            
         BNE   *+8                 NO                                           
         BAS   RE,HOOK             INITIALIZE FIELDS FOR 1ST REC                
*                                                                               
PR100    DS    0H                                                               
         CLC   KEY(10),KEYSAVE     AGY/MED/CLT/PRD                              
         BE    PR200                                                            
*                                                                               
*OUTPUT PRD, REG, AND EST TOTALS, SET FORCEHED, GO TO PR400                     
         BAS   RE,PREG             OUTPUT REG TOTALS                            
         BAS   RE,PEST             OUTPUT EST TOTALS                            
         BAS   RE,PPRD             OUTPUT PRD TOTALS                            
         MVI   FORCEHED,C'Y'                                                    
         MVC   SVBUDKEY,KEY        SET FIELDS FOR USE IN HOOK "GETS"            
         B     PR400               GO PROCESS BUDREC                            
*                                                                               
PR200    DS    0H                                                               
         CLC   KEY(13),KEYSAVE     AGY/MED/CLT/PRD/EST                          
         BE    PR300                                                            
*                                                                               
*OUTPUT REG TOTALS,OUTPUT EST TOTALS, SET FORCEHED, GO TO PR400                 
         BAS   RE,PREG             OUTPUT REG TOTALS                            
         BAS   RE,PEST             OUTPUT EST TOTALS                            
         MVI   FORCEHED,C'Y'                                                    
         MVC   SVBUDKEY,KEY        SET FIELDS FOR USE IN HOOK "GETS"            
         B     PR400               GO PROCESS BUDREC                            
*                                                                               
PR300    DS    0H                                                               
         CLC   KEY(16),KEYSAVE     AGY/MED/CLT/PRD/EST/REG                      
         BE    PR400                                                            
*                                                                               
*OUTPUT REG TOTALS                                                              
         BAS   RE,PREG             OUTPUT REG TOTALS                            
*                                                                               
PR400    DS    0H                  OUTPUT BUDREC LINE                           
         BAS   RE,PDST                                                          
         MVI   FIRSTT,1            TURN OFF FIRST TIME SW                       
         B     PR20                NEXT RECORD                                  
*                                                                               
*                                                                               
PR900    CLI   RECFOUND,C'Y'       REPORT HAS DATA IN IT                        
         BNE   PR900X                                                           
         BAS   RE,PREG             OUTPUT REG TOTALS                            
         BAS   RE,PEST             OUTPUT EST TOTALS                            
         BAS   RE,PPRD             OUTPUT PRD TOTALS                            
****                               OUTPUT CLT TOTALS                            
         MVC   P2(18),=C'*TOTAL FOR CLIENT '                                    
         MVC   P2+19(3),SVCLT                                                   
         MVI   P2+22,C'*'                                                       
******   LA    R2,P2+23            R2 SHOULD BE AT TOTAL COLUMN                 
         SR    R0,R0                                                            
         L     R1,CLTTOT           CLIENT TOTAL ACCUMULATOR                     
         D     R0,=F'100'                                                       
         EDIT  (R1),(10,(R2)),COMMAS=YES,FLOAT=-                                
         XC    CLTTOT,CLTTOT                                                    
         B     PRX                                                              
PR900X   MVC   P1(16),=C'NO RECORDS FOUND'                                      
PRX      GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         SPACE 3                                                                
*                                                                               
PPRD     NTR1                      OUTPUT PRODUCT TOALS                         
         MVC   P2(18),=C'*TOTAL FOR PRODUCT'                                    
         MVC   P2+19(3),SVPRD                                                   
         MVI   P2+22,C'*'                                                       
*****    LA    R2,P2+23         R2 SHOULD BE POINTING TO TOTAL COLUMN           
         SR    R0,R0                                                            
         L     R1,PRDTOT           PRODUCT TOTAL ACCUMULATOR                    
         D     R0,=F'100'                                                       
         EDIT  (R1),(10,(R2)),COMMAS=YES,FLOAT=-                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    PRDTOT,PRDTOT       CLEAR PRODUCT TOTALS                         
         B     EXIT                RETURN                                       
         SPACE 3                                                                
*                                                                               
PREG     NTR1                      OUTPUT REGION TOTALS                         
         MVC   P2(19),=C'*TOTAL FOR REGION  '                                   
         MVC   P2+20(3),SVREG                                                   
         LH    R3,MNTHCNT       NO OF MONTHS IN BUDGET + 1 FOR TOTALS           
         LA    R2,P2+25            PRINTLINE 2                                  
         LA    R4,TOTLS            REG TOTALS                                   
PREGLUP  SR    R0,R0                                                            
         L     R1,0(R4)                                                         
         D     R0,=F'100'          GET WHOLE DOLLARS                            
         CH    R3,=H'1'            DOING MONTH TOTALS ?                         
         BNE   PREG10              NO                                           
         EDIT  (R1),(10,(R2)),COMMAS=YES,FLOAT=-                                
         B     PREGPRT                                                          
PREG10   C     R1,=F'1000000'      MILLION OR OVER ?                            
         BL    PREG20              NO                                           
         EDIT  (R1),(8,(R2)),FLOAT=-                                            
         B     PREGUP                                                           
PREG20   EDIT  (R1),(8,(R2)),COMMAS=YES,FLOAT=-                                 
PREGUP   LA    R4,4(R4)                                                         
         LA    R2,8(R2)                                                         
         BCT   R3,PREGLUP                                                       
PREGPRT  XC    TOTLS(52),TOTLS     CLEAR REGION TOTALS                          
         MVI   ALLOWLIN,4                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                RETURN                                       
         SPACE 3                                                                
*                                                                               
PEST     NTR1                      OUTPUT ESTIMATE TOTALS                       
         MVC   P2(19),=C'*TOTAL FOR ESTIMATE'                                   
         MVC   P2+20(3),SVEST                                                   
         LH    R3,MNTHCNT       NO OF MONTHS IN BUDGET + 1 FOR TOTALS           
         LA    R2,P2+25            PRINTLINE 2                                  
         LA    R4,TOTLS+52         ESTIMATE TOTALS                              
PESTLUP  SR    R0,R0                                                            
         L     R1,0(R4)                                                         
         D     R0,=F'100'          GET WHOLE DOLLARS                            
         CH    R3,=H'1'            DOING MONTH TOTALS ?                         
         BNE   PEST10              NO                                           
         EDIT  (R1),(10,(R2)),COMMAS=YES,FLOAT=-                                
         B     PESTPRT                                                          
PEST10   C     R1,=F'1000000'      MILLION OR OVER ?                            
         BL    PEST20              NO                                           
         EDIT  (R1),(8,(R2)),FLOAT=-                                            
         B     PESTUP                                                           
PEST20   EDIT  (R1),(8,(R2)),COMMAS=YES,FLOAT=-                                 
PESTUP   LA    R4,4(R4)                                                         
         LA    R2,8(R2)                                                         
         BCT   R3,PESTLUP                                                       
PESTPRT  XC    TOTLS+52(52),TOTLS+52       CLEAR ESTIMATE TOTALS                
         MVI   ALLOWLIN,2                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
*****    B     EXIT                RETURN                                       
         XIT1  REGS=(R2)           LEAVE R2 POINTING TO TOTAL COLUMN            
         SPACE 3                                                                
*                                                                               
PDST     NTR1                      OUTPUT DISTRICT                              
*                                                                               
         MVC   SAVEKEY,KEY         SAVE KEYS FOR SEQ RESTORE                    
         MVC   SAVEKEYS,KEYSAVE                                                 
         MVC   SVBUDKEY,KEY        SET FIELDS NEEDED FOR GET RTNS.              
         MVC   AIO,AIO2                                                         
         CLC   SAVEKEY(13),SAVEKEYS       AGY/MED/CLT/PRD/EST                   
         BE    *+8            EQUAL?                                            
         BAS   RE,HOOK        NO - GET PRD REC FOR DIV CODE (SVDIV)             
*                                  AND RESET MONTH COUNTER                      
         CLI   FIRSTT,0            FIRST TIME ?                                 
         BE    PDST05              YES                                          
         CLC   SAVEKEY(13),SAVEKEYS       AGY/MED/CLT/PRD/EST/REG               
         BE    *+8            EQUAL? - SKIP GETREG                              
PDST05   BAS   RE,GETREG           GET REGION NAME                              
         BAS   RE,GETDST           GET DISTRICT NAME                            
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO AND KEYS                         
         MVC   KEY,SAVEKEY                                                      
         GOTO1 READ                RESTORE SEQ                                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   KEYSAVE,SAVEKEYS                                                 
         CLI   FIRSTT,0                                                         
         BE    PDST10                                                           
         CLC   KEY(16),KEYSAVE     AGY/MED/CLT/PRD/EST/REG                      
         BE    PDST20                                                           
PDST10   MVC   P2(3),=C'REG'                                                    
         MVC   P2+4(3),SVREG                                                    
         MVC   P2+8(20),REGNM                                                   
         MVI   ALLOWLIN,8                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
PDST20   MVC   P2(3),=C'DST'                                                    
         MVC   P2+4(3),SVDST                                                    
         MVC   P2+8(18),DSTNM                                                   
*                                                                               
         LH    R6,MNTHCNT       NO OF MONTHS IN BUDGET + 1 FOR TOTALS           
         LA    R2,P2+25            PRINTLINE 2                                  
         LA    R3,AMTS                                                          
         LA    R4,TOTLS                                                         
PDSTLUP  DS    0H                                                               
         CH    R6,=H'1'            FINISHED WITH MONTHS ?                       
         BNE   *+10                NO                                           
         MVC   0(4,R3),AMTS+48        SET FOR TOTALS OUTPUT                     
         L     R1,0(R4)            REG TOTALS                                   
         A     R1,0(R3)            ADD BUDREC AMOUNT                            
         ST    R1,0(R4)                                                         
         L     R1,52(R4)           EST TOTALS                                   
         A     R1,0(R3)            ADD BUDREC AMOUNT                            
         ST    R1,52(R4)                                                        
         CH    R6,=H'1'            FINISHED WITH MONTHS ?                       
         BNE   PDSTEDT              ONLY ADD TOTAL BUDREC AMOUNT BELOW          
         L     R1,PRDTOT           PRD TOTALS                                   
         A     R1,0(R3)            ADD BUDREC AMOUNT                            
         ST    R1,PRDTOT                                                        
         L     R1,CLTTOT           CLT TOTALS                                   
         A     R1,0(R3)            ADD BUDREC AMOUNT                            
         ST    R1,CLTTOT                                                        
PDSTEDT  SR    R0,R0                                                            
         L     R1,0(R3)                                                         
         D     R0,=F'100'          GET WHOLE DOLLARS                            
         CH    R6,=H'1'            FINISHED WITH MONTHS ?                       
         BNE   PDSTED10            NO                                           
         EDIT  (R1),(10,(R2)),COMMAS=YES,FLOAT=-                                
         B     PDSTPRT             LINE IS FINISHED                             
PDSTED10 C     R1,=F'1000000'      MILLION OR OVER ?                            
         BL    PDSTED20            NO                                           
         EDIT  (R1),(8,(R2)),FLOAT=-                                            
         B     PDSTEDUP                                                         
PDSTED20 EDIT  (R1),(8,(R2)),COMMAS=YES,FLOAT=-                                 
PDSTEDUP LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         LA    R2,8(R2)                                                         
         BCT   R6,PDSTLUP                                                       
PDSTPRT  MVI   ALLOWLIN,0                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                RETURN                                       
         EJECT                                                                  
*                                                                               
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
         CLI   RECFOUND,C'Y'       DOES REPORT HAVE DATA IN IT ?                
         BNE   HOOKX               NO                                           
*                                                                               
         MVC   AIO1,AIO                                                         
         MVC   SAVEKEY,KEY         SAVE KEYS FOR BUD REC                        
         MVC   KEYSAVE,SAVEKEYS                                                 
         MVC   AIO,AIO2                                                         
         CLC   KEY(7),KEYSAVE      AGY/MED/CLT                                  
         BNE   HKCLT                                                            
         CLC   KEY(10),KEYSAVE     AGY/MED/CLT/PRD                              
         BNE   HKPRD                                                            
         CLC   KEY(13),KEYSAVE     AGY/MED/CLT/PRD/EST                          
         BNE   HKEST                                                            
         MVC   AIO,AIO1            RESET AIO FOR NEXT GETREC                    
         B     HKREST              CLT,PRD,EST ARE UNCHANGED                    
*                                                                               
HKCLT    BAS   RE,GETCLT           GET CLIENT RECORD                            
HKPRD    BAS   RE,GETPRD           GET PRODUCT RECORD                           
HKEST    XC    SVESTST(12),SVESTST      ESTIMATE START AND END DATES            
         BAS   RE,GETEST           GET ESTIMATE RECORD                          
*                                                                               
         MVC   AIO,AIO1            RESET AIO FOR NEXT GETREC                    
         MVC   KEY,SAVEKEY         RESTORE KEY                                  
         GOTO1 READ                RESTORE BUDGET SEQ                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
HKREST   CLC   ESTNM(9),=C'NOT FOUND'   ESTIMATE RECORD FOUND?                  
         BE    HKMOVE               NO                                          
         OC    SVESTST,SVESTST      ANY DATA?                                   
         BZ    HKMOVE               NO                                          
         GOTO1 DATCON,DMCB,(0,SVESTST),(5,H6+15)                                
         GOTO1 DATCON,DMCB,(0,SVESTEND),(5,H6+26)                               
         MVI   H6+24,C'-'                                                       
*                                                                               
HKMOVE   DS    0H                                                               
*                                                                               
         MVC   H1(5),=C'MEDIA'                                                  
         MVC   H1+10(1),QMED                                                    
         MVC   H1+15(10),MEDNM                                                  
         MVC   H3(6),=C'CLIENT'                                                 
         MVC   H3+10(3),SVCLT                                                   
         MVC   H3+15(20),CLTNM                                                  
         MVC   H4(7),=C'PRODUCT'                                                
         MVC   H4+10(3),SVPRD                                                   
         MVC   H4+15(20),PRDNM                                                  
         MVC   H5(8),=C'ESTIMATE'                                               
         MVC   H5+10(3),SVEST                                                   
         MVC   H5+15(20),ESTNM                                                  
         MVC   H5+60(07),=C'DOLLARS'                                            
         MVC   H6+59(08),=C'--------'                                           
         MVC   H5+56(03),=C'NET'                                                
         MVC   H6+56(03),=C'---'                                                
         CLI   SVCPROFL+14,C'N'                                                 
         BE    HFMT                                                             
         MVC   H5+54(05),=C'GROSS'                                              
         MVC   H6+54(05),=C'-----'                                              
         CLI   SVCPROFL+14,C'G'                                                 
         BE    HFMT                                                             
         MVC   H5+54(05),=C' COST'                                              
         MVC   H6+54(05),=C' ----'                                              
*                                                                               
****                         FORMAT MONTHS OF BUDGET HEADER LINE                
HFMT     DS    0H                                                               
         BAS   RE,PBLDMOS     BUILD BUDGET MONTH TABLE (EMOLST)                 
         LA    R4,EMOLST                                                        
         LA    R5,H9+27            START OF "MONTHS" LINE                       
         LA    R3,H10+27           FOR UNDERSCORES                              
         SR    R6,R6               SET FOR MONTH COUNT                          
*                                                                               
HFMT2    CLI   0(R4),X'FF'         END OF MONTH LIST                            
         BE    HFMTX                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,(R4)),(9,(R5))                                    
         MVC   0(6,R3),=C'------'                                               
*                                                                               
HFMT4    LA    R4,2(R4)                                                         
         LA    R5,8(R5)                                                         
         LA    R3,8(R3)                                                         
         LA    R6,1(R6)            BUMP UP MONTH COUNT                          
         B     HFMT2                                                            
*                                                                               
HFMTX    DS    0H                                                               
         MVC   0(8,R5),=C'**TOTAL*'                                             
         MVC   0(8,R3),=C'--------'                                             
         LA    R6,1(R6)            ONE MORE FOR BUDGET TOTALS                   
         STH   R6,MNTHCNT          SAVE MONTH COUNT                             
*                                                                               
HOOKX    B     EXIT                                                             
RECFOUND DC    X'0'                                                             
         SPACE 5                                                                
*                                                                               
HEDSPECS SSPEC H1,50,C'PRINT BUDGET RECORDS LIST'                               
         SSPEC H2,50,C'-------------------------'                               
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H6,95,REQUESTOR                                                  
         SSPEC H6,121,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
         DROP  R8                                                               
*                                                                               
PBLDMOS  DS    0H           BUILD ESTIMATE MONTH TABLE (EMOLST)                 
         ST    RE,SAVERE           (FOR REPORT ONLY)                            
         LA    R4,EMOLST                                                        
         LA    R5,SVESTST                                                       
         XC    0(28,R4),0(R4)           CLEAR                                   
         GOTO1 DATCON,DMCB,(0,(R5)),(3,WORK)                                    
*                                                                               
         GOTO1 DATCON,(R1),(0,6(R5)),(3,WORK+3)                                 
*                                                                               
         MVC   0(2,R4),WORK                                                     
         MVC   24(2,R4),=2X'FF'                                                 
PBLDMOS2 CLI   1(R4),12                                                         
         BE    PBLDMOS3                                                         
         IC    R1,1(R4)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,3(R4)                                                         
         MVC   2(1,R4),0(R4)                                                    
         B     PBLDMOS4                                                         
*                                                                               
PBLDMOS3 IC    R1,0(R4)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,2(R4)                                                         
         MVI   3(R4),1                                                          
*                                                                               
PBLDMOS4 LA    R4,2(R4)                                                         
         CLC   0(2,R4),WORK+3                                                   
         BL    PBLDMOS2                                                         
         MVI   2(R4),X'FF'         END OF MONTH LIST                            
*                                  RETURN                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
BLDMOS   DS    0H          BUILD ESTIMATE AND/OR BUDGET MONTH TABLES            
         L     R6,AIO              (EMOLST AND BMOLST)                          
         USING PBUDREC,R6                                                       
         ST    RE,SAVERE                                                        
         LA    R4,EMOLST                                                        
         LA    R5,SVESTST                                                       
BLDMOS1  CLI   0(R4),0                                                          
         BNE   BLDMOS6                  ALREADY BUILT                           
         CLI   0(R5),0                  TEST NEEDED                             
         BE    BLDMOS6                                                          
         XC    0(28,R4),0(R4)           CLEAR                                   
         GOTO1 DATCON,DMCB,(0,(R5)),(3,WORK)                                    
*                                                                               
         GOTO1 DATCON,(R1),(0,6(R5)),(3,WORK+3)                                 
*                                                                               
         MVC   0(2,R4),WORK                                                     
         MVC   24(2,R4),=2X'FF'                                                 
BLDMOS2  CLI   1(R4),12                                                         
         BE    BLDMOS3                                                          
         IC    R1,1(R4)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,3(R4)                                                         
         MVC   2(1,R4),0(R4)                                                    
         B     BLDMOS4                                                          
*                                                                               
BLDMOS3  IC    R1,0(R4)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,2(R4)                                                         
         MVI   3(R4),1                                                          
*                                                                               
BLDMOS4  LA    R4,2(R4)                                                         
         CLC   0(2,R4),WORK+3                                                   
         BL    BLDMOS2                                                          
*                                                                               
BLDMOS6  LA    R0,SVESTST                                                       
         CR    R5,R0                                                            
         BNE   BLDMOSX                                                          
         LA    R4,BMOLST                                                        
         LA    R5,PBUDST                                                        
         B     BLDMOS1                                                          
*                                                                               
BLDMOSX  L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R6                                                               
         SPACE 3                                                                
*                                                                               
TOTMSG   DC    C'** TOTALS DISPLAYED - ENTER NEXT REQUEST **'                   
*                                                                               
*                                                                               
PERWRN   DC    C'** WARNING - EST. DATES CHANGED SINCE LAST ACCESS **'          
TYPWRN   DC    C'** WARNING - MONTHLY AND NON-MONTHLY BUDGETS COMBINED'         
         EJECT                                                                  
*                                  FORMAT SCREEN                                
FMT      DS    0H                                                               
         L     R6,AIO                                                           
         USING PBUDREC,R6                                                       
         ST    RE,SAVERE                                                        
         LA    R4,BMOLST                                                        
         CLC   SVDST,=C'ALL'       DOING TOTALS?                                
         BE    FMT1                YES                                          
         CLI   ACTNUM,ACTDIS                                                    
         BE    FMT1A                                                            
         CLI   ACTNUM,ACTSEL                                                    
         BE    FMT1A                                                            
FMT1     LA    R4,EMOLST                                                        
FMT1A    LA    R5,PBUDAMTS                                                      
         LA    R2,BUDLN1H                                                       
*                                                                               
FMT2     XC    WORK2(6),WORK2                                                   
         CLI   0(R4),0                                                          
         BE    FMT4                                                             
         MVC   WORK2(6),=C'TOTAL*'                                              
         CLI   0(R4),X'FF'                                                      
         BE    FMT4                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(3,(R4)),(9,WORK2)                                   
*                                                                               
FMT4     CLC   8(6,R2),WORK2                                                    
         BE    FMT6                                                             
         MVC   8(6,R2),WORK2                                                    
         FOUT  (R2)                                                             
*                                                                               
FMT6     BAS   RE,BUMPFLD                                                       
         XC    WORK2(15),WORK2                                                  
         OC    0(4,R5),0(R5)                                                    
         BZ    FMT8                                                             
*                                                                               
         L     R0,0(R5)                                                         
         EDIT  (R0),(15,WORK2),2,COMMAS=YES,FLOAT=-,ALIGN=LEFT                  
*                                                                               
FMT8     CLC   8(15,R2),WORK2                                                   
         BE    FMT9                                                             
         MVC   8(15,R2),WORK2                                                   
         FOUT  (R2)                                                             
FMT9     BAS   RE,BUMPFLD                                                       
         CLI   0(R4),X'FF'                                                      
         BE    FMTX                                                             
         LA    R4,2(R4)                                                         
         LA    R5,4(R5)                                                         
         B     FMT2                                                             
*                                                                               
FMTX     L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
         DROP  R6                                                               
*                                  EDIT AMOUNTS AND MONTHS                      
EDIT     DS    0H                                                               
         MVI   ENDSW,0             END-OF-INPUT INDICATOR                       
         ST    RE,SAVERE                                                        
         L     R6,AIO                                                           
         USING BUDHDRD,R6                                                       
         XC    PBUDAMTS,PBUDAMTS                                                
         LA    R2,BUDLN1H                                                       
EDIT2    CLI   5(R2),0                                                          
         BE    EDIT9                                                            
         LA    R4,PBUDAMTS+48                                                   
         CLC   8(3,R2),=C'TOT'                                                  
*****    BE    EDIT6                                                            
         BE    EDIT5X                                                           
         GOTO1 DATVAL,DMCB,(2,8(R2)),WORK                                       
*                                                                               
         CLI   DMCB+3,0                                                         
         BNE   *+12                                                             
         MVI   ERROR,INVDATE       INVALID DATE EXPRESSION                      
         B     TRAPERR                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK)                                    
         LA    R4,EMOLST                                                        
EDIT4    CLC   WORK(2),0(R4)                                                    
         BE    EDIT5                                                            
         LA    R4,2(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   EDIT4                                                            
         MVI   ERROR,OUTOFEST      DATE OUTSIDE OF EST RANGE                    
         B     TRAPERR                                                          
*                                                                               
EDIT5    LA    R0,EMOLST                                                        
         SR    R4,R0                                                            
         SLL   R4,1                X 2                                          
         LA    R4,PBUDAMTS(R4)                                                  
         B     EDIT6                                                            
*                                                                               
EDIT5X   MVI   ENDSW,1             INDICATE END-OF-INPUT                        
*                                                                               
EDIT6    BAS   RE,BUMPFLD                                                       
         MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         XC    DMCB(24),DMCB                                                    
         CLI   5(R2),0                                                          
         BE    EDIT7                                                            
         IC    R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R0)                                          
*                                                                               
         CLI   DMCB,0                                                           
         BNE   TRAPERR                                                          
*                                                                               
EDIT7    DS    0H                                                               
         OC    0(4,R4),0(R4)                                                    
         BZ    EDIT7B                                                           
         CLI   ENDSW,1             END-OF-INPUT ?                               
         BE    EDIT7B              YES                                          
         MVI   ERROR,DUPMNTH       MULTIPLE INPUTS FOR SAME MONTH               
         B     TRAPERR                                                          
EDIT7B   MVC   0(4,R4),DMCB+4                                                   
         CLI   ENDSW,1             END-OF-INPUT ?                               
         BE    EDIT10              YES                                          
EDIT8    BAS   RE,BUMPFLD                                                       
         CLI   0(R2),10            2-BYTE FIELD MEANS END OF SCREEN             
         BNE   EDIT2                                                            
         B     EDIT10                                                           
EDIT9    BAS   RE,BUMPFLD                                                       
         CLI   5(R2),0                                                          
         BE    EDIT8                                                            
         MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     TRAPERR                                                          
*                                                                               
EDIT10   DS    0H                                                               
EDIT11   OC    PBUDAMTS+48(4),PBUDAMTS+48    TEST TOTAL INPUT                   
         BNZ   EDIT13                        YES                                
         LA    R2,BUDLN13H                   NO - SEND IT                       
         MVC   8(6,R2),=C'TOTAL*'                                               
         FOUT  (R2)                                                             
         BAS   RE,BUMPFLD                                                       
         LA    R0,12                                                            
         LA    R4,PBUDAMTS                                                      
         LA    R5,PBUDAMTS+48                                                   
EDIT12   L     R1,0(R4)                                                         
         A     R1,0(R5)                                                         
         ST    R1,0(R5)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,EDIT12                                                        
*                                                                               
         L     R0,0(R5)                                                         
         EDIT  (R0),(15,8(R2)),2,COMMAS=YES,FLOAT=-,ALIGN=LEFT                  
*                                                                               
         FOUT  (R2)                                                             
         B     EDITX                                                            
*                                       TOTAL WAS INPUT                         
EDIT13   OC    PBUDAMTS(48),PBUDAMTS    WAS ANYTHING ELSE                       
         BZ    EDITX                    NO                                      
*                                       YES - CHECK TOTAL                       
         LA    R0,12                                                            
         LA    R4,PBUDAMTS                                                      
         XC    WORK(4),WORK                                                     
         LA    R5,WORK                                                          
EDIT14   L     R1,0(R4)                                                         
         A     R1,0(R5)                                                         
         ST    R1,0(R5)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,EDIT14                                                        
*                                                                               
         CLC   WORK(4),PBUDAMTS+48                                              
         BE    EDITX                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TOTERM),TOTERM                                         
         LA    R4,CONHEAD+L'TOTERM+2                                            
         L     R0,WORK                                                          
         EDIT  (R0),(15,(R4)),2,COMMAS=YES,ALIGN=LEFT                           
*                                                                               
         LA    R2,BUDLN1H                                                       
         GOTO1 ERREX2                                                           
         B     EXIT                                                             
         SPACE 2                                                                
EDITX    L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R6                                                               
*                                                                               
TOTERM   DC    C'**ERROR - TOTAL NOT = SUM, SHOULD BE'                          
*                                                                               
         EJECT                                                                  
*                                                                               
         SPACE 3                                                                
BUMPFLD  SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 3                                                                
TSTANY   DS    0H                                                               
         LA    R2,BUDLN1H                                                       
TSTANY2  CLI   5(R2),0                                                          
         BNER  RE                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),10            END OF SCREEN                                
         BER   RE                                                               
         B     TSTANY2                                                          
         SPACE 3                                                                
TSTAMT   DS    0H                                                               
         LA    R2,BUDLN1H+14       AMOUNTS HEADER                               
         LA    R5,BUDDUMH          END OF SCREEN (INPUT)                        
         MVI   ADDSW,0                                                          
TSTAMT2  CLI   5(R2),0                                                          
         BNE   TSTAMTX             AMOUNTS FOUND                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               MONTH HEADER                                 
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               AMOUNTS HEADER                               
         CR    R2,R5               END OF SCREEN?                               
         BHR   RE                  YES - NOTHING FOUND                          
         B     TSTAMT2                                                          
TSTAMTX  DS    0H                                                               
         MVI   ADDSW,1             AMOUNT(S) FOUND                              
         BR    RE                                                               
         EJECT                                                                  
CLRNAME  XC    BUDMEDN,BUDMEDN                                                  
         XC    BUDCLTN,BUDCLTN                                                  
         XC    BUDPRDN,BUDPRDN                                                  
         XC    BUDESTN,BUDESTN                                                  
         XC    BUDESTD,BUDESTD                                                  
         XC    BUDREGN,BUDREGN                                                  
         XC    BUDDSTN,BUDDSTN                                                  
         CLI   ACTNUM,ACTLIST                                                   
         BE    CLRNAME1                                                         
         XC    BUDMSG,BUDMSG                                                    
         XC    BUDMSG2,BUDMSG2                                                  
         FOUT  BUDMSGH                                                          
         FOUT  BUDMSG2H                                                         
CLRNAME1 FOUT  BUDMEDNH                                                         
         FOUT  BUDCLTNH                                                         
         FOUT  BUDPRDNH                                                         
         FOUT  BUDESTNH                                                         
         FOUT  BUDESTDH                                                         
         FOUT  BUDREGNH                                                         
         FOUT  BUDDSTNH                                                         
         BR    RE                  RETURN                                       
*                                                                               
CLRSAVE  XC    KEY,KEY                                                          
         XC    SVESTST,SVESTST                                                  
         XC    SVESTEND,SVESTEND                                                
         XC    SVCPROFL,SVCPROFL                                                
         XC    AMTS,AMTS                                                        
         XC    EMOLST,EMOLST                                                    
         XC    BMOLST,BMOLST                                                    
         BR    RE                  RETURN                                       
*                                                                               
GETBDNM  DS    0H                  GET "$ CARRIED AT" TYPE                      
         XC    WORK,WORK                                                        
         MVC   WORK(07),=C'**NET**'                                             
         CLI   SVCPROFL+14,C'N'                                                 
         BER   RE                  RETURN                                       
         MVC   WORK(09),=C'**GROSS**'                                           
         CLI   SVCPROFL+14,C'G'                                                 
         BER   RE                  RETURN                                       
         MVC   WORK(09),=C'**COST** '                                           
         BR    RE                  RETURN                                       
*                                                                               
         EJECT                                                                  
*                                                                               
* ROUTINES FOR READING THE PRTRECS FOR THE SIX KEY FIELDS USED                  
*                                                                               
GETCLT   DS    0H                  GET CLIENT RECORD                            
         ST    RE,SAVERE                                                        
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CLTHDRD,R6                                                       
         MVC   PCLTKAGY,AGENCY                                                  
         MVC   PCLTKMED,SVMED                                                   
         MVI   PCLTKRCD,X'02'                                                   
         MVC   PCLTKCLT,SVCLT                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         XC    SVCPROFL,SVCPROFL                                                
         XC    CLTNM,CLTNM                                                      
         MVC   CLTNM(9),=C'NOT FOUND'                                           
         CLC   KEY(25),KEYSAVE                                                  
         BNE   GETCLTX                                                          
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVC   CLTNM,PCLTNAME                                                   
         MVC   SVCPROFL,PCLTPROF                                                
*                                                                               
GETCLTX  L     RE,SAVERE                                                        
         BR    RE                  RETURN                                       
*                                                                               
GETPRD   DS    0H                  GET PRODUCT RECORD                           
         ST    RE,SAVERE                                                        
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PRDHDRD,R6                                                       
         MVC   PPRDKAGY,AGENCY                                                  
         MVC   PPRDKMED,SVMED                                                   
         MVI   PPRDKRCD,X'06'                                                   
         MVC   PPRDKCLT,SVCLT                                                   
         MVC   PPRDKPRD,SVPRD                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         XC    PRDNM,PRDNM                                                      
         MVC   PRDNM(9),=C'NOT FOUND'                                           
         CLC   KEY(25),KEYSAVE                                                  
         BNE   GETPRDX                                                          
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVC   PRDNM,PPRDNAME                                                   
         MVC   SVDIV,PPRDDIV       DIVISION CODE                                
*                                                                               
GETPRDX  L     RE,SAVERE                                                        
         BR    RE                  RETURN                                       
*                                                                               
GETEST   DS    0H                  GET ESTIMATE RECORD                          
         ST    RE,SAVERE                                                        
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING ESTHDRD,R6                                                       
         MVC   PESTKAGY,AGENCY                                                  
         MVC   PESTKMED,SVMED                                                   
         MVI   PESTKRCD,X'07'                                                   
         MVC   PESTKCLT,SVCLT                                                   
         MVC   PESTKPRD,SVPRD                                                   
         PACK  DUB,SVEST           SVEST IS CHARACTER NUMERIC                   
         CVB   R0,DUB                                                           
         STH   R0,BEST             BEST IS BINARY                               
         MVC   PESTKEST,BEST                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         XC    ESTNM,ESTNM                                                      
         XC    BUDESTD,BUDESTD                                                  
         MVC   ESTNM(9),=C'NOT FOUND'                                           
         MVC   SVESTST,BRECDTES       ESTIMATE START DATE (YYMMDD)              
         MVC   SVESTEND,BRECDTES+6    ESTIMATE END   DATE (YYMMDD)              
         CLC   KEY(25),KEYSAVE                                                  
         BNE   GETESTX                                                          
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVC   ESTNM,PESTNAME                                                   
         MVC   SVESTST,PESTST       ESTIMATE START DATE (YYMMDD)                
         MVC   SVESTEND,PESTEND     ESTIMATE END   DATE (YYMMDD)                
*                                                                               
GETESTX  L     RE,SAVERE                                                        
         BR    RE                  RETURN                                       
*                                                                               
GETREG   DS    0H                  GET REGION RECORD                            
         ST    RE,SAVERE                                                        
         XC    REGNM,REGNM                                                      
         CLC   SVREG,=3C'0'        NO 000 REGION RECORD                         
         BE    GETREGX             RETURN                                       
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING REGHDRD,R6                                                       
         MVC   PREGKAGY,AGENCY                                                  
         MVC   PREGKMED,SVMED                                                   
         MVI   PREGKRCD,X'04'                                                   
         MVC   PREGKCLT,SVCLT                                                   
         MVC   PREGKDIV,SVDIV                                                   
         MVC   PREGKREG,SVREG                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         MVC   REGNM(9),=C'NOT FOUND'                                           
         CLC   KEY(25),KEYSAVE                                                  
         BNE   GETREGX             RETURN                                       
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVC   REGNM,PREGNAME                                                   
*                                                                               
GETREGX  L     RE,SAVERE                                                        
         BR    RE                  RETURN                                       
*                                                                               
GETDST   DS    0H                  GET DISTRICT RECORD                          
         ST    RE,SAVERE                                                        
         XC    DSTNM,DSTNM                                                      
         CLC   SVDST,=3C'0'        NO 000 REGION RECORD                         
         BE    GETDSTX             RETURN                                       
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DSTHDRD,R6                                                       
         MVC   PDSTKAGY,AGENCY                                                  
         MVC   PDSTKMED,SVMED                                                   
         MVI   PDSTKRCD,X'05'                                                   
         MVC   PDSTKCLT,SVCLT                                                   
         MVC   PDSTKDIV,SVDIV                                                   
         MVC   PDSTKREG,SVREG                                                   
         MVC   PDSTKDST,SVDST                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         MVC   DSTNM(9),=C'NOT FOUND'                                           
         CLC   KEY(25),KEYSAVE                                                  
         BNE   GETDSTX             RETURN                                       
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVC   DSTNM,PDSTNAME                                                   
*                                                                               
GETDSTX  L     RE,SAVERE                                                        
         BR    RE                  RETURN                                       
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMC1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMD1D                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
SVAREA   DS    0H                                                               
FIRSTT   DS    X                 FIRST TIME FOR REPORT                          
SVCOMP   DS    X                                                                
SVBUDKEY DS    0CL25                                                            
SVAGY    DS    CL2                                                              
SVMED    DS    CL1                                                              
SVRCD    DS    X                                                                
SVCLT    DS    CL3                                                              
SVPRD    DS    CL3                                                              
SVEST    DS    CL3                                                              
SVREG    DS    CL3                                                              
SVDST    DS    CL3                                                              
         DS    CL6                                                              
SVDIV    DS    CL3                                                              
SVESTST  DS    CL6                 ESTIMATE START DATE (YYMMDD)                 
SVESTEND DS    CL6                 ESTIMATE END   DATE (YYMMDD)                 
BRECDTES DS    CL12      FOR USE IN PR (REPORT) IF EST REC NOT FOUND            
SVCPROFL DS    CL32                CLIENT PROFILE                               
WORK2    DS    CL64                                                             
MNTHCNT  DS    H                   NUMBER OF MONTHS IN BUDGET                   
SAVERE   DS    F                                                                
AMTS     DS    CL52                4 X 12 MO'S. + 4 BYTES FOR TOTALS            
TOTLS    DS    2CL52                 SAME AS AMTS - REG/EST TOTALS              
PRDTOT   DS    F                   PRODUCT GRAND TOTAL                          
CLTTOT   DS    F                   CLIENT  GRAND TOTAL                          
EMOLST   DS    CL28                BIN YM X 12 + 4 BYTES "EXTRA"                
BMOLST   DS    CL28                     SAME AS EMOLST                          
NEWCLT   DS    X                                                                
ERRSW    DS    X                                                                
ENDSW    DS    X                                                                
ADDSW    DS    X                                                                
MYKEY    DS    CL31                                                             
SAVEKEY  DS    CL31                                                             
SAVEKEYS DS    CL31               KEYSAVE                                       
X        DS    XL100                                                            
*                                                                               
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE PPRDREC                                                        
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE PESTREC                                                        
         EJECT                                                                  
DIVHDRD  DSECT                                                                  
       ++INCLUDE PDIVREC                                                        
         EJECT                                                                  
REGHDRD  DSECT                                                                  
       ++INCLUDE PREGREC                                                        
         EJECT                                                                  
DSTHDRD  DSECT                                                                  
       ++INCLUDE PDSTREC                                                        
         EJECT                                                                  
BUDHDRD  DSECT                                                                  
       ++INCLUDE PBUDREC                                                        
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028PRSFM12   11/06/00'                                      
         END                                                                    
