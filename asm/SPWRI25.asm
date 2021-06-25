*          DATA SET SPWRI25    AT LEVEL 003 AS OF 02/05/02                      
*PHASE T20425A,*                                                                
*                                                                               
*********************************************************************           
*                                                                   *           
*                                                                   *           
************* MODULE NOT IN USE - LAST UPDATED: *********************           
*          DATA SET SPWRI28    AT LEVEL 021 AS OF 08/18/98          *           
*********************************************************************           
*                                                                   *           
*                                                                   *           
*          SPWRI25 (T20425) - AGENCY SUMMARY (PW)                   *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 11AUG98 02 NRK -- REPLACE VALIPER WITH PERVAL CALL.               *           
* 19MAY98 01 EFJ -- INITIAL DEVELOPMENT                             *           
*                                                                   *           
*********************************************************************           
         TITLE 'T20425 - SPOTPAK WRITER AGENCY SUMMARY (PW)'                    
T20425   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,T20425**,RA,RR=R2                                          
         LR    R5,RC                                                            
         USING WORKD,R5                                                         
         ST    R2,RELO                                                          
         MVC   SAVERD,4(RD)        SAVE A(CALLING PROGRAM'S SAVE AREA)          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         ST    RC,AGEND                                                         
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                  REPORT CALLING MODE                          
         CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPVAL        VALIDATION                                   
         BE    VALID                                                            
         CLI   RPMODE,RPINPUT      INPUT                                        
         BE    INPUT                                                            
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         B     XIT                                                              
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
NEQXIT   LTR   RB,RB                                                            
XIT      XIT1  ,                                                                
         EJECT                                                                  
* INITIALIZATION                                                                
*                                                                               
INIT     DS    0H                                                               
         OI    SBQSKIP,SBQSKBUY+SBQSKGL+SBQSKBIL                                
         OI    SBQREAD,SBQRDBH                                                  
         LR    RF,R9                                                            
         AH    RF,=Y(SBQREAD2-SYSD)                                             
         OI    0(RF),SBQRD2PW                                                   
         OI    SBQPER,SBQPMN                                                    
         MVI   SBQPERLO,1                                                       
         MVI   SBQPERHI,X'FF'                                                   
         LA    RF,MYAMON                                                        
         ST    RF,AMONTHS                                                       
         OI    SBQPER,SBQPBIG      EXTENDED PERIOD TABLES                       
*                                                                               
         CLI   RPTSCRN,0           TEST USER REPORT SCREEN                      
         BE    INITX                NO                                          
*                                                                               
         MVI   MYFIRSTH,12                                                      
         MVI   WIDTHOPT,C'W'       WIDE PRINTING (165)                          
*                                                                               
         ZAP   CLTORDT,=PL8'0'                                                  
*                                                                               
INITX    B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
* FURTHER REQUEST VALIDATION                                                    
*                                                                               
VALID    DS    0H                                                               
         LA    R2,WRIPERH          YYMM DATES REQUIRED                          
         GOTO1 ANY                                                              
         MVI   QPERTYPE,2                                                       
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),(0,BLOCK)                              
         LA    R6,BLOCK                                                         
         USING PERVALD,R6                                                       
         CLI   PVALASSM,X'11'      MMYY-MMYY ENTERED?                           
         JE    VAL0100             YES - SO CONTINUE                            
         CLI   PVALASSM,X'71'      MMYY ONLY ENTERED?                           
         JE    VAL0100             YES - SO CONTINUE                            
*                                                                               
         MVI   ERROR,INVDATE       ELSE - SET ERROR                             
         GOTO1 ERREX               AND GO TO ERROR EXIT                         
*                                                                               
VAL0100  EQU   *                                                                
*                                                                               
         MVC   QSTART(4),PVALESTA  MOVE IN START YYMM                           
         MVC   QSTART+4(2),=C'00'  AND NO DAY                                   
         MVC   QEND(4),PVALEEND    MOVE IN END YYMM                             
         MVC   QEND+4(2),=C'00'    AND NO DAY                                   
         DROP  R6                                                               
*                                                                               
         MVC   MYSTART,SBQSTART    SAVE ORIGINAL REQUEST DATES                  
         MVC   MYEND,SBQEND                                                     
         MVI   MYMONTHS,X'00'                                                   
* GO FORWARD BY 1 YEAR                                                          
         LA    R5,1                                                             
         GOTO1 ADDAY,DMCB,(C'Y',SBQEND),SBQEND,(R5)                             
         MVC   SBQEND+4(2),=C'00'        ZERO DAY                               
* AND BACK                                                                      
         LNR   R5,R5                                                            
         GOTO1 ADDAY,DMCB,(C'Y',SBQSTART),SBQSTART,(R5)                         
         MVC   SBQSTART+4(2),=C'00'      ZERO DAY                               
VALX     B     XIT                                                              
         EJECT                                                                  
* ERROR EXITS AND MESSAGES                                                      
*                                                                               
EINV     MVI   ERROR,INVALID                                                    
         B     CURSOR                                                           
*                                                                               
CURSOR   GOTO1 CURSERR                                                          
*                                                                               
         EJECT                                                                  
INPUT    L     R4,AGLOBAL          ** INPUT ROUTINE **                          
         USING GLOBALD,R4                                                       
         CLI   MYMONTHS,0          TEST TABLE BUILT                             
         BNE   *+8                                                              
         BAS   RE,BLDMON                                                        
         CLI   SBMODE,SBPROCES     HOOK MODE PASSED FROM SPOTIO                 
         BE    PROCEST                                                          
         CLI   SBMODE,SBPROCWP     HOOK MODE PASSED FROM SPOTIO                 
         BE    PROCWP                                                           
         B     XIT                                                              
         SPACE 2                                                                
* DRIVER INPUT ROUTINE                                                          
*                                                                               
DRIVIN   LR    R0,RE                                                            
         MVI   GLMODE,GLINPUT                                                   
         BAS   RE,GODRIVER                                                      
*                                                                               
DRIVINX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
PROCEST  L     R2,SBAIO1                                                        
         USING ESTHDRD,R2                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(4),GETBROAD                                                 
         MVC   WORK+4(4),ADDAY                                                  
         MVC   WORK+8(4),GETDAY                                                 
         MVC   WORK+12(4),DATCON                                                
         L     R3,SBAIO3           GET THE ESTIMATES'S MONTHS                   
         GOTO1 MOBILE,DMCB,(12,SBESTST),(DATEFORM,(R3)),WORK,SBSPPROF           
         GOTO1 DATCON,DMCB,(2,2(R3)),(3,FULL)                                   
         ZIC   R6,FULL+1           R5=MONTH NUMBER                              
         L     R7,SBADATE          ACCUMULATE PAID/ORD $$$ FOR EACH             
*        L     RF,SBNDATES         REQUESTED DATE PERIOD                        
         LHI   RF,48                                                            
*                                                                               
EST2     XC    ORDDOL,ORDDOL       DOLLAR ACCUMULATORS                          
         XC    PAIDDOL,PAIDDOL                                                  
         SR    R1,R1                                                            
*                                                                               
EST4     CLC   0(2,R7),0(R3)       TEST ESTIMATE MONTH WITHIN PERIOD            
         BH    EST6                                                             
         CLC   2(2,R7),2(R3)                                                    
         BL    EST8                                                             
         LR    RE,R6               YES-GET PAID/ORD $$$ FOR THIS MONTH          
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         L     R1,PAIDDOL                                                       
         A     R1,EPAIDN(RE)       YTD                                          
         A     R1,EPAIDN+52(RE)    CURRENT                                      
         ST    R1,PAIDDOL                                                       
         L     R1,ORDDOL                                                        
         A     R1,EORDN(RE)        YTD                                          
         A     R1,EORDN+52(RE)     CURRENT                                      
         ST    R1,ORDDOL                                                        
*                                                                               
EST6     LA    R3,4(R3)            NEXT ESTIMATE MONTH                          
         CLI   0(R3),X'FF'                                                      
         BE    EST8                                                             
         LA    R6,1(R6)            AUGMENT MONTH NUMBER                         
         CH    R6,=H'13'                                                        
         BL    EST4                                                             
         BH    *+12                                                             
         CLI   DATEFORM,10         13TH MONTH ALLOWED FOR 4-WEEK MONTHS         
         BE    EST4                                                             
         LA    R6,1                                                             
         B     EST4                                                             
*                                                                               
EST8     LTR   R1,R1               TEST ANY PAID DOLLARS                        
         BZ    EST10                                                            
         L     RE,SBACHUNK         YES-                                         
         MVC   0(4,RE),0(R7)       STORE PERIOD START/END                       
         MVC   4(4,RE),PAIDDOL     AND PAID DOLLARS                             
         MVC   8(4,RE),ORDDOL      AND ORD DOLLARS                              
*                                                                               
* CALL DRIVER ONLY IF THIS BUCKET IN REQUEST PERIOD                             
         CLC   0(2,RE),SBBQENDP    BUCKET STARTS AFTER REQ END?                 
         BNL   ESTX                 YES - WE'RE DONE                            
         CLC   2(2,RE),SBBQSTP     BUCKET ENDS BEFORE REQ START?                
         BNH   EST10                YES - NEXT BUCKET                           
         BAS   RE,DRIVIN           CALL DRIVER FOR INPUT                        
*                                                                               
EST10    CLI   0(R3),X'FF'         TEST END OF ESTIMATE MONTHS                  
         BE    ESTX                                                             
         LA    R7,4(R7)            NO-NEXT PERIOD                               
         CLI   0(R7),0                                                          
         BE    ESTX                                                             
         BCT   RF,EST2                                                          
*                                                                               
ESTX     B     EQXIT                                                            
         EJECT                                                                  
PROCWP   L     R2,SBAIO1                                                        
         USING PWRECD,R2                                                        
         LA    R6,PWEL                                                          
         SR    R1,R1                                                            
*                                                                               
WIPW05   CLI   0(R6),0                                                          
         BE    WIPWX                                                            
         CLI   0(R6),X'07'                                                      
         BE    WIPW10                                                           
         CLI   0(R6),X'15'                                                      
         BE    WIPW10                                                           
WIPW06   IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     WIPW05                                                           
*                                                                               
WIPW10   ST    R6,SBACURCH                                                      
* CALL DRIVER ONLY IF THIS BUCKET IN REQUEST PERIOD                             
         CLC   2(2,R6),SBBQENDP    BUCKET STARTS AFTER REQ END?                 
         BNL   WIPW06               YES - WE'RE DONE                            
         CLC   2(2,R6),SBBQSTP     BUCKET BEFORE REQ START?                     
         BL    WIPW06                                                           
         BAS   RE,DRIVIN           CALL DRIVER FOR INPUT                        
         B     WIPW06                                                           
*                                                                               
WIPWX    B     EQXIT                                                            
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
SETXSPOT MVC   SYSDIR,=C'XSPDIR  '                                              
         MVC   SYSFIL,=C'XSPFILE '                                              
         MVC   LKEY,=H'32'                                                      
         MVC   LSTATUS,=H'4'                                                    
         BR    RE                                                               
*                                                                               
SETSPOT  MVC   SYSDIR,=C'SPTDIR  '                                              
         MVC   SYSFIL,=C'SPTFILE '                                              
         MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         BR    RE                                                               
         EJECT                                                                  
* DRIVER HOOK                                                                   
*                                                                               
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLINIT       INITIALIZATION                               
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLINCOMP     INTERNAL COMPUTES                            
         BE    INTCOMP                                                          
         CLI   GLHOOK,GLPRINT      PRINT A LINE                                 
         BE    PRINT                                                            
         CLI   GLHOOK,GLFIRST      FIRSTS                                       
         BE    FIRSTS                                                           
         CLI   GLHOOK,GLLAST       LASTS                                        
         BE    LASTS                                                            
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BE    HEAD                                                             
*                                                                               
DRHOOKX  B     XIT                                                              
         EJECT                                                                  
* DRIVER INITIALIZATION                                                         
*                                                                               
DRVINIT  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                                      
*                                                                               
RESOLVE  LA    R1,ROUTLIST         SEARCH LIST FOR ROUTINE NAME                 
*                                                                               
RESOLVE2 CLI   0(R1),FF                                                         
         BE    XIT                                                              
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         B     XIT                                                              
         SPACE 1                                                                
ROUTLIST DS    0F                                                               
         DC    CL8'IWIMORD ',A(IWIMORD)                                         
         DC    CL8'ICLTORD ',A(ICLTORD)                                         
         DC    CL8'OCLTORD ',A(OCLTORD)                                         
         DC    CL8'IBHGRS  ',A(IBHGRS)                                          
         DC    CL8'IBHCGRS ',A(IBHCGRS)                                         
         DC    CL8'IWIMPD  ',A(IWIMPD)                                          
         DC    CL8'IPER    ',A(IPER)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
* INTERNAL COMPUTES HOOK                                                        
*                                                                               
INTCOMP  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
* PRINT A LINE                                                                  
*                                                                               
PRINT    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK FIRSTS                                                            
*                                                                               
FIRSTS   DS    0H                                                               
         CLI   GLARGS,0            TEST LEVEL 0 BREAK                           
         BNE   FIRSTX                                                           
         MVC   TITLE,BLANKS        YES-SET THE APPROPRIATE TITLE                
         MVC   TITLE(21),=C'AGENCY SUMMARY REPORT'                              
         LA    R2,S49TITH          OVERRIDE TITLE?                              
         CLI   5(R2),0                                                          
         BE    FIRSTC                                                           
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
FIRSTC   GOTO1 CENTER,DMCB,TITLE,63                                             
FIRSTX   B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
* DRIVER HOOK LASTS                                                             
*                                                                               
LASTS    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HEADHOOK                                                               
*                                                                               
HEAD     DS    0H                                                               
HDX      B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
* DRIVER HOOK TO EXECUTE ROUTINES                                               
*                                                                               
EXEC     DS    0H                                                               
         L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
*                                                                               
EXECIX   B     XIT                                                              
EXECOX   B     XIT                                                              
         EJECT                                                                  
* DRIVER INPUT/OUTPUT ROUTINES                                                  
*                                                                               
IPER     DS    0H                                                               
         CLI   SBMODE,SBPROCBH                                                  
         BNE   IPER1                                                            
         L     RF,SBAIO1                                                        
         XC    FULL,FULL                                                        
         MVC   FULL(2),BKEYYSRV-BKEY(RF)                                        
         MVI   FULL+2,1            SET FIRST OF MONTH                           
         GOTO1 DATCON,DMCB,(3,FULL),(2,HALF)                                    
         LA    R6,HALF                                                          
         B     IPER4                                                            
*                                                                               
IPER1    L     R6,SBACURCH                                                      
         CLI   SBMODE,SBPROCWP                                                  
         BNE   *+12                                                             
         LA    R6,2(R6)                                                         
         B     IPER4                                                            
*                                                                               
         L     R6,SBACHUNK                                                      
         CLI   SBMODE,SBPROCES                                                  
         BE    IPER4                                                            
         DC    H'0'                                                             
*                                                                               
IPER4    DS    0H                                                               
         LA    R1,MYMONTHS                                                      
*                                                                               
IPER6    OC    0(4,R1),0(R1)                                                    
         BZ    IPER8                                                            
         CLC   0(2,R6),0(R1)                                                    
         BL    IPER7               BEFORE PERIOD                                
         CLC   0(2,R6),2(R1)                                                    
         BNH   IPER10                                                           
         LA    R1,4(R1)                                                         
         B     IPER6                                                            
*                                                                               
IPER7    MVI   3(R2),1                                                          
         B     XIT                                                              
IPER8    MVC   0(4,R2),=X'FEFEFEFE'  AFTER PERIOD                               
         B     XIT                                                              
*                                                                               
IPER10   MVC   0(2,R2),0(R1)       PERIOD START                                 
         MVC   2(2,R2),2(R1)       PERIOD END                                   
         B     EXECIX                                                           
         EJECT                                                                  
*                                                                               
IWIMORD  DS    0H                                                               
         CLI   SBMODE,SBPROCES                                                  
         BNE   EXECIX                                                           
         L     RF,SBACHUNK                                                      
         L     RF,8(RF)                                                         
         B     ROUNDDOL                                                         
*                                                                               
ICLTORD  DS    0H                                                               
         CLI   SBMODE,SBPROCWP                                                  
         BE    CLTORD10                                                         
         CLI   SBMODE,SBPROCES                                                  
         BNE   EXECIX                                                           
         LA    R2,8(R2)            HANG ON TIGHT!!!                             
         L     RF,SBACHUNK                                                      
         L     RF,8(RF)                                                         
         B     ROUNDDOL                                                         
*                                                                               
CLTORD10 DS    0H                                                               
         L     RF,SBACURCH                                                      
         CLI   0(RF),X'07'         WEEKLY CURRENT $ EL?                         
         BNE   CLTORD20             NO                                          
         USING PWCUREL,RF                                                       
         SR    RE,RE                                                            
         CLC   PWCURBIL,=X'80000000'                                            
         BE    *+8                                                              
         L     RE,PWCURBIL                                                      
         L     RF,PWCURCG                                                       
         AR    RF,RE                                                            
         B     ROUNDDOL                                                         
*                                                                               
CLTORD20 DS    0H                                                               
         CLI   0(RF),X'15'         WEEKLY CURRENT $ EL?                         
         BNE   XIT                  NO                                          
         USING PWCLCEL,RF                                                       
         L     RF,PWCLCAMT                                                      
         B     ROUNDDOL                                                         
         DROP  RF                                                               
*                                                                               
OCLTORD  DS    0H                                                               
         MVI   GLHOOK,GLEDIT        NO - MUST BE OUTPUT                         
         TM    GLINDS,GLTOTLIN     THIS A TOTAL LINE?                           
         BZ    OCLT10               NO                                          
         MVC   0(8,R2),CLTORDT                                                  
         ZAP   CLTORDT,=PL8'0'                                                  
         B     OCLTX                                                            
*                                                                               
OCLT10   CP    0(8,R2),=PL8'0'     ANY WIM ORD $$$?                             
         BNE   *+10                 YES - USE THEM                              
         MVC   0(8,R2),8(R2)        ELSE USE EST $$$                            
         AP    CLTORDT,0(8,R2)                                                  
OCLTX    B     EXECOX                                                           
         EJECT                                                                  
*                                                                               
IWIMPD   DS    0H                                                               
         CLI   SBMODE,SBPROCES                                                  
         BNE   EXECIX                                                           
         L     RF,SBACHUNK                                                      
         L     RF,4(RF)                                                         
         B     ROUNDDOL                                                         
         EJECT                                                                  
*                                                                               
IBHGRS   DS    0H                                                               
         CLI   SBMODE,SBPROCBH                                                  
         BNE   EXECIX                                                           
         L     RF,SBBILGRS                                                      
         B     ROUNDDOL                                                         
*                                                                               
IBHCGRS  DS    0H                                                               
         CLI   SBMODE,SBPROCBH                                                  
         BNE   EXECIX                                                           
         L     RF,SBAIO1                                                        
         USING BILLHDRD,RF                                                      
         ICM   RF,15,BCCGRS        GET CLT $ BILLED                             
         BNZ   ROUNDDOL                                                         
         L     RF,SBBILGRS         IF NO CLT BILLED, USE REGULAR BILLED         
         B     ROUNDDOL                                                         
         DROP  RF                                                               
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* INPUT: RF = DOLLARS                                                           
*                                                                               
* (NOTE: DOESN'T REALLY ROUND ANYMORE - CARRY ALL PENNIES AND ROUND             
*        ON OUTPUT).                                                            
*                                                                               
ROUNDDOL CVD   RF,DUB                                                           
         ZAP   0(8,R2),DUB                                                      
*        SRP   0(8,R2),64-2,5      DIVIDE BY 100 TO GET ROUNDED $$$             
         B     EXECIX                                                           
         EJECT                                                                  
* ROUTINE TO HOOK TO CALLING PROGRAM TO CALL DRIVER                             
*                                                                               
GODRIVER NTR1                                                                   
         L     RF,ADRIVER                                                       
         L     RE,SAVERD                                                        
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF                                                            
         XIT1                      DO NOT CHANGE THIS!!!                        
         EJECT                                                                  
*                                                                               
* BUILD MY OWN MONTH PERIOD TABLE                                               
BLDMON   NTR1                                                                   
         MVC   DATEFORM,SBSPPROF+2  PICK UP DATE FORM FROM SPOT PROFILE         
         NI    DATEFORM,X'0F'                                                   
         OC    SBQBCLT,SBQBCLT     TEST MULTI CLIENT REQUEST                    
         BNZ   *+14                                                             
         MVI   DATEFORM,0          YES -  FORCE TO BROADCAST WEEKS              
         MVC   SBSPPROF+6(3),=X'010101'                                         
*                                                                               
         MVI   MYSTART+5,C'1'      SET TO FIRST DAY OF MONTH                    
         MVI   MYEND+5,C'1'        SET TO THE BROADCAST MONTH END               
         GOTO1 GETBROAD,DMCB,(1,MYEND),WORK,GETDAY,ADDAY                        
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   MYEND,WORK+6                                                     
*                                                                               
         CLI   DATEFORM,2                                                       
         BE    BLD40                                                            
*                                  FORCE START AND END TO BROADCAST             
         GOTO1 GETDAY,DMCB,MYSTART,DUB                    WEEKS                 
         SR    R4,R4                                                            
         IC    R4,0(R1)            (DAY NUMBER)                                 
         LA    R5,1                ASSUME MONDAY                                
         CLI   SBSPPROF+8,0        TEST SPECIAL DAY OF WEEK                     
         BE    BLD10                                                            
         ZIC   R5,SBSPPROF+8       FOUND ONE - USE THAT                         
*                                                                               
BLD10    CR    R4,R5                                                            
         BE    BLD20                                                            
         BH    *+8                                                              
         LA    R4,7(R4)                                                         
         SR    R5,R4                                                            
         MVC   DUB(6),MYSTART                                                   
         GOTO1 ADDAY,DMCB,DUB,MYSTART,(R5)                                      
*                                                                               
BLD20    GOTO1 GETDAY,DMCB,MYEND,DUB                                            
         IC    R4,0(R1)                                                         
         LA    R5,7                ASSUME ENDING ON SUNDAY                      
         CLI   SBSPPROF+8,0                                                     
         BE    BLD30                                                            
         ZIC   R5,SBSPPROF+8       START DAY FOUND IN PROFILE                   
         BCT   R5,BLD30            END IS 1 LESS THAN THIS                      
         LA    R5,7                                                             
*                                                                               
BLD30    CR    R4,R5                                                            
         BE    BLD40                                                            
         BL    *+8                                                              
         LA    R5,7(R5)                                                         
         SR    R5,R4                                                            
         MVC   DUB,MYEND                                                        
         GOTO1 ADDAY,DMCB,DUB,MYEND,(R5)                                        
*                                                                               
BLD40    GOTO1 PERVERT,DMCB,MYSTART,MYEND    GET NUMBER OF MONTHS               
         LA    R4,MYMONTHS         BUILD MONTHS                                 
         MVC   WORK(4),GETBROAD                                                 
         MVC   WORK+4(4),ADDAY                                                  
         MVC   WORK+8(4),GETDAY                                                 
         MVC   WORK+12(4),DATCON                                                
         LA    RE,MYSTART                                                       
         ST    RE,DMCB                                                          
         MVC   DMCB(1),DMCB+15     NUMBER OF MONTHS IN REQUEST                  
         GOTO1 MOBILE,DMCB,,(DATEFORM,(R4)),WORK,SBSPPROF                       
         CLI   0(R4),X'FF'                                                      
         BE    *+12                                                             
         LA    R4,4(R4)                                                         
         B     *-12                                                             
         MVI   0(R4),0                                                          
BLDX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
         GETEL (R6),24,ELCODE                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
         SPACE 1                                                                
*                                                                               
BLANKS   DC    CL132' '                                                         
XFF      DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                           
FF       EQU   X'FF'                                                            
*                                                                               
FLAGS    DS    X                                                                
ORDDOL   DS    F                                                                
PAIDDOL  DS    F                                                                
CLTORDT  DS    PL8                 WIMORD TOTAL                                 
*                                                                               
MYSTART  DS    CL6                 ORIGINAL REQ DATES                           
MYEND    DS    CL6                 ORIGINAL REQ DATES                           
MYMONTHS DS    25XL4                                                            
MYAMON   DS    49XL4               USED FOR AMONTHS                             
*                                                                               
         EJECT                                                                  
WORKD    DSECT                                                                  
*                                                                               
SAVERD   DS    A                                                                
RELO     DS    A                                                                
AGEND    DS    A                                                                
*                                                                               
WORKL    EQU   *-WORKD                                                          
*                                                                               
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*SPWRIWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*DRINTRECD                                                                      
*SPGENAGY                                                                       
*SPGENBUY                                                                       
*SPGENEST                                                                       
*SPGENBILL                                                                      
*SPGENWIPW                                                                      
*SPWRIFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD2                                                     
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
BILLHDRD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPGENWIPW                                                      
       ++INCLUDE SPWRIFFD                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIB5D                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDPERVALD                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPWRI25   02/05/02'                                      
         END                                                                    
