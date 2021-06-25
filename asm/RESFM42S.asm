*          DATA SET RESFM42S   AT LEVEL 121 AS OF 01/03/03                      
*PHASE T81842A,*                                                                
*INCLUDE REGENBUC                                                               
         TITLE 'T81842 - RESFM42 - BACKHIST TAKEOVER SCROLLER'                  
*                                                                               
*******************************************************************             
*                                                                 *             
*        RESFM42 (T81842) --- BACKHIST TAKEOVER SCROLLER          *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* NOV22/99 (BU ) --- ORIGINAL ENTRY                               *             
*                                                                 *             
* JAN20/00 (BU ) --- CLEAR CMBOTBL FULLY.  LEFTOVER WAS CAUSING   *             
*                    AN ABORT SITUATION.                          *             
*                                                                 *             
* JAN26/00 (BU ) --- DON'T FORGET TO REMOVE CODE WHICH INSERTS    *             
*                    TEST DATES INTO RSTAHIST FIELDS!!            *             
*                                                                 *             
* FEB23/00 (BU ) --- TAKE INVOICE DATA ONLY IF BOTH REPS PERMIT   *             
*                                                                 *             
* APR06/00 (BU ) --- FIX MAXIO PROBLEM.                           *             
*                                                                 *             
* JUL05/00 (BU ) --- REMOVE REFERENCES TO GLV1GOTO PER MEL H.     *             
*                                                                 *             
* SEP22/00 (BU ) --- NEW MAKEGOOD PROCESSING                      *             
*                                                                 *             
* DEC04/00 (BU ) --- ADD LOOKUP FOR A201 KEY AS WELL AS A202      *             
*                    DON'T FILTER OUT NETWORK ORDERS              *             
*                    TAKE OVER WIPS                               *             
*                    DON'T DELETE SALES ASSISTANTS                *             
*                                                                 *             
* SEP25/01 (BU ) --- REMOVE A201 LOOKUP FOR BACKHIST TAKEOVERS.   *             
*                                                                 *             
* NOV06/01 (SKU) --- FIX OFFICE/TEAM MISMATCH BUG                 *             
*                                                                 *             
* SEP18/02 (SKU) --- ADD 'BF' KEY FOR CODESWIT                    *             
*                                                                 *             
*                                                                 *             
*                    ***  END TOMBSTONE  ***                      *             
*******************************************************************             
*                                                                               
T81842   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1842**,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         MVC   VSWITCH,CSWITCH     SET A(SWITC)                                 
         MVC   VBINSRCH,CBINSRCH   SET A(BINSRCH)                               
         DROP  RE                                                               
*                                  ESTABLISH A(REPFACS)                         
         GOTO1 CALLOV,DMCB,0,X'D9000AAC',0                                      
         MVC   VREPFACS,0(R1)                                                   
*                                  EXTERNAL ROUTINE, MISC. SUBROUTINES          
*                                                                               
*                                                                               
*   TEST DUMP                                                                   
DIE      EQU   *                                                                
****>>>  MVC   DIE(2),=X'0000'     FORCE DUMP                                   
*   TEST DUMP END                                                               
*                                                                               
*                                                                               
         LA    RF,CONHEADH         SET A(BUCKET STORAGE AREA)                   
         A     RF,=F'6000'                                                      
         ST    RF,ABUCKBLK                                                      
         LA    RF,CTOLAST          SET A(EQUIVALENCY TABLE)                     
         A     RF,=F'8000'         DISPLACE 8000 DOWN                           
         ST    RF,AEQUITBL         SAVE A(EQUIVALENCY TABLE)                    
         ST    RF,AEQUNTRY         SAVE A(EQUIVALENCY TABLE IN USE)             
         A     RF,=F'3000'         DISPLACE ANOTHER 3000                        
         ST    RF,AMISFLGS         SAVE A(MISCELLANEOUS FLAGS SPACE)            
*                                                                               
*  'MISFLGS' ARE SET UP 11K AFTER CTOLAST                                       
*                                                                               
         A     RF,=F'120'          DISPLACE ANOTHER 120                         
         ST    RF,ACONTROL         SAVE A(CONTROL RESTORATION SPACE)            
         A     RF,=F'400'          DISPLACE ANOTHER 400                         
         ST    RF,ACMBOTBL         SAVE A(COMBO PARTIC TABLE)                   
         ST    RF,ACMBNTRY         SAVE A(COMBO PARTIC TABLE IN USE)            
         MVI   ACTELOPT,C'N'       SET 'NO ACTIVITY ELEMENT'                    
*                                                                               
         OI    CONSERVH+6,X'81'    SCREEN IS ALWAYS MODIFIED                    
*                                                                               
         L     R3,AMISFLGS         MISCELLANEOUS FLAGS STORAGE AREA             
*                                                                               
         USING MISFLGS,R3                                                       
*                                                                               
         TM    MISFLAGS,X'80'      RETURN FROM GLOBBER CALL?                    
*                                                                               
***>>>   GOTO1 =A(SUBROUT),DMCB,(RC),('QCKGLOB',0),0,RR=YES                     
         BNO   MAIN0040                                                         
*                                                                               
         NI    MISFLAGS,X'FF'-X'80'                                             
*                                  TURN OFF RETURN FLAG                         
         DROP  R3                                                               
*                                                                               
         B     LIST0640            CONTINUE TO CHECK DISPLAY ARRAY              
*                                                                               
MAIN0040 EQU   *                                                                
*                                                                               
***   SHOULD THIS TRANSFER TO CHECK-TABLE ROUTINE??                             
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,LISTRECS       LIST POSSIBLE TAKEOVER ORDERS                
         BE    LIST                                                             
         CLI   MODE,SETFILE        SWITCH TO ALTERNATE FILES                    
         BE    SETFILES                                                         
***>>>   CLI   MODE,DISPREC        DISPLAY A SPECIFIC ORDER                     
***>>>   BE    DISP                                                             
                                                                                
         B     EXIT                                                             
                                                                                
NO       LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
*******************************************************************             
* VALIDATE KEY                                                                  
*******************************************************************             
VKEY     DS    0H                                                               
*                                                                               
         XC    IOCOUNT,IOCOUNT     CLEAR IO COUNT                               
         MVI   MAXIOFLG,0          CLEAR MAX IO FLAG                            
         GOTO1 =A(VKEYNMOD),DMCB,(RC),RR=Y                                      
         BNZ   VKEY0800            ERROR RETURN:  EXIT                          
*                                                                               
         L     RF,AMISFLGS         MISCELLANEOUS FLAGS STORAGE AREA             
*                                                                               
         USING MISFLGS,RF                                                       
*                                                                               
         XC    SINVTAK(2),SINVTAK  CLEAR INVOICE TAKEOVER FLAGS                 
*                                                                               
         DROP  RF                                                               
*                                                                               
*                                                                               
         GOTO1 FOUTBLK,DMCB,CTOSELH,CTOLAST,0,0                                 
*                                                                               
         LA    R2,CTOREPH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         MVC   TARGREP,TWAAGY      SAVE TWO-CHAR REP ID                         
         GOTO1 =A(VALIREP),DMCB,(RC),RR=Y                                       
         BNZ   SRCENOGD            SOURCE REP NOT FOUND ON FILE                 
         LA    R2,CTOSTAH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALISTA             VALIDATE THE STATION                         
         MVC   SRCESTAT,WORK       RETURNED FROM VALISTA                        
         MVC   SRCEGRP,WORK+41                                                  
         GOTO1 =A(OFFCTEAM),DMCB,(RC),RR=Y                                      
                                                                                
* IF STATION SIGN-ON, CHECK IF IT'S A VALID SIGN ON ID                          
                                                                                
         CLI   TWAACCS,C'$'                                                     
         BNE   VKEY0100                                                         
*                                                                               
         B     SLOCKOUT            STATION SHOULDN'T BE DOING THIS!!            
*                                                                               
*        L     R6,AIO                                                           
*        USING RSTASOEL,R6                                                      
*        MVI   ELCODE,6            GET VALID SIGN ON ID ELEMENT                 
*        BAS   RE,GETEL                                                         
*        BNE   VKEY0100                                                         
*                                                                               
*VKEY0080 DS    0H                                                              
*        CLC   RSTASID,TWAORIG     VALID SIGN-ON?                               
*        BE    VKEY0100            YES - PROCEED                                
*        BAS   RE,NEXTEL           NO  - CHECK NEXT ELEMENT                     
*        BE    VKEY0080                                                         
*        B     SLOCKOUT            ALL DONE, NO MATCH, NOT VALID                
*        DROP  R6                                                               
                                                                                
VKEY0100 DS    0H                                                               
         LA    R2,CTOOFFH          OFFICE OPTIONAL                              
         CLI   5(R2),0                                                          
         BE    VKEY0110                                                         
         GOTO1 =A(CHEKOFF),DMCB,(RC),RR=Y                                       
         BNZ   INVOFF              OFF NOT FOUND ON SOURCE REP FILE             
***>>>   GOTO1 VALIOFF                                                          
                                                                                
VKEY0110 DS    0H                                                               
         XC    SAVEAGY,SAVEAGY                                                  
         LA    R2,CTOAGIH          AGENCY OPTIONAL                              
         CLI   5(R2),0                                                          
         BE    VKEY0120                                                         
         GOTO1 CHEKAGI                                                          
         BNZ   INVAGY              AGY NOT FOUND ON SOURCE REP FILE             
                                                                                
VKEY0120 DS    0H                                                               
*                                                                               
*   EFFECTIVE/TAKEOVER DATE IS NOW A PROTECTED FIELD WHICH WILL                 
*        BE DERIVED FROM THE JOIN DATE OF THE TARGET REP'S                      
*        STATION RECORD.                                                        
*                                                                               
***      LA    R2,CTODATEH         EFFECTIVE DATE REQUIRED                      
***      CLI   5(R2),0                                                          
***      BE    MISSFLD                                                          
***      GOTO1 VALIPERI                                                         
*                                                                               
*   AFTER VALIDATION OF EFFECTIVE DATE, CHECK THAT STATION RECORDS              
*        PERMIT MOVEMENT                                                        
*                                                                               
         GOTO1 =A(CHEKTSTA),DMCB,(RC),RR=YES                                    
*                                  CHECK TARGET STATION RECORD                  
         BNZ   STABARRD            STATION NOT CLEARED FOR TRANSFER             
                                                                                
VKEY0140 DS    0H                                                               
***      LA    R2,CTOASOFH         AS OF OVERRIDE DATE OPTIONAL                 
***      CLI   5(R2),0                                                          
***      BE    VKEY0160                                                         
***      GOTO1 VALIASOF                                                         
                                                                                
VKEY0160 DS    0H                                                               
         LA    R2,CTOSALH          SALESPERSON DEFAULT OPTIONAL                 
         CLI   5(R2),0                                                          
         BE    VKEY0170                                                         
         GOTO1 CHEKSALL                                                         
         BZ    VKEY0170            SUCCESSFUL RETURN                            
         CLI   BYTE,1              S/P LEAVE DATE FOUND?                        
         BE    INVSPLEV            YES - DISPLAY MESSAGE                        
         B     INVSALPR            NO  - SALESPERSON NOT ON FILE                
                                                                                
VKEY0170 DS    0H                                                               
         LA    R2,CTOTYPH          CONTRACT TYPE DEFAULT OPTIONAL               
         CLI   5(R2),0                                                          
         BE    VKEY0180                                                         
         GOTO1 CHEKTYPE                                                         
         BNZ   INVTYPE             CONTRACT TYPE NOT ON FILE                    
                                                                                
VKEY0180 DS    0H                                                               
         MVC   CTOLAST+1(2),=X'0101' RETRANSMIT THE ENTIRE SCREEN               
         B     VKEY0800                                                         
VKEY0800 EQU   *                                                                
***      BAS   RE,SETUSED          SET UP USED CONTRACTS TABLE                  
*                                     NEED TO READ SIGN-ON REPFIL               
*                                     SO THIS RTN IS CALLED BEFORE              
*                                     MODE OF 'SETFILE' IS RECEIVED             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE AS AT DATE                                                           
* INPUT  - R2 POINTS TO AS AT FIELD  HEADER                                     
*                                                                               
*                                                                               
***********************************************************************         
**VALIASOF NTR1                                                                 
**         B     EXIT                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERIOD                                                               
* INPUT  - R2 POINTS TO PERIOD FIELD HEADER                                     
* OUTPUT - EFFDATE HAS EFFECTIVE DATE                                           
*                                                                               
***********************************************************************         
*VALIPERI NTR1                                                                  
*        XC    BLOCK(256),BLOCK                                                 
*        GOTO1 SCANNER,DMCB,(R2),BLOCK,C',=,-'                                  
*        CLI   DMCB+4,0                                                         
*        BE    INVLDAT2            ERROR ENCOUNTERED                            
*                                                                               
* VALIDATE START DATE                                                           
*                                                                               
*        LA    R5,BLOCK                                                         
*        GOTO1 DATVAL,DMCB,(0,12(R5)),EFFDATE                                   
*        OC    DMCB(4),DMCB        ERROR?                                       
*        BZ    INVLDAT2                                                         
*        GOTO1 DATCON,DMCB,(0,EFFDATE),(2,EFDTCOMP)                             
*                                  CONVERT EFFECTIVE DATE TO COMP               
*        GOTO1 DATCON,DMCB,(0,EFFDATE),(3,EFDTBIN)                              
*                                  CONVERT EFFECTIVE DATE TO BINARY             
*                                                                               
*VALPERX  DS    0H                                                              
*        B     EXIT                                                             
         EJECT                                                                  
*******************************************************************             
* LIST RECORDS                                                    *             
*      'LIST' MODE IS DONE AFTER 'SETFILE' MODE IS RECEIVED, SO   *             
*             LIST IS NOW POINTING AT THE SOURCE REPFILE.         *             
*******************************************************************             
LIST     DS    0H                                                               
*                                                                               
         MVI   TAKEALL,C'N'        SET 'TAKEALL = NO'                           
         LA    RF,CTOSELH          SET A(FIRST SELECT FIELD)                    
         LA    RE,CTOLAST          SEND A(END OF SCREEN)                        
         LR    R2,RF                                                            
         CLI   5(RF),2             INPUT CAN'T BE MORE THAN 2                   
         BH    INVLFLD                                                          
         CLC   8(2,RF),=C'S+'      ALL LINES SELECTED?                          
         BNE   LIST0020            NO                                           
         MVI   TAKEALL,C'Y'        SET 'TAKEALL = YES'                          
         B     LIST0008                                                         
LIST0005 EQU   *                                                                
         CR    RE,RF               END OF SCREEN REACHED?                       
         BNH   LIST0010            YES - FINISHED                               
         CLI   5(RF),0             IF FIRST POS. = 'S+'                         
         BE    LIST0008            OTHERS MUST BE BLANK OR '*S'                 
         LR    R2,RF                                                            
         CLI   5(RF),2                                                          
         BNE   INVLFLD                                                          
         CLC   8(2,RF),=C'*S'      ELSE ERROR                                   
         BNE   INVLFLD                                                          
LIST0008 EQU   *                                                                
         LA    RF,CTOSEL1H-CTOSELH(RF)                                          
         B     LIST0005            GO BACK FOR NEXT                             
LIST0010 EQU   *                                                                
         CLI   TAKEALL,C'Y'        IF SELECT ALL, PROCESS SELECT LINES          
         BE    LIST0040                                                         
         B     LIST0060                                                         
*                                                                               
*   LOOK FOR PROCESS REQUEST (S ON REQUEST LINE)                                
*                                                                               
LIST0020 EQU   *                                                                
         CR    RE,RF               END OF SCREEN REACHED?                       
         BNH   LIST0060            YES - FINISHED                               
         LR    R2,RF                                                            
         CLI   5(RF),2                                                          
         BNE   LIST0031                                                         
         CLC   8(2,RF),=C'*S'                                                   
         BE    LIST0030                                                         
LIST0031 CLI   5(RF),1                                                          
         BNE   LIST0032                                                         
         CLI   8(RF),C'S'          LINE SELECTED?                               
         BE    LIST0040            YES - PROCESS SELECTED LINE(S)               
LIST0032 CLI   5(RF),0             IF NOT 'S' OR '*S' OR BLANK - ERROR          
         BNE   INVLFLD                                                          
LIST0030 EQU   *                                                                
         LA    RF,CTOSEL1H-CTOSELH(RF)                                          
*                                  NO  - BUMP TO NEXT SELECT FIELD              
         B     LIST0020            GO BACK FOR NEXT                             
LIST0040 EQU   *                                                                
         BAS   RE,ADDCONS          PROCESS SCREEN DATA                          
         BZ    LIST0620            NO ERRORS - UPDATED: REDISPLAY               
         L     R2,DUB              LOAD A(ERROR FIELD)                          
         B     ERREND              EXIT WITH ERROR                              
LIST0060 EQU   *                                                                
         L     RF,AEQUITBL         SET A(EQUIVALENCY TABLE)                     
         XCEFL 0(RF),1000          CLEAR TABLE                                  
         L     RF,ACMBOTBL         CLEAR COMBO CONTROL TABLE                    
         XC    0(200,RF),0(RF)     CLEAR COMBO TABLE AREA                       
         XC    200(100,RF),200(RF) CLEAR REMAINDER OF COMBO TABLE AREA          
         GOTO1 CHEKSSTA            CHECK SOURCE STATION RECORD                  
         BZ    LIST0080            STATION CLEARED FOR TRANSFER                 
*                                                                               
         LA    R2,CTOSTAH          SET CURSOR RETURN POSITION                   
         BAS   RE,SWTGTREP         SWITCH BACK TO ORIGINAL REP                  
         TM    BARFLAG,X'80'       BARRED BY OLD REP?                           
         BO    OLDBARRS            YES                                          
         B     STABARRS            NO  - STA NOT CLEARED FOR TRANSFER           
*                                                                               
LIST0080 EQU   *                                                                
         LA    RF,CTOSELH          SET A(FIRST SELECT FIELD)                    
         LA    RE,CTOLAST          SEND A(END OF SCREEN)                        
LIST0100 EQU   *                                                                
         CR    RE,RF               END OF SCREEN REACHED?                       
         BNH   LIST0120            YES - FINISHED                               
         OI    4(RF),X'20'         SET PREVIOUSLY VALID BIT                     
         LA    RF,CTOSEL1H-CTOSELH(RF)                                          
*                                  BUMP TO NEXT SELECT FIELD                    
         B     LIST0100            GO BACK FOR NEXT LINE                        
LIST0120 EQU   *                                                                
*                                                                               
         LA    R6,KEY                                                           
         USING RCONKEY,R6                                                       
*                                                                               
*                                                                               
****     OC    FIRSTKEY,FIRSTKEY                                                
****     BNZ   LIST0140                                                         
         GOTO1 FOUTBLK,DMCB,CTOSELH,CTOLAST,0,0                                 
*                                                                               
                                                                                
                                                                                
LIST0140 EQU   *                                                                
         MVI   CLEARFLG,C'N'       UNLESS FIRST TIME, SCRN NOT CLEARED          
                                                                                
         LA    R2,CTOCONH          FIRST FIELD ON SCREEN                        
                                                                                
         CLI   NEXTSCRN,C'Y'       USER JUST PRESSED ENTER?                     
         BNE   LIST0220            W/O CHANGING THE KEY FIELDS                  
*                                  YES - DISPLAY NEXT PAGE OF CONTRACTS         
         XC    SCRNKEY,SCRNKEY     CLEAR FIRST SCROLLING KEY                    
         CLI   LASTSCRN,C'Y'       LAST SCREEN DISPLAYED?                       
         BNE   LIST0180            NO                                           
         MVI   LASTSCRN,C'N'       YES - TURN OFF SWITCH                        
         MVC   KEY,FIRSTKEY        RESET FIRST KEY TO RESTART                   
         B     LIST0200                                                         
LIST0180 EQU   *                                                                
         MVC   KEY,SAVEKEY         INSERT LAST CONTRACT DISPLAYED               
         XC    KEY+16(11),KEY      CLEAR LOW-ORDER KEY                          
         MVI   KEY+16,10           SET TO SKIP-READ THIS KEY                    
                                                                                
LIST0200 EQU   *                                                                
         GOTO1 HIGH                                                             
*                                                                               
         B     LIST0280                                                         
                                                                                
LIST0220 DS    0H                                                               
         XC    KEY,KEY                                                          
                                                                                
         MVI   RCONSTYP,X'8E'      STA/FLIGHT/CON#                              
*                                                                               
         MVC   RCON8ERP,SRCEREP    INSERT SOURCE REP                            
         MVC   RCON8EST,SRCESTAT   INSERT TAKEOVER STATION                      
LIST0240 EQU   *                                                                
         GOTO1 HIGH                                                             
         B     LIST0280                                                         
LIST0260 EQU   *                                                                
         GOTO1 SEQ                                                              
LIST0280 EQU   *                                                                
*                                                                               
*   DUMP                                                                        
***      CLC   =X'04757630',RCON8ECN                                            
***      BNE   *+6                                                              
***      DC    H'0'                                                             
*   DUMP SPECIAL CONTRACT                                                       
*                                                                               
         L     RF,IOCOUNT          INCREASE IO COUNT                            
         LA    RF,1(RF)                                                         
         ST    RF,IOCOUNT                                                       
         CLC   IOCOUNT,=F'08000'   MAX IO COUNT REACHED?                        
***      CLC   IOCOUNT,=F'50000'   MAX IO COUNT REACHED?                        
         BL    LIST0290                                                         
         MVC   SAVEKEY,MAXIOKEY    SAVE KEY FOR RESTART                         
         MVI   MAXIOFLG,1          SET MAX IO ENCOUNTERED                       
         L     RF,MAXIOCTR         INCREASE MAX IO COUNT                        
         LA    RF,1(RF)                                                         
         ST    RF,MAXIOCTR                                                      
         B     LIST0620            END THIS CYCLE                               
LIST0290 EQU   *                                                                
         MVC   MAXIOKEY,KEY        SAVE KEY FOR MAXIO SITUATION                 
         CLC   KEY(8),KEYSAVE      SAME KEY TYPE/REP/STATION?                   
         BNE   LIST0600            NO  - FINISHED SWEEP                         
*                                                                               
*   DOESN'T MATTER WHEN ORDER BEGAN.  WHAT IS IMPORTANT IS THAT IT              
*        WAS ACTIVE AFTER THE EFFECTIVE DATE OF THE TAKEOVER.                   
*        THEREFORE, THE TEST FOR START DATE IS DEACTIVATED                      
*                                                                               
***      CLC   EFDTCOMP,RCON8EFS   EFFECTIVE DATE VS KEY START DATE             
***      BL    LIST0300            STARTED BEFORE EFF DATE: SKIP                
         CLC   RCON8EFS,EFDTCOMP   KEY START DATE VS EFFECTIVE DATE             
*                                  STARTED BEFORE EFF DATE?:                    
         BL    LIST0320            YES - CHECK END VS HIST DATE                 
LIST0300 EQU   *                   NO  - SKIP THIS CONTRACT                     
         MVI   RCON8EID,10         SET TYPE UP TO SKIP TO NEXT CON              
         XC    RCON8EAG(10),RCON8EAG                                            
*                                  CLEAR LOW KEY                                
         B     LIST0240            RESTART ON NEXT KEY                          
LIST0320 EQU   *                                                                
         CLC   RCON8EFE,HISTDATE   KEY END DATE VS HISTORY DATE                 
*                                  ENDED BEFORE HISTORY T/O DATE?               
         BL    LIST0300            YES - SKIP THIS CONTRACT                     
*                                                                               
*                                                                               
*   DUMP                                                                        
***      CLC   =X'03126861',RCON8ECN                                            
***      BNE   *+6                                                              
***      DC    H'0'                                                             
*   DUMP SPECIAL CONTRACT                                                       
*                                                                               
         CLI   CTOAGIH+5,0         AGENCY FILTER?                               
         BE    LIST0380            NO  -                                        
         CLI   RCON8EID,1          YES - TYPE 1 KEY (AGENCY/ADVERT)?            
         BNE   LIST0380            NO  - FILTER TEST ALREADY DONE -             
*                                     NEXT FILTER(S) TO BE DONE                 
         CLC   SAVEAGY+4(2),SPACES AGENCY-OFFICE IN FILTER?                     
         BE    LIST0340            NO                                           
         CLC   RCON8AGY,SAVEAGY    YES - ORDER FOR AGENCY/OFF FILTER?           
*                                     COMPARE SIX CHARS                         
         BE    LIST0380            YES - CHECK NEXT FILTER                      
         B     LIST0360            NO  - SET TO SKIP                            
LIST0340 EQU   *                                                                
         CLC   RCON8AGY(4),SAVEAGY YES - ORDER FOR AGENCY FILTER?               
*                                     COMPARE FOUR CHARS                        
         BE    LIST0380            YES - CHECK NEXT FILTER                      
LIST0360 EQU   *                                                                
         MVI   RCON8EID,10         NO  - SET TYPE TO SKIP TO NEXT CON           
         XC    RCON8EAG(10),RCON8EAG                                            
*                                  CLEAR LOW KEY                                
         B     LIST0240            RESTART ON NEXT KEY                          
*                                                                               
LIST0380 EQU   *                                                                
         CLI   RCON8EID,2          TYPE 2 KEY (SP/CONTYP/ETC)?                  
         BH    LIST0390            HIGH - CHECK FOR TYP3                        
         BL    LIST0260            LOW  - GO BACK FOR TYP2                      
         CLI   RCON8CTP,C' '       NO CONTYPE ON RECORD?                        
         BE    LIST0390            YES - NO FILTER                              
         CLI   RCON8CTP,X'00'      NO CONTYPE ON RECORD?                        
         BE    LIST0390            YES - NO FILTER                              
         LA    R5,CTYPTABL         YES - IS CONTYPE TO BE SKIPPED?              
LIST0384 EQU   *                                                                
         CLI   0(R5),0             END OF TABLE REACHED?                        
         BE    LIST0390            YES                                          
         CLC   0(1,R5),RCON8CTP    CONTRACT TYPE IN TABLE?                      
         BE    LIST0400            YES - SKIP THIS ORDER                        
         LA    R5,1(R5)            NO  - NEXT TABLE ENTRY                       
         B     LIST0384            GO BACK FOR NEXT                             
LIST0390 EQU   *                                                                
         CLI   CTOOFFH+5,0         OFFICE FILTER?                               
         BE    LIST0420            NO  -                                        
         CLI   RCON8EID,3          YES - TYPE 3 KEY (OFF/DEMO/CREAT)?           
         BNE   LIST0260            NO  - GO BACK FOR NEXT KEY                   
         CLC   RCON8EOF,CTOOFF     ORDER FOR FILTER OFFICE?                     
         BE    LIST0420            YES - PROCESS ORDER                          
LIST0400 EQU   *                                                                
         MVI   RCON8EID,10         NO  - SET TYPE TO SKIP TO NEXT CON           
         XC    RCON8EAG(10),RCON8EAG                                            
*                                  CLEAR LOW KEY                                
         B     LIST0240            RESTART ON NEXT KEY                          
LIST0420 DS    0H                                                               
*                                                                               
*   TEST ONLY:  'NOPW/NO9E CONSIDERED AS SET!!                                  
*        REMOVE FOR FINAL TESTING                                               
*                                                                               
***>>>   B     LIST0440            YES - NO 9E KEY TEST DONE                    
*                                                                               
         CLC   =C'NO9E',CTOOPT     'IGNORE 9E' OPTION SET?                      
         BE    LIST0445            YES - NO 9E KEY TEST DONE                    
         CLC   =C'NOPE',CTOOPT     'IGNORE 9E/NO PW' OPTION SET?                
         BE    LIST0445            YES - NO 9E KEY TEST DONE                    
*                                                                               
*   DON'T USE CONTABLE:  ACCESS PASSIVE A202 FOR PRIOR ACTIVITY                 
*                                                                               
         MVC   KEYA2SAV,KEY        SAVE 8E KEY IN PROGRESS                      
         L     RF,IOCOUNT          INCREASE IO COUNT                            
         LA    RF,1(RF)                                                         
         ST    RF,IOCOUNT                                                       
         BAS   RE,SWTGTREP         SWITCH TO TARGET REP                         
         XC    SETKEYA2,SETKEYA2   CLEAR NEW KEY                                
         MVC   SETKEYA2(2),=X'A201'      INSERT SETKEYA1/TKO TYPE               
         MVC   SETKEYA2+12(2),TARGREP    INSERT TARGET REP                      
         MVC   SETKEYA2+14(5),RCON8EST   INSERT STATION LETTERS                 
         MVC   SETKEYA2+19(2),SRCEREP    INSERT SOURCE REP                      
         MVC   SETKEYA2+23(4),RCON8ECN   INSERT CONTRACT NUMBER                 
         MVC   KEY,SETKEYA2        OVERLAY KEY WITH A1 SETUP                    
         CLC   =C'ALL',CONREC      ALLHIST REQUEST?                             
         BNE   LIST0425            NO  - DON'T CHECK A201 KEYS                  
         GOTO1 HIGH                                                             
         L     RF,IOCOUNT          INCREASE IO COUNT                            
         LA    RF,1(RF)                                                         
         ST    RF,IOCOUNT                                                       
         CLC   KEY(27),KEYSAVE     A201 KEY ON FILE?                            
         BE    LIST0430            YES - TAKEOVER USED: SKIP ORDER              
LIST0425 EQU   *                                                                
         MVC   SETKEYA2(2),=X'A202'      INSERT SETKEYA2/BB  TYPE               
         MVC   KEY,SETKEYA2        OVERLAY KEY WITH A2 SETUP                    
         GOTO1 HIGH                                                             
         L     RF,IOCOUNT          INCREASE IO COUNT                            
         LA    RF,1(RF)                                                         
         ST    RF,IOCOUNT                                                       
         CLC   KEY(27),KEYSAVE     KEY ON FILE?                                 
         BNE   LIST0440            NO  - DO THIS ORDER                          
LIST0430 EQU   *                                                                
         BAS   RE,SWSRCREP         SWITCH TO SOURCE REP                         
         MVC   KEY,KEYA2SAV        RESTORE SOURCE FILE SEQUENCE                 
         GOTO1 HIGH                REREAD FOR SEQUENCE                          
         B     LIST0400            SKIP THIS ORDER                              
                                                                                
***      LA    R4,CONTABLE         SET A(CONTRACT NUMBER TABLE)                 
***      L     R3,CONCTR           SET CURRENT CONTRACT CTR                     
*                                                                               
***      GOTO1 VBINSRCH,DMCB,RCON8ECN,(R4),(R3),4,(0,4),500                     
*                                  IS CONTRACT IN TABLE.  IF SO,                
*                                     HAS ALREADY BEEN TRANSFERRED              
***      CLI   DMCB,0              RECORD FOUND?                                
***      BE    LIST0400            YES - SKIP THIS ORDER                        
*                                                                               
         DROP  R6                                                               
                                                                                
LIST0440 DS    0H                                                               
         BAS   RE,SWSRCREP         SWITCH TO SOURCE REP                         
         MVC   KEY,KEYA2SAV        RESTORE SOURCE FILE SEQUENCE                 
         GOTO1 HIGH                REREAD FOR SEQUENCE                          
LIST0445 DS    0H                                                               
         OI    DMINBTS,X'08'       READ DELETED                                 
         MVC   HIGHKEY,KEY                                                      
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'FF'-X'08' RESET                                        
                                                                                
         L     R5,AIO              IN CASE RECORD IS DELETED BUT THE            
         USING RCONREC,R5             PASSIVE POINTERS AREN'T                   
         TM    RCONCNTL,X'80'      SKIP IF RECORD IS REALLY MARKED              
         BO    LIST0400               FOR DELETION                              
*                                                                               
         CLC   =C'ALL',CONREC      ALLHIST REQUEST?                             
         BE    LIST0447            YES - DON'T FILTER OUT NETWORK ORDS          
*                                                                               
         CLC   =C'BACKNET',CONREC  BACKNET REQUEST?                             
         BE    LIST0446            YES - ONLY PROCESS NETWORK ORDS              
*                                                                               
*       "BACKHIST"                 NO  - DON'T TAKE NEXTWORK ORDERS             
*                                                                               
         CLI   RCONTYPE,C'N'       DON'T TAKE OVER NETWORK ORDERS               
         BE    LIST0400                                                         
         CLI   RCONTYPE,C'X'       DON'T TAKE OVER NETWORK ORDERS               
         BE    LIST0400                                                         
         B     LIST0447            NOT NETWORK: ACCEPT IT SO FAR                
LIST0446 EQU   *                                                                
*                                                                               
*       "BACKNET "                 ONLY  TAKE NEXTWORK ORDERS                   
*                                                                               
         CLI   RCONTYPE,C'N'       NETWORK: TAKE IT OVER                        
         BE    LIST0447                                                         
         CLI   RCONTYPE,C'X'       NETWORK: TAKE IT OVER                        
         BE    LIST0447                                                         
         B     LIST0400            NOT NETWORK: SKIP IT                         
*                                                                               
*        NEED TO CHECK SOFT CONTYPE FOR NETWORK HERE                            
*                                                                               
*        CHECK CONFIRM/VERSION/WIP STATUS:  SKIP WIP'S                          
*                                                                               
*                                                                               
*   TEST SKIP WIP                                                               
***>>>   B     LIST0460                                                         
*   TEST SKIP WIP END                                                           
*                                                                               
LIST0447 DS    0H                                                               
         LA    RF,RCONELEM         FIND X'20' ELEMENT                           
LIST0450 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    LIST0400            YES - SKIP THIS ORDER                        
*                                     SHOULD HAVE BEEN THERE                    
         CLI   0(RF),X'20'         SEND INFO ELEMENT?                           
         BE    LIST0454            YES - CHECK IT                               
         ZIC   RE,1(RF)                                                         
         AR    RF,RE               BUMP TO NEXT ELEMENT                         
         B     LIST0450            GO BACK FOR NEXT                             
LIST0454 EQU   *                                                                
         MVI   CNFORVER,0          CLEAR TO 'CONFIRMED ORDER' STATUS            
         TM    RCONSENF-RCONSEND(RF),X'02'                                      
*                                  LAST CONFIRMED BY STATION?                   
         BO    LIST0460            YES - ACCEPT IT                              
         MVI   CNFORVER,1          SET TO 'VERSION' STATUS                      
         TM    RCONSENF-RCONSEND(RF),X'20'+X'10'                                
*                                  STA # ADVANCED?                              
         BO    LIST0460            BOTH SIDES ON:  ACCEPT IT                    
         CLC   =C'ALL',CONREC      ALLHIST REQUEST?                             
         BNE   LIST0400            NO  - IN PROCESS ON ONE SIDE OR              
         B     LIST0460            YES - ACCEPT 'IN PROGRESS'                   
*                                     OTHER:  SKIP IT                           
***      CLC   =C'ACC-',RCONBUYR   ACCOUNTING CONTRACT?                         
***      BE    LIST0460            YES - ACCEPT 'IN PROGRESS'                   
***      B     LIST0400            NO  - IN PROCESS ON ONE SIDE OR              
*                                     OTHER:  SKIP IT                           
LIST0460 DS    0H                                                               
         CLI   CLEARFLG,C'Y'                                                    
         BE    LIST0500                                                         
         LR    R3,R2               SAVE OFF R2                                  
                                                                                
LIST0480 DS    0H                                                               
         LR    R2,R3               RESTORE R2                                   
         MVI   CLEARFLG,C'Y'                                                    
                                                                                
LIST0500 DS    0H                                                               
         OC    FIRSTKEY,FIRSTKEY                                                
         BNZ   LIST0520                                                         
         MVC   FIRSTKEY,KEY        SAVE KEY TO BE DISPLAYED                     
LIST0520 DS    0H                                                               
         OC    SCRNKEY,SCRNKEY                                                  
         BNZ   LIST0540                                                         
         MVC   SCRNKEY,KEY         SAVE KEY TO BE DISPLAYED                     
LIST0540 DS    0H                                                               
*                                                                               
         L     RF,CONCOUNT         COUNT UP THE CONTRACTS                       
         LA    RF,1(RF)                                                         
         ST    RF,CONCOUNT                                                      
         CLC   CONCOUNT,=F'5'      SCREEN FULL?                                 
         BH    LIST0550                                                         
         GOTO1 DISCON,DMCB,(R2)    DISPLAY THE CONTRACT                         
         LA    R2,CTOCON1H-CTOCONH(R2)                                          
*                                  BUMP TO NEXT SCREEN FIELD                    
                                                                                
LIST0550 EQU   *                                                                
         CLI   CONCTFLG,C'Y'       CONTRACT COUNTER FLAG SET 'YES'?             
         BE    LIST0555            YES - DON'T STOP AT END OF SCREEN            
         LA    RF,CTOTAGH          A(END OF SCREEN)                             
         CR    R2,RF               END OF SCREEN REACHED?                       
         BNL   LIST0620            YES                                          
LIST0555 EQU   *                                                                
         XC    KEY,KEY             CLEAR KEY DISPLAYED                          
         MVC   KEY(16),HIGHKEY     RESTORE KEY THRU CONTRACT #                  
         MVI   KEY+16,10           SET TO SKIP THIS SET OF KEYS                 
         B     LIST0240            NO  - GO BACK FOR ANOTHER CONTRACT           
                                                                                
LIST0560 DS    0H                                                               
                                                                                
LIST0580 DS    0H                                                               
         CLC   KEY(RCONSTEM-RCONSTYP),KEYSAVE                                   
         BE    LIST0440                                                         
                                                                                
LIST0600 DS    0H                  WE'VE HIT THE LAST CONTRACT                  
         L     RF,ACONTROL         SET A(CONTROL SAVE AREA)                     
         MVC   0(LCTLAREA,RF),CTRLAREA                                          
*                                  SAVE CONTROL AREA FOR RESTORE                
*                                     IF NECESSARY                              
         XC    CTOCNT(18),CTOCNT   CLEAR COUNTER                                
         CLI   CONCTFLG,C'Y'       COUNT CONTRACT OPTION?                       
         BNE   LIST0610            NO                                           
         MVC   CTOCNT(6),=C'COUNT:'                                             
         EDIT  CONCOUNT,(6,CTOCNT+7)                                            
         MVC   SAVEKEY,CON#KEY     INSERT LAST CONTRACT NUMBER                  
LIST0610 EQU   *                                                                
         FOUT  CTOCNTH             SET FOR TRANSMIT                             
         CLI   CONCTFLG,C'Y'       COUNT CONTRACT OPTION?                       
         BNE   LIST0612            NO  - DON'T TEST CONTRACT COUNT              
         CLC   CONCOUNT,=F'5'      YES - MORE THAN FIVE CONTRACTS?              
         BH    ENDLIST             YES - DON'T SET 'LAST CON FOUND'             
LIST0612 EQU   *                                                                
         MVI   LASTSCRN,C'Y'                                                    
LIST0615 EQU   *                                                                
         MVC   SAVEKEY,FIRSTKEY    NEXT SCREEN GOES TO THE BEGINNNING           
*** TESTING                                                                     
***      B     ENDLIST                                                          
*** TESTING                                                                     
         MVC   CTOLAST+1(2),=X'0101' RETRANSMIT THE ENTIRE SCREEN               
         B     ENDLIST                                                          
                                                                                
LIST0620 DS    0H                                                               
*                                                                               
         BAS   RE,SWTGTREP         SWITCH BACK TO ORIGINAL REP                  
*                                                                               
         MVI   LASTSCRN,C'N'                                                    
         CLI   MAXIOFLG,0          MAX IO INDICATOR SET?                        
         BNE   LIST0630            YES                                          
         MVC   SAVEKEY,CON#KEY     NO  - INSERT LAST CONTRACT NUMBER            
LIST0630 EQU   *                                                                
***      MVC   SAVEKEY,CON#KEY     INSERT LAST CONTRACT NUMBER                  
         MVC   CTOLAST+1(2),=X'0101' RETRANSMIT THE ENTIRE SCREEN               
         L     RF,ACONTROL         SET A(CONTROL SAVE AREA)                     
         MVC   0(LCTLAREA,RF),CTRLAREA                                          
*                                  SAVE CONTROL AREA FOR RESTORE                
*                                     IF NECESSARY                              
         CLI   MAXIOFLG,0          MAX IO INDICATOR SET?                        
         BE    LIST0635            NO                                           
         EDIT  MAXIOCTR,(3,CTOOPT+9)                                            
         MVI   CTOOPT+8,C' '       SET SPACE                                    
         FOUT  CTOOPTH                                                          
         OC    CONCOUNT,CONCOUNT   ANY ORDERS FOUND?                            
         BZ    MAXSERCH            NO  - SET 'SEARCHING' MESSAGE                
         B     MAXDATA             YES - SET 'PROCESS' MESSAGE                  
LIST0635 EQU   *                                                                
         CLI   UPDTDONE,C'Y'       UPDATE DONE?                                 
         BNE   NEXTLIST            NO  - NO PAPERWORK NEEDED                    
LIST0640 DS    0H                                                               
*                                                                               
         LR    R2,RA               USE R2 TO COVER THE ENTRY                    
         AH    R2,=AL2(SFMPROFS-CONHEADH)                                       
         USING SVDSECT,R2                                                       
         TM    SVPGPBIT,X'02'      7TH BIT (NO AUTO CONTRACTS)                  
         BO    NEXTUPDT            SET:  DON'T PRINT CONTRACT                   
*                                                                               
*                                                                               
*   NO PAPERWORK WILL BE GENERATED FOR BACKBILLING TAKEOVERS                    
*        DEACTIVATE NEXT STATEMENT TO REACTIVATE PAPERWORK                      
*        REACTIVATE CTOOPT TESTS ALSO                                           
*                                                                               
         B     NEXTUPDT            YES - NO 9E KEY TEST DONE                    
*                                                                               
**       CLC   =C'NOPW',CTOOPT     'SKIP PAPERWRK OPTION SET?                   
**       BE    NEXTUPDT            YES -                                        
**       CLC   =C'NOPE',CTOOPT     'SKIP PAPERWRK/NO 9E OPTION SET?             
**       BE    NEXTUPDT            YES -                                        
**       GOTO1 =A(PAPERWRK),DMCB,(RC),RR=Y                                      
*                                  PUT OUT PAPERWORK                            
**       B     NEXTUPDT            PUT OUT 'UPDATE MESSAGE'                     
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
****>>>                                                                         
***********************************************************************         
* SETFILES - SWITCH TO ALTERNATE FILES IF ACTION IS 'SELECT'                    
***********************************************************************         
SETFILES DS    0H                                                               
         BAS   RE,SWSRCREP         SWITCH TO SOURCE REP                         
         GOTO1 =A(SETCTYPS),DMCB,(RC),RR=Y                                      
*                                  SET UP CONTYPE EXCLUDE TABLE                 
*                                                                               
SFIL0080 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         TITLE 'ADD CONTRACT RECORD'                                            
***********************************************************************         
* ADDCONS  - PROCESS RECORD(S) SELECTED ON SCREEN                               
***********************************************************************         
ADDCONS  NTR1                                                                   
*                                                                               
         MVI   EFFELTS,C'N'        INITIALIZE EFFECTIVE DATE ELTS FLG           
*                                                                               
                                                                                
         XC    ANEWCMBO,ANEWCMBO   CLEAR ADDR POINTER                           
         MVC   ACMBNTRY,ACMBOTBL   SET A(1ST ENTRY IN COMBO TABLE)              
         MVI   UPDTDONE,C'N'       SET 'UPDATE DONE' = NO                       
         GOTO1 =A(VALSCRN),DMCB,(RC),RR=Y                                       
*                                  VALIDATE SCREEN FIELDS                       
         BNZ   ACON0420            ERROR FOUND - EXIT                           
*                                                                               
*   BECAUSE EACH ORDER MAY BE A COMBO, AND BECAUSE THE INITIAL ORDER            
*        OF THAT COMBO (THE ORDER APPEARING ON THE SELECT SCREEN)               
*        MAY *NOT* BE AN **ALTERNATE CALENDAR** STATION, BUT ONE OF             
*        THE OTHER PARTICIPATING STATIONS *MIGHT* BE, IT IS NECESSARY           
*        TO CHECK **ALL ORDERS, ALL THE TIME**.                                 
**********************************************************************          
*                                                                               
****     L     RE,AMISFLGS         ALTERNATE CALENDAR CHECK NEEDED?             
****     USING MISFLGS,RE                                                       
*                                                                               
****     CLI   ALTCAL,C'Y'         STATION NEEDS ALTERNATE CALENDAR?            
*                                                                               
****     DROP  RE                                                               
****     BNE   ACON0010            NO                                           
*                                                                               
**********************************************************************          
*                                                                               
         GOTO1 =A(CHEKALT),DMCB,(RC),RR=Y                                       
*                                  CHECK ALTERNATE CALENDAR VALIDITY            
         BZ    ACON0010            ALL ALTERNATE CALENDARS FOUND                
         LA    RE,CTOSTAH          ALTERNATE CALENDARS MISSING                  
         ST    RE,DUB              SET A(FIELD IN ERROR)                        
         MVC   RERROR,=AL2(777)    ALTERNATE CALENDARS MISSING                  
         B     ACON0420            ERROR FOUND - EXIT                           
ACON0010 EQU   *                                                                
         XC    WORK,WORK                                                        
         L     R5,AEQUITBL         SET A(EQUIVALENCY TABLE 1ST ENTRY)           
         ST    R5,AEQUNTRY         SAVE A(ENTRY IN PROGRESS)                    
         USING EQUITABL,R5                                                      
ACON0020 EQU   *                                                                
         MVI   CTLCOMBO,C'N'       SET 'NOT COMBO'                              
         TM    ETFLAG,X'80'        ENTRY 'SELECTED'?                            
         BO    ACON0060            YES - PROCESS IT                             
ACON0040 EQU   *                                                                
         LA    R5,LEQUITBL(R5)     BUMP TO NEXT ENTRY                           
         L     RF,ACMBNTRY         BUMP TO NEXT ENTRY IN COMBO TABLE            
         LA    RF,LCMBBUCK(RF)     BUMP TO CORRESP CMBO TABLE                   
         ST    RF,ACMBNTRY         PUT IT BACK                                  
         OC    0(6,R5),0(R5)       ANYTHING IN ENTRY?                           
         BZ    ACON0340            NO  - FINISHED WITH JOB                      
         B     ACON0020                                                         
ACON0060 EQU   *                                                                
         L     RF,ACMBNTRY         CHECK FOR COMBO ENTRY                        
         OC    0(LCMBNTRY,RF),0(RF)  ANY ENTRY?                                 
         BZ    ACON0080            NO                                           
         MVI   CTLCOMBO,C'Y'       SET COMBO ORDER 'YES'                        
ACON0080 EQU   *                                                                
         OI    ETFLAG,X'40'        MARK ENTRY 'PROCESSED'                       
         MVI   UPDTDONE,C'Y'       SET 'UPDATE DONE' = YES                      
ACON0100 EQU   *                                                                
         L     R4,AIO                                                           
         USING RCONREC,R4                                                       
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY+28(4),ETDSKADR  SET DISK ADDR OF CONTRACT                    
         MVC   SRCECON,ETOCONUM    INSERT CONTRACT NUMBER                       
*                                                                               
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
         GOTO1 GETREC,DMCB,RCONREC                                              
*                                  RETRIEVE THE CONTRACT RECORD                 
         MVC   OCONDATE,RCONDATE   SAVE CONTRACT FLIGHT DATES                   
         CLC   RCONPRD,SPACES      ANY PRODUCT CODE?                            
         BE    ACON0110            NO  - DON'T RETRIEVE                         
         CLC   ORIGUTL,SRCEUTL     SOURCE/TARGET ON SAME FILE?                  
         BNE   ACON0105            NO  - MUST REMOVE PRODUCT CODE               
         CLC   =C'ALL',CONREC      YES - ALLHIST REQUEST?                       
         BE    ACON0110            YES - DON'T REMOVE PRODUCT CODE              
ACON0105 EQU   *                                                                
         GOTO1 =A(PRODCODE),DMCB,(RC),RCONREC,RR=Y                              
*                                  CHECK/PROCESS PRODUCT CODE                   
ACON0110 EQU   *                                                                
*                                                                               
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*   CONTRACT FLIGHT VS TAKEOVER DATE:                                           
*        IF CONTRACT BEGINS NO EARLIER THAN THE TAKEOVER DATE,                  
*        NO ADJUSTMENT TO EITHER CONTRACT FLIGHT DATES OR BUYS MUST             
*        BE MADE.                                                               
*        IF CONTRACT CROSSED THE TAKEOVER DATE, CONTRACT FLIGHT DATES           
*        MUST BE CHANGED, AND THE CONTRACT MUST BE CHANGED TO SHOW              
*        THE BUCKETS GENERATED IN SUPPORT OF THE BUYS, WHICH MUST               
*        ALSO BE ADJUSTED TO THE CHANGED FLIGHT DATES.                          
*                                                                               
         MVI   CONSAVED,C'N'       SET CONTRACT SAVED TO 'NO'                   
*                                                                               
*********************************************************************           
*                                                                               
*   TREAT ALL ORDERS AS CROSSING TAKEOVER DATE, DUE TO ALTERNATE                
*        CALENDAR REQUIREMENTS.                                                 
*                                                                               
****>>>  CLC   RCONDATE(3),EFDTBIN                                              
*                                  CONTRACT CROSSES TAKEOVER DATE?              
****>>>  BNL   ACON0120            NO  -                                        
*                                                                               
*********************************************************************           
         MVI   CONSAVED,C'Y'       SET CONTRACT SAVED TO 'YES'                  
ACON0120 EQU   *                                                                
*                                                                               
         CLI   CTLCOMBO,C'N'       COMBO ORDER IN PROCESS?                      
         BE    ACON0140            NO                                           
         CLI   CTLCOMBO,C'Y'       COMBO BASE ORDER IN PROCESS?                 
         BE    ACON0140            YES - BASE ORDER NEEDS NEXT REP CON#         
*                                  NO  - COMBO NUM SET:  FORCE IN               
         MVC   RCONKCON,ETNCONUM         TO CONTRACT KEY                        
         MVC   TRGTCON,ETNCONUM    SAVE NEW CONTRACT NUMBER                     
         B     ACON0200                                                         
ACON0140 EQU   *                                                                
*                                                                               
         XC    COVSHT#,COVSHT#     CLEAR POSSIBLE COVERSHEET NUMBER             
         GOTO1 =A(NEXTCONS),DMCB,(RC),(R4),(R5),RR=Y                            
*                                  GET NEXT REP CONTRACT NUMBER                 
ACON0200 EQU   *                                                                
         CLC   =C'ALL',CONREC      ALLHIST REQUEST?                             
         BE    ACON0202            YES - DON'T RESET BUYER                      
         MVC   RCONBUYR,SPACES     CLEAR BUYER NAME                             
         MVC   RCONBUYR(07),=C'ACC-BBL'                                         
*                                  SET TO 'BACKBILLING/HISTORY'                 
ACON0202 EQU   *                                                                
         MVC   RCONKAGY(6),ETNAGYOF                                             
*                                  INSERT REPLACEMENT AGENCY/OFFICE             
         MVC   RCONKADV,ETNADVRT   INSERT REPLACEMENT ADVERTISER                
         MVC   RCONKOFF,ETNSPOFF   INSERT REPLACEMENT S/P OFFICE                
         MVC   RCONSAL,ETNSALEP    INSERT REPLACEMENT SALESPERSON               
         MVC   RCONTEM,ETNSPTEM    INSERT REPLACEMENT S/P TEAM                  
         MVC   RCONTYPE,ETNCNTYP   INSERT REPLACEMENT CONTRACT TYPE             
*                                                                               
         L     RF,AMISFLGS         MISCELLANEOUS FLAGS STORAGE AREA             
*                                                                               
         USING MISFLGS,RF                                                       
*                                                                               
         TM    MISFLAGS,X'40'      BOTH REPS OKAY INVOICE DATA?                 
*                                                                               
         DROP  RF                                                               
*                                                                               
         BO    ACON0205            YES - LEAVE IT IN RECORD                     
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'04',RCONREC),0,0                
*                                  DELETE ALL INVOICE BUCKETS                   
ACON0205 EQU   *                                                                
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'2C',RCONREC),0,0                
*                                  DELETE OLD TKO-BB/MOVE HIST ELT              
*                                  INSERT NEW TKO-BB/MOVE HIST ELT              
         XC    ELEM(RCONMMLQ),ELEM                                              
         MVI   ELEM,X'2C'                                                       
         MVI   ELEM+1,RCONMMLQ     NEW ELEMENT LENGTH                           
         GOTO1 DATCON,DMCB,(5,WORK),(2,ELEM+2)                                  
*                                  INSERT DATE OF TAKEOVER                      
         MVC   ELEM+4(4),SRCECON   INSERT ORIGINAL CONTRACT NUMBER              
         MVC   ELEM+8(2),SRCEREP   INSERT ORIGINAL SOURCE REP                   
         PRINT GEN                                                              
         GOTO1 ADDELEM                                                          
*                                  INSERT NEW ELEMENT INTO RECORD               
*                                  INSERT DARE TAKEOVER ELT                     
         XC    ELEM(RCONTKLQ),ELEM                                              
         MVI   ELEM,X'1C'                                                       
         MVI   ELEM+1,RCONTKLQ     NEW ELEMENT LENGTH                           
         MVC   ELEM+2(2),SRCEREP   INSERT SOURCE REP                            
         MVC   ELEM+4(4),SRCECON   INSERT ORIGINAL CONTRACT NUMBER              
         GOTO1 DATCON,DMCB,(5,WORK),(2,ELEM+8)                                  
*                                  INSERT DATE OF TAKEOVER                      
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,ELEM+10        INSERT TIME                                  
         GOTO1 ADDELEM                                                          
*                                  INSERT NEW ELEMENT INTO RECORD               
         PRINT NOGEN                                                            
*                                  FIND 9F EXTRA DESCRIPTOR ELT                 
*                                      DELETE SALES ASST                        
         LA    RF,RCONELEM                                                      
ACON0210 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    ACON0214            YES - NO X'9F' ELT                           
         CLI   0(RF),X'9F'         EXTRA DESCRIPTOR ELT?                        
         BE    ACON0212            YES                                          
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     ACON0210            GO BACK FOR NEXT                             
ACON0212 EQU   *                                                                
         CLC   =C'ALL',CONREC      ALLHIST REQUEST?                             
         BE    ACON0214            YES - LEAVE SALES ASSISTANT ALONE            
         XC    RCONXAST-RCONXXEL(9,RF),RCONXAST-RCONXXEL(RF)                    
*                                  RESET SALES ASSISTANT                        
ACON0214 EQU   *                                                                
         MVC   RCONKREP,TARGREP    INSERT TARGET REP INTO RECORD                
         XC    KEY,KEY                                                          
         MVC   KEY(27),RCONKEY     SET KEY FOR ADDREC                           
         GOTO1 =A(UPDTCON),DMCB,(RC),RR=Y                                       
*                                                                               
         CLI   CONSAVED,C'Y'       CONTRACT SAVED?                              
         BE    ACON0220            YES - DON'T ADD RIGHT NOW                    
         GOTO1 =A(CONCREAT),DMCB,(RC),RR=Y                                      
*                                  NO  - ADD RIGHT NOW AND ADD                  
*                                     PASSIVE POINTERS                          
ACON0220 DS    0H                                                               
*                                                                               
*   CONTRACT REMAINS IN AIO1.  IF BUYS ARE ADJUSTED, IT IS MODIFIED             
*        IN AIO1, THEN ADDED AT END, RATHER THAN PRIOR TO OTHER                 
*        RECORD PROCESSING.                                                     
*                                                                               
         GOTO1 =A(TAKEBUYS),DMCB,(RC),(R4),RR=Y                                 
*                                  PASS IN A(CONTRACT IN AIO1)                  
         GOTO1 =A(TAKECFC),DMCB,(RC),(R4),RR=Y                                  
*                                  PASS IN A(CONTRACT IN AIO1)                  
         CLI   CTLCOMBO,C'N'       COMBO IN PROGRESS?                           
         BE    ACON0225            NO  -  CHECK FOR COVERSHEET                  
         CLI   CTLCOMBO,C'Y'       COMBO/1ST ORDER IN PROGRESS?                 
         BNE   ACON0230            NO  -  SKIP COVERSHEET: ->                   
*                                     (COMBO, BUT NOT 1ST IN PROGRESS)          
ACON0225 EQU   *                                                                
         MVI   ELCODE,X'A6'        FIND COVERSHEET ELEMENT                      
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   ACON0230            NO COVERSHEET - SKIP NEXT RTN                
*                                                                               
         GOTO1 =A(TAKECOV),DMCB,(RC),(R4),RR=Y                                  
*                                  PASS IN A(CONTRACT IN AIO1)                  
ACON0230 EQU   *                                                                
*                                                                               
***>>>   GOTO1 =A(TAKEDARE),DMCB,(RC),(R4),(R5),RR=Y                            
*                                  R5 -> EQUIV TABLE ENTRY IN PROCESS           
***>>>   GOTO1 =A(TAKEMGS),DMCB,(RC),RR=Y                                       
*                                                                               
         CLI   CONSAVED,C'N'       CONTRACT SAVED?                              
         BE    ACON0240            NO  - ALREADY ADDED                          
*                                                                               
*                                  YES - ADD CONTRACTS AND POINTERS NOW         
         GOTO1 =A(CONCREAT),DMCB,(RC),RR=Y                                      
*                                                                               
ACON0240 DS    0H                                                               
*                                                                               
         BAS   RE,SWTGTREP         SWITCH BACK TO ORIGINAL REP                  
         CLI   CTLCOMBO,C'N'       NO COMBO WITH THIS ORDER?                    
         BE    ACON0320            NO COMBO - MOVE TO NEXT SCRN ENTRY           
         CLI   CTLCOMBO,C'Y'       YES - FIRST COMBO ORDER DONE?                
         BE    ACON0260            NO  - SET UP FOR 1ST                         
         ZIC   RF,CTLCOMBO         YES - BUMP TO NEXT ORDER                     
         B     ACON0280                                                         
ACON0260 EQU   *                                                                
         MVC   SAVCON#S(8),ETOCONUM                                             
         MVC   SAVGRPS(4),SRCEGRP  SAVE SOURCE/TARGET GROUPS                    
         MVC   SAVSTYPS(2),SRCETYPE                                             
*                                  SAVE SOURCE/TARGET TYPES                     
*                                  SAVE OLD/NEW CONTRACT NUMBERS                
         SR    RF,RF               SET COUNT TO ZERO                            
ACON0280 EQU   *                                                                
         LA    RF,1(RF)            BUMP TO 1ST/NEXT SLOT                        
         STC   RF,CTLCOMBO         STORE IT BACK                                
         CLI   CTLCOMBO,3          THREE PARTICIPATING CON#S DONE?              
         BH    ACON0300            YES - DON'T DO ANY MORE FOR                  
*                                     THIS BASE CONTRACT                        
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         SR    RE,RE                                                            
         M     RE,=F'19'           CALC DISPL INTO CMBO TABL                    
         L     RE,ACMBNTRY         DOES A COMBO NEED DOING?                     
         AR    RE,RF               DISPLACE TO ENTRY                            
         OC    0(LCMBNTRY,RE),0(RE)  ANY ENTRY HERE?                            
         BZ    ACON0300            NO  - FINISHED                               
         MVC   ETOCONUM(8),ACMBOCON(RE)                                         
*                                  INSERT COMBO OLD/NEW ORDER #S                
         MVC   SRCEGRP(4),ACMBOGRP(RE)                                          
*                                  INSERT COMBO ORDER GROUP/SUBGRPS             
         MVC   SRCETYPE(2),ACMBOSTY(RE)                                         
*                                  INSERT COMBO ORDER STATION TYPES             
*                                                                               
*   RETRIEVE KEY FOR COMBO ORDER TO GET D/A INTO TABLE.                         
*                                                                               
         XC    KEY,KEY                                                          
         ZAP   DUB(5),=P'99999999'                                              
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),ETOCONUM    INSERT ORIGINAL ORDER NUMBER                 
         SP    DUB(5),WORK(5)                                                   
         MVO   WORK(5),DUB(5)                                                   
         MVI   KEY,X'8C'           SET UP KEY                                   
         MVC   KEY+21(2),SRCEREP   INSERT SOURCE REP CODE                       
         MVC   KEY+23(4),WORK      INSERT KEY                                   
*                                                                               
         BAS   RE,SWSRCREP         SWITCH TO SOURCE REP                         
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         BAS   RE,SWTGTREP         SWITCH TO TARGET REP                         
*                                                                               
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                NOT FOUND?                                   
         MVC   ETDSKADR,KEY+28     INSERT DISK ADDR INTO TABLE                  
*                                                                               
*   NOTE:  NO ATTEMPT IS MADE TO SAVE THE ORIGINAL D/A, AS IT                   
*        WILL NOT BE USED AGAIN.                                                
*                                                                               
         B     ACON0100            GO BACK AND PROCESS IT                       
ACON0300 EQU   *                                                                
         MVC   ETOCONUM(8),SAVCON#S                                             
*                                  RESET ORIGINAL CON #S                        
         MVC   SRCEGRP(4),SAVGRPS  RESET ORIGINAL GROUP/SUBGROUPS               
         MVC   SRCETYPE(2),SAVSTYPS                                             
*                                  RESET ORIGINAL STATION TYPES                 
ACON0320 EQU   *                                                                
         B     ACON0040            GO BACK FOR NEXT SELECT                      
*                                                                               
ACON0340 EQU   *                                                                
         MVC   CONACT,SPACES                                                    
         MVC   CONACT(4),=C'LIST'  TURN IT TO LIST                              
         LA    RF,CONACTH          SET FLAGS ON FIELD                           
         OI    6(RF),X'80'         SET TO 'TRANSMIT'                            
*                                                                               
         DROP  R4,R5                                                            
*                                                                               
         L     R4,AEQUITBL         SET A(EQUIVALENCY TABLE)                     
         USING EQUITABL,R4                                                      
*                                                                               
         LA    R3,CTOSELH          SET A(1ST SCREEN SELECT FIELD)               
ACON0360 EQU   *                                                                
         CLI   TAKEALL,C'Y'        ALL CONTRACTS SELECTED?                      
         BE    ACON0380            YES                                          
         CLI   5(R3),1             USER ONLY INPUT 'S' - NO 'S+'                
         BNE   ACON0400            ONLY INPUT LEN OK                            
         CLI   8(R3),C'S'          CONTRACT SELECTED?                           
         BNE   ACON0400            NO  - SKIP IT                                
ACON0380 EQU   *                                                                
         OC    ETNCONUM,ETNCONUM   ANYTHING IN FIELD?                           
         BZ    ACON0400            NO  - DON'T FLAG AS 'SELECTED'               
         MVC   8(2,R3),=C'*S'      YES - INDICATE 'PROCESSED'                   
         FOUT  (R3)                                                             
         LR    R5,R3               SET A(SELECT FIELD)                          
         LA    R5,CTONCNH-CTOSELH+8(R5)                                         
*                                  DISPLACE TO NEW CON# FIELD                   
*                                     DATA FIELD/NOT FIELD HDR                  
         GOTO1 HEXOUT,DMCB,ETNCONUM,(R5),4,=C'TOG'                              
         FOUT  (R5)                                                             
ACON0400 EQU   *                                                                
         LA    R4,LEQUITBL(R4)     BUMP TO NEXT EQUIV ENTRY                     
         LA    R3,CTOSEL1H-CTOSELH(R3)                                          
*                                  BUMP TO NEXT SELECT FIELD                    
         LA    RF,CTOTAGH          CHECK FOR END OF SCREEN                      
         CR    R3,RF               END OF SCREEN REACHED?                       
         BL    ACON0360            NO  - GO BACK AND CHECK NEXT                 
*                                                                               
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
         GOTO1 =A(SETEQUIV),DMCB,(RC),RR=Y                                      
*                                  SCAN EQUIV TABLE, GENERATE NEW               
*                                     EQUIVALENCY RECORDS                       
         SR    R0,R0               YES - SET CC ZERO FOR RETURN                 
*                                                                               
         DROP  R4                                                               
ACON0420 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
****>>>                                                                         
*              ROUTINE TO FOUT BLANKS                                           
         SPACE 1                                                                
*              PARAMETER  1        A(FIRST FIELD)                               
*              PARAMETER  2        A(END-ADDR)  EX=BUYLAST                      
*              PARAMETER  3        0=DO NOT FOUT IF SPACES                      
*                                  1=FOUT IF NOT SPACES                         
*              PARAMETER  4        0=CLEAR PROTECTED FIELDS ALSO                
*                                  1=DON'T CLEAR PROTECTED FIELDS               
         SPACE 1                                                                
FOUTBLK  NTR1                                                                   
         LM    R2,R5,0(R1)                                                      
         MVI   WORK2,C' '                                                       
         MVC   WORK2+1(79),WORK2                                                
         SPACE 1                                                                
FOUT1    ZIC   RE,0(R2)                                                         
         LTR   R5,R5               CLEAR PROTECTED FIELDS ALSO?                 
         BZ    FOUT1A              YES - SKIP PROTECTED TEST                    
         TM    1(R2),X'20'         NO  - PROTECTED?                             
         BO    FOUT9               YES - DON'T CLEAR                            
FOUT1A   EQU   *                                                                
         SPACE 1                                                                
*                                                                               
*        SPECIAL TEST TO LEAVE SPECIFIC PROTECTED FIELDS ALONE                  
*                                                                               
         CLC   =C'NEW->',8(R2)     NEW LINE INDICATOR?                          
         BE    FOUT9               YES - DON'T CLEAR IT                         
         LR    RF,RE                                                            
         SH    RF,=H'9'                                                         
         TM    1(R2),X'02'         EXTENDED FLD HDR?                            
         BZ    *+8                                                              
         SH    RF,=H'8'                                                         
         SPACE 1                                                                
         EX    RF,ORSPAC                                                        
         EX    RF,COMPSPAC                                                      
         BE    FOUT9               ALREADY                                      
         LTR   R4,R4                                                            
         BP    *+8                 SENDING  NON MYSPACES DATA                   
         EX    RF,MOVESPAC         CLEARING SCREEN                              
         FOUT  (R2)                                                             
         SPACE 1                                                                
FOUT9    LA    R2,0(RE,R2)         NEXT FIELD                                   
         CR    R2,R3               LAST FIELD                                   
         BL    FOUT1                                                            
         XIT1                                                                   
         SPACE 1                                                                
ORSPAC   OC    8(0,R2),WORK2                                                    
COMPSPAC CLC   8(0,R2),WORK2                                                    
MOVESPAC MVC   8(0,R2),WORK2                                                    
         EJECT                                                                  
****>>>                                                                         
**********************************************************************          
*  SWITCH TO SOURCE REP FILE                                         *          
**********************************************************************          
SWSRCREP NTR1                                                                   
         CLC   ORIGUTL,SRCEUTL     SOURCE/TARGET ON SAME FILE?                  
         BE    SWSR0020            YES - DON'T SWITCH                           
         GOTO1 VSWITCH,DMCB,(SRCEUTL,X'FFFFFFFF'),0                             
         CLI   4(R1),0             SWITCHED OKAY?                               
         BE    SWSR0020            YES - NOW READ CONTRACT RECORDS              
         CLI   4(R1),2             NO  - SYSTEM NOT OPENED?                     
         BE    *+6                 YES                                          
         DC    H'0'                NO  - OTHER REASON                           
*                                                                               
         B     SWTGTSET                                                         
*                                                                               
SWSR0020 DS    0H                                                               
         XIT1                      SWITCHED TO SOURCE REP                       
*                                                                               
SWTGTSET NTR1                      ERROR: SWITCH BACK TO TARGET REP             
*        GOTO1 VSWITCH,DMCB,(X'08',X'FFFFFFFF'),0                               
         GOTO1 VSWITCH,DMCB,=C'REP ',0                                          
         CLI   4(R1),0             SWITCHED BACK OKAY?                          
         BE    SRCCLOSD            YES - EXIT WITH MESSAGE                      
         DC    H'0'                NO  - ABORT                                  
*                                                                               
SWTGTREP NTR1                      SWITCH BACK TO TARGET REP                    
         CLC   ORIGUTL,SRCEUTL     SOURCE/TARGET ON SAME FILE?                  
         BE    SWTG0020            YES - DON'T SWITCH                           
*        GOTO1 VSWITCH,DMCB,(X'08',X'FFFFFFFF'),0                               
         GOTO1 VSWITCH,DMCB,=C'REP ',0                                          
         CLI   4(R1),0             SWITCHED BACK OKAY?                          
         BE    SWTG0020            YES - EXIT ROUTINE                           
         DC    H'0'                NO  - ABORT                                  
SWTG0020 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
****>>>                                                                         
****>>>                                                                         
*                                                                               
*   CHEKAGI: ACCESS THE SOURCE REP FILE FOR THIS AGENCY.                        
*        IF NOT FOUND, CC NOT ZERO WILL PRODUCE ERROR MESSAGE.                  
*                                                                               
CHEKAGI  NTR1                                                                   
*                                                                               
*                                                                               
         LA    R2,CTOAGIH          SET A(AGENCY FILTER HEADER)                  
         CLI   8(R2),C'='          REQUEST FOR BROWSER CALL?                    
         BNE   CAGI0020            NO                                           
         MVC   DMCB+20(1),SRCEUTL                                               
         MVC   DMCB+21(1),ORIGUTL                                               
         MVC   DMCB+22(2),SRCEREP                                               
         GOTOX (RFBROWSE,VREPFACS),DMCB,ACOMFACS,BASERD,(R2),0,        +        
               (X'40',C' AGY'),                                                 
**       GOTOX =V(REBROWSE),DMCB,ACOMFACS,BASERD,(R2),0,(0,C' AGY'),, +         
**             RR=Y                                                             
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
CAGI0020 EQU   *                                                                
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'           SET KEY FOR AGENCY RECORD                    
         MVC   KEY+19(6),SPACES    SPACE FILL AGY PORTION                       
         LA    RF,CTOAGIH          SET A(AGENCY FILTER FIELD)                   
         ZIC   R0,5(RF)            SET COUNT FOR LOOP - USE FIELD LEN           
         LA    RF,8(RF)            BUMP TO DATA IN FIELD                        
         LA    RE,KEY+19           SET A(KEY FIELD)                             
CAGI0040 EQU   *                                                                
         CLI   0(RF),C'-'          SEPARATOR ENCOUNTERED?                       
         BNE   CAGI0080            NO  - MOVE CHARACTER                         
         LA    RF,1(RF)            YES - SKIP SEPARATOR                         
         LA    RE,KEY+23           NEXT CHARS MUST BE OFFICE                    
*                                                                               
*    NOTE:  FIELD MAY RUN OVER ACTUAL KEY.  THIS SHOULDN'T BE                   
*        A PROBLEM, AND IT SHOULD BE CLEANED UP WITHIN KEY BUILDING             
*                                                                               
CAGI0080 EQU   *                                                                
         MVC   0(1,RE),0(RF)       MOVE CHARACTER TO KEY                        
         LA    RF,1(RF)            BUMP TO NEXT POSITION                        
         LA    RE,1(RE)            BUMP TO NEXT POSITION                        
         BCT   R0,CAGI0040         GO BACK FOR NEXT CHARACTER                   
*                                  ALL CHARS PROCESSED                          
         MVC   KEY+25(2),SRCEREP   INSERT SOURCE REP CODE                       
         GOTO1 HIGH                READ FOR KEY                                 
*                                                                               
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
*                                                                               
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    CAGI0160            YES - KEY VALID AS ENTERED                   
CAGI0120 EQU   *                   NO  - KEY NOT FOUND                          
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     CAGI0200                                                         
CAGI0160 EQU   *                                                                
         MVC   SAVEAGY(6),KEY+19   SAVE KEY FOR FILTERING                       
CAGI0180 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
CAGI0200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
****>>>                                                                         
****>>>                                                                         
*                                                                               
*   CHEKSALL: ACCESS THE REP SALESPERSON RECORD TO VALIDATE S/P                 
*        EXISTENCE.                                                             
*                                                                               
CHEKSALL NTR1                                                                   
*                                                                               
         MVI   BYTE,0              CLEAR S/P ERROR FLAG                         
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,6               INSERT RECORD TYPE                           
         MVC   KEY+22(2),TWAAGY    INSERT REP CODE                              
         LA    R2,CTOSALH          A(SALESPERSON FIELD)                         
         MVC   KEY+24(3),SPACES    CLEAR CODE IN KEY                            
         ZIC   RF,5(R2)            SET L(INPUT)                                 
         BCTR  RF,0                SUBTRACT 1 FOR EX                            
         EX    RF,CSAL0020         MOVE CODE BY LENGTH                          
         B     CSAL0040                                                         
CSAL0020 MVC   KEY+24(0),8(R2)     MOVE CODE BY LENGTH                          
CSAL0040 EQU   *                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     RECORD FOUND?                                
         BE    CSAL0200            YES - ACCEPTED                               
CSAL0160 EQU   *                                                                
         LTR   RB,RB               NO  - SET CC NOT = ZERO                      
         B     CSAL0600                                                         
CSAL0200 EQU   *                                                                
         MVC   SAVEAIO,AIO         SAVE ORIGINAL AIO                            
         MVC   AIO,AIO2                                                         
         L     R3,AIO2             USE SECOND IO AREA FOR TKO                   
         USING RSALREC,R3          RETRIEVE S/P REC INTO IO AREA 2              
*                                                                               
         GOTO1 GETREC,DMCB,RSALREC                                              
*                                                                               
         MVC   AIO,SAVEAIO         RESET ORIGINAL AIO                           
*                                                                               
         OC    RSALLEAV,RSALLEAV   S/P LEAVE DATE?                              
         BZ    CSAL0210            NO  - CONTINUE                               
         MVI   BYTE,1              SET S/P LEAVE FLAG                           
         LTR   RB,RB               NO  - SET CC NOT = ZERO                      
         B     CSAL0600                                                         
CSAL0210 EQU   *                                                                
         OC    OFFTEAMS,OFFTEAMS   ANY OFFICE/TEAMS?                            
         BZ    CSAL0280            NO  - USE AS IS                              
         LA    R4,15               15 SETS MAXIMUM                              
         LA    R5,OFFTEAMS                                                      
*                                                                               
CSAL0220 EQU   *                                                                
         CLC   RSALOFF,0(R5)       OFFICE IN LIST?                              
         BE    CSAL0240            YES                                          
         LA    R5,4(R5)            BUMP TO NEXT ENTRY                           
         BCT   R4,CSAL0220                                                      
         B     CSAL0280            OFFICE NOT IN LIST - USE                     
*                                                                               
*                                  OFFICE MAY BE IN LIST MORE                   
*                                     THAN ONE TIME                             
*                                                                               
CSAL0240 DS    0H                  OFFICE IS IN OFFTEAM LIST,                   
         CLC   RSALOFF,0(R5)       OFFICE IN LIST?                              
         BNE   CSAL0260            NO                                           
         CLC   RSALTEAM,2(R5)      YES - TEAM FOR THIS OFFICE?                  
         BNE   CSAL0260            NO                                           
         MVC   SAVESPOF,RSALOFF    SAVE SALESPERSON OFFICE                      
         B     CSAL0280                                                         
CSAL0260 LA    R5,4(R5)            NO  - BUMP TO NEXT OFFICE                    
         BCT   R4,CSAL0240         GO BACK FOR NEXT OFF/TEAM                    
         LA    RF,288              TEAM NOT ALLOWED TO SELL OFFICE              
         ST    RF,DUB              SAVE ERROR                                   
         LTR   RB,RB               SET CC NOT ZERO                              
         B     CSAL0600                                                         
CSAL0280 EQU   *                   MATCHUP:  USE IT                             
         MVC   SAVESALP,KEY+24     SAVE S/P DEFAULT VALUE                       
         MVC   SAVESPOF,RSALOFF    SAVE SALESPERSON OFFICE                      
         B     CSAL0800                                                         
CSAL0600 EQU   *                                                                
         NI    CTOSALH+4,X'FF'-X'20'                                            
*                                  TURN OFF PREVALID                            
CSAL0800 EQU   *                                                                
*                                                                               
         DROP  R3                                                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CHEKTYPE: ACCESS THE CONTRACT TYPE RECORD TO VALIDATE EXISTENCE             
*                                                                               
CHEKTYPE NTR1                                                                   
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'32'           INSERT RECORD TYPE                           
         MVC   KEY+24(2),TWAAGY    INSERT REP CODE                              
         MVC   KEY+26(1),CTOTYP    INSERT CONTRACT TYPE IN KEY                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     RECORD FOUND?                                
         BE    CTYP0200            YES - ACCEPTED                               
         NI    CTOTYPH+4,X'FF'-X'20'                                            
*                                  TURN OFF PREVALID                            
         LTR   RB,RB               NO  - SET CC NOT = ZERO                      
         B     CTYP0600                                                         
CTYP0200 EQU   *                                                                
         MVC   SAVECTYP,KEY+26     SAVE CONTYPE DEFAULT VALUE                   
         SR    R0,R0               SET CC = ZERO                                
CTYP0600 EQU   *                                                                
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CHEKSSTA: ACCESS THE SOURCE STA RECORD TO DETERMINE IF STATION              
*        IS CLEARED FOR TAKEOVER PROCESSING.  THIS CODE CHECKS THE              
*        SOURCE REP'S STATION RECORD, WHERE                                     
*              1.  THE STATION MUST EXIST                                       
*              2.  THE LEAVE DATE MUST BE EQUAL OR BEFORE THE                   
*                  EFFECTIVE DATE                                               
*              3.  THE NEW REP MUST BE THE TARGET REP                           
*                                                                               
*                                                                               
CHEKSSTA NTR1                                                                   
*                                                                               
         MVI   BARFLAG,X'00'       CLEAR INDICATOR                              
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,2               INSERT RECORD TYPE                           
         MVC   KEY+20(2),SRCEREP   INSERT SOURCE REP CODE                       
         MVC   KEY+22(5),SRCESTAT  INSERT STATION CALL LETTERS                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     RECORD FOUND?                                
         BNE   SSTA0220            NO  - EXIT WITH ERROR                        
         GOTO1 GETREC              RETRIEVE STATION RECORD                      
*                                                                               
*   CHANGE OLD/NEW FROM 'CML' TO 'CUM' FOR LATER TESTS                          
*                                                                               
         L     R6,AIO                                                           
         USING RSTAFNEL,R6                                                      
         MVI   ELCODE,X'0C'        GET FORMER/NEW REP ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   SSTA0040            NOT FOUND - REJECT                           
         CLC   RSTAFNFO,=C'CML'    FORMER REP = CUM?                            
         BNE   SSTA0020            NO                                           
         MVC   RSTAFNFO,=C'CUM'    YES - REPLACE WITH SIGNON                    
SSTA0020 EQU   *                                                                
         CLC   RSTAFNNE,=C'CML'    FORMER REP = CUM?                            
         BNE   SSTA0040            NO                                           
         MVC   RSTAFNNE,=C'CUM'    YES - REPLACE WITH SIGNON                    
*                                                                               
         DROP  R6                                                               
*                                                                               
*   DETERMINE IF OLD REP HAS BARRED TAKEOVER FROM THIS STATION                  
*                                                                               
SSTA0040 EQU   *                                                                
         L     R6,AIO                                                           
         USING RSTAXXEL,R6                                                      
         MVI   ELCODE,X'08'        GET EXTENDED DESCRIPTION ELEMENT             
         BAS   RE,GETEL                                                         
         BNE   SSTA0060            NOT FOUND - PERMIT                           
*                                     SHOULDN'T REALLY HAPPEN                   
*                                                                               
         TM    RSTAOPTA,X'04'      STATION BARRED BY OLD REP?                   
         BNO   SSTA0060            NO                                           
         OI    BARFLAG,X'80'       YES - SET INDICATOR                          
         B     SSTA0220            EXIT                                         
SSTA0060 EQU   *                                                                
         L     R5,AIO                                                           
         USING RSTAREC,R5                                                       
         OC    RSTAEND,RSTAEND     LEAVE DATE ENTERED?                          
         BZ    SSTA0220            NO  - STA NOT MARKED FOR TAKEOVER            
         CLC   SAVSTDAT,RSTAEND                                                 
         BL    SSTA0220            EFFECTIVE DATE BEFORE LEAVE:                 
*                                     REJECTED                                  
*                                                                               
*   TEST:   INSERT A HISTORY DATE                                               
****     MVC   RSTAHIST,=X'C39D'                                                
*   TEST:   INSERT A HISTORY DATE:  END TEST                                    
*                                                                               
         CLC   =C'ALL',CONREC      ALLHIST REQUEST?                             
         BE    SSTA0080            YES - IGNORE DATE TEST                       
*                                                                               
         CLC   HISTDATE,RSTAHIST   SRCE/TRGT HIST DATES AGREE?                  
         BE    SSTA0080                                                         
         MVC   DUB(4),=C'HSDT'     NO  - REJECT WITH ERROR                      
         B     SSTA0220                                                         
SSTA0080 EQU   *                                                                
         CLC   =C'ALL',CONREC      ALLHIST REQUEST?                             
         BNE   SSTA0082            NO                                           
         L     RF,AMISFLGS         MISCELLANEOUS FLAGS STORAGE AREA             
*                                                                               
         USING MISFLGS,RF                                                       
*                                                                               
         OI    MISFLAGS,X'40'      SET TO ACCEPT INVOICE DATA                   
*                                                                               
         DROP  RF                                                               
*                                                                               
         B     SSTA0096            SKIP ALL OTHER TESTS                         
SSTA0082 EQU   *                                                                
         TM    RSTAFLGS,X'80'      SOURCE OKAYS INVOICE BUCKET TAKE?            
         BNO   SSTA0090            NO                                           
         L     RF,AMISFLGS         MISCELLANEOUS FLAGS STORAGE AREA             
*                                                                               
         USING MISFLGS,RF                                                       
*                                                                               
         OI    MISFLAGS,X'40'      SET TO ACCEPT INVOICE DATA                   
*                                                                               
*                                                                               
         MVI   SINVTAK,C'Y'        YES - SET SOURCE FLAG TO 'YES'               
         DROP  RF                                                               
SSTA0090 EQU   *                                                                
         L     RF,AMISFLGS         MISCELLANEOUS FLAGS STORAGE AREA             
*                                                                               
         USING MISFLGS,RF                                                       
*                                                                               
         OI    MISFLAGS,X'40'      SET TO ACCEPT INVOICE DATA                   
*                                                                               
         OC    SINVTAK(2),SINVTAK SOURCE/TARGET INVOICE BOTH ZERO?              
         BZ    SSTA0096            YES - BOTH SIDES SET SAME                    
         CLC   SINVTAK,TINVTAK     SOURCE/TARGET SET TO 'Y'?                    
         BE    SSTA0096            YES - BOTH SIDES SET SAME                    
         MVC   DUB(4),=C'INVT'     NO  - REJECT WITH ERROR                      
         B     SSTA0220                                                         
*                                                                               
         DROP  R5,RF                                                            
SSTA0096 EQU   *                                                                
*                                                                               
         L     R6,AIO                                                           
         USING RSTAFNEL,R6                                                      
         MVI   ELCODE,X'0C'        GET FORMER/NEW REP ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   SSTA0220            NOT FOUND - REJECT                           
         LA    RF,2                SET COMPARE LENGTH = 3 CHARS                 
         CLI   RSTAFNNE+2,C' '     LAST CHAR NEW REP = SPACE?                   
         BE    SSTA0100            YES - DROP COMPARE 1 POSITION                
         CLI   RSTAFNNE+2,X'00'    LAST CHAR NEW REP = BINARY ZERO?             
         BNE   SSTA0120            NO  -                                        
SSTA0100 EQU   *                                                                
         LA    RF,1                SET COMPARE LENGTH = 2 CHARS                 
SSTA0120 EQU   *                                                                
         EX    RF,SSTA0140         COMPARE BY LENGTH                            
         B     SSTA0160                                                         
SSTA0140 CLC   RSTAFNNE(0),TRGTREP                                              
*                                  NEW REP VS SCREEN TARGET REP                 
SSTA0160 EQU   *                                                                
         BE    SSTA0240            SAME REP - ACCEPT                            
*                                                                               
*                                                                               
*   NOT FOUND:  DO SECONDARY COMPARISON                                         
*        TABLE SETUP:                                                           
*              CHARS 0 - 2 = DARE TABLE ENTRY/OLD-NEW STATION                   
*              CHARS 3 - 5 = ACCEPTABLE SIGNON ID (3 CHARS)                     
*                                                                               
*                                                                               
         LA    R4,SECRPTBL                                                      
SSTA0180 EQU   *                                                                
         CLI   0(R4),0             END OF TABLE?                                
         BE    SSTA0220            YES - SOURCE NOT VALIDATED                   
         CLC   RSTAFNNE(3),0(R4)   STATION/NEW REP = TABLE ENTRY?               
         BNE   SSTA0200            NO                                           
         CLC   TRGTREP(3),3(R4)    YES - VALID SIGN ON?                         
         BE    SSTA0240            YES - PROCEED                                
SSTA0200 EQU   *                                                                
         LA    R4,LSECRPTB(R4)     NO  - BUMP TO NEXT TABLE ENTRY               
         B     SSTA0180            GO BACK FOR NEXT                             
SECRPTBL EQU   *                                                                
         DC    C'ARPALL'                                                        
LSECRPTB EQU   *-SECRPTBL                                                       
         DC    X'0000'             DELIMITER                                    
*                                                                               
         DS    H'0'                                                             
         DROP  R6                                                               
*                                  NOT SAME REP - REJECT                        
*                                                                               
SSTA0220 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     SSTA0320                                                         
SSTA0240 EQU   *                                                                
         MVI   SRCETYPE,C'O'       SET STATION TYPE TO 'OTHER'                  
         L     R5,AIO                                                           
         USING RSTAREC,R5                                                       
         LA    RF,RSTAELEM                                                      
*                                                                               
         DROP  R5                                                               
*                                                                               
SSTA0260 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    SSTA0300            YES - NO X'05' ELEMENT                       
         CLI   0(RF),5             END OF RECORD?                               
         BNE   SSTA0280            YES - NO X'05' ELEMENT                       
         MVI   SRCETYPE,C'G'       SET STATION TYPE TO 'GRAPH'                  
         CLC   10(2,RF),=X'0406'   GRAPHNET?                                    
         BE    SSTA0300            YES                                          
         MVI   SRCETYPE,C'A'       SET STATION TYPE TO 'ACE'                    
         B     SSTA0300            YES                                          
SSTA0280 EQU   *                                                                
         ZIC   RE,1(RF)            TAKE ELEMENT LENGTH                          
         AR    RF,RE               BUMP TO NEXT ELEMENT                         
         B     SSTA0260            GO BACK FOR NEXT                             
SSTA0300 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
SSTA0320 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
****>>>                                                                         
*                                                                               
*   SETUSED:  SCAN X'9E' POINTERS FOR TARGET STATION/REP, AND                   
*        ESTABLISH TABLE OF CONTRACTS ALREADY TAKEN OVER.  THIS                 
*        WILL SERVE TO PREVENT SAME CONTRACT COMING OVER MORE                   
*        THAN ONCE.                                                             
*                                                                               
*******************************************************************             
*SETUSED NTR1                                                                   
*         LA    R2,CONTABLE         SET A(CONTRACT NUMBER TABLE)                
*         XC    KEY,KEY             CLEAR THE KEY                               
*         MVI   KEY,X'9E'           SET FOR X'9E' PASSIVES                      
*         MVC   KEY+12(2),AGENCY    INSERT TARGET REP CODE                      
*         MVC   KEY+14(5),SRCESTAT  INSERT TAKEOVER STATION                     
*         MVC   KEY+19(2),SRCEREP   INSERT SOURCE REP CODE                      
*         GOTO1 HIGH                ACCESS FIRST KEY                            
*         B     SUSE0040                                                        
*SUSE0020 EQU   *                                                               
*         GOTO1 SEQ                                                             
*SUSE0040 EQU   *                                                               
*         CLI   KEY,X'9E'           SAME KEY?                                   
*         BNE   SUSE0200            NO  - FINISHED                              
*         CLC   KEY+12(2),AGENCY    KEY FOR TARGET AGENCY?                      
*         BNE   SUSE0200            NO  - FINISHED                              
*         CLC   KEY+14(5),SRCESTAT  KEY FOR TARGET STATION?                     
*         BNE   SUSE0200            NO  - FINISHED                              
*         CLC   KEY+19(2),SRCEREP   KEY FOR SOURCE REP?                         
*         BNE   SUSE0200            NO  - FINISHED                              
*                                  YES - TABLE THE CONTRACT NUMBER              
*         MVC   0(4,R2),KEY+23      INSERT KEY INTO TABLE                       
*         LA    R2,4(R2)            BUMP TABLE                                  
*         L     RF,CONCTR           INCREMENT SEARCH COUNTER                    
*         LA    RF,1(RF)                                                        
*         ST    RF,CONCTR                                                       
*         B     SUSE0020            GO BACK FOR NEXT                            
*SUSE0200 EQU   *                                                               
*         LA    R4,CONTABLE         SET A(TABLE OF RECORDS)                     
*         L     R3,CONCTR           SET NUMBER OF RECORDS                       
*         GOTO1 XSORT,DMCB,(0,(R4)),(R3),4,4,0                                  
*         XIT1                                                                  
         EJECT                                                                  
*******************************************************************             
* DISPLAY CONTRACT INFORMATION                                                  
*    1ST PARAMETER HAS A(STARTING FIELD HEADER)                                 
*    AIO HAS ADDRESS OF CONTRACT RECORD                                         
*******************************************************************             
DISCON   NTR1                                                                   
         L     R4,0(R1)            SET A(NEXT SCREEN LINE)                      
*                                                                               
         L     R5,AIO                                                           
         USING RCONREC,R5                                                       
*                                                                               
         MVC   CON#KEY,KEY         SAVE CONTRACT RECORD X'8E' KEY               
*                                                                               
         LA    R2,LISTAR           SET TO DATA FIELD OF LINE                    
         USING DISLIN1,R2                                                       
         LA    R3,LISTAR2          SET TO DATA FIELD OF LINE                    
         USING DISLIN2,R3                                                       
         GOTO1 HEXOUT,DMCB,RCONKCON,DL1CON#,4,=C'TOG'                           
*                                  INSERT CONTRACT NUMBER ON LINE               
         GOTO1 DATCON,DMCB,(3,RCONDATE),(5,DL1FLITE)                            
         MVI   DL1FLITE+8,C'-'                                                  
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(5,DL1FLITE+9)                        
         MVC   DL1OFFC,RCONKOFF    INSERT OFFICE CODE                           
         MVC   DL1CTYPE,RCONTYPE   INSERT CONTRACT TYPE                         
         MVC   DL1SALEP,RCONSAL    INSERT SALESPERSON                           
*                                                                               
         CLC   RCONPRD,SPACES      ANY PRODUCT CODE?                            
         BE    DISC0040            NO  - DISPLAY ENTERED VALUE                  
         GOTO1 =A(PRDNAME),DMCB,RCONKADV,RCONPRD,(RC),RR=Y                      
         MVC   DL1PROD(20),WORK    YES - DISPLAY PRODUCT CODE                   
*                                                                               
*                                                                               
         B     DISC0060                                                         
DISC0040 EQU   *                                                                
         LA    RF,RCONELEM         FIND X'05' ELEMENT                           
DISC0044 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                DAMAGED RECORD:  WARNING!                    
         CLI   0(RF),5             PRODUCT NAME ELEMENT?                        
         BE    DISC0048            YES                                          
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     DISC0044            GO BACK FOR NEXT                             
DISC0048 EQU   *                                                                
         MVC   DL1PROD,2(RF)       YES - DISPLAY PRODUCT CODE                   
DISC0060 EQU   *                                                                
         GOTO1 =A(AGYNAME),DMCB,RCONKAGY,RR=Y                                   
         MVC   DL1AGY(20),WORK                                                  
         GOTO1 ADVNAME,DMCB,RCONKADV                                            
         MVC   DL2ADV(20),WORK                                                  
*                                                                               
*   RESTORE CONTRACT RECORD AS LAST ONE READ                                    
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
*   CYCLE X'03' ELEMENTS, ACCUMULATING ESTIMATED DOLLARS                        
*         X'04' IF ALLHIST, AND RCONBUYR = 'ACC-'                               
*                                                                               
*   TEST BUCKETS                                                                
*        L     RF,TESTCTR                                                       
*        LA    RF,1(RF)                                                         
*        ST    RF,TESTCTR                                                       
*        CLI   TESTCTR+3,2                                                      
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*   TEST BUCKETS END                                                            
*                                                                               
         SR    RE,RE               CLEAR ACCUMULATOR                            
         LA    R1,RCONELEM                                                      
         MVC   DL2ACCT,SPACES      CLEAR FLAG                                   
         MVI   ELTID,3             SET 'ESTIMATE BUCKET'                        
         CLC   =C'ALL',CONREC      ALLHIST REQUEST?                             
         BNE   DISC0080            NO                                           
         CLC   =C'ACC-',RCONBUYR   YES - ACCOUNTING CONTRACT?                   
         BNE   DISC0080            NO                                           
         MVI   ELTID,4             YES - SET 'INVOICE BUCKET'                   
         MVC   DL2ACCT(10),=C'   ACCT $$'                                       
DISC0080 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
         BE    DISC0160            YES                                          
         CLC   0(1,R1),ELTID       BUCKET (X'03'/'04') ELEMENT?                 
         BNE   DISC0100            NO  - SKIP IT                                
         ZICM  RF,6(R1),4          YES - GET AMOUNT FROM ELEMENT                
         AR    RE,RF                                                            
DISC0100 EQU   *                                                                
         ZIC   RF,1(R1)            L(ELEMENT)                                   
         AR    R1,RF               BUMP TO NEXT ELEMENT                         
         B     DISC0080            GO BACK FOR NEXT                             
ELTID    DS    X                   TEST THIS BUCKET                             
         DS    0H                                                               
DISC0160 EQU   *                                                                
         EDIT  (RE),(16,DL2DOLRS),2,COMMAS=YES                                  
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
         MVC   8(74,R4),LISTAR     LOAD 1ST LINE TO SCREEN                      
         LA    R4,CTOAGYH-CTOCONH(R4)                                           
*                                  BUMP TO NEXT LINE                            
         MVC   8(74,R4),LISTAR2    LOAD 2ND LINE TO SCREEN                      
         LA    R4,CTONEWH-CTOAGYH(R4)                                           
*                                  BUMP TO NEXT LINE                            
         MVC   8(5,R4),=C'NEW->'   REINSERT LINE LITERAL                        
         FOUT  (R4)                                                             
         ZIC   RF,0(R4)            BUMP TO NEXT FIELD                           
         AR    R4,RF                                                            
*                                                                               
         L     R2,AEQUNTRY         SET A(EQUIVALENCY TABLE ENTRY)               
         USING EQUITABL,R2                                                      
*                                                                               
         MVC   ETOAGYOF,RCONKAGY   LOAD ORIGINAL AGENCY/OFFICE                  
         MVC   ETOADVRT,RCONKADV   LOAD ORIGINAL ADVERTISER                     
         MVC   ETOSALEP,RCONSAL    LOAD ORIGINAL SALESPERSON                    
         MVC   ETOCNTYP,RCONTYPE   LOAD ORIGINAL CONTRACT TYPE                  
         MVC   ETOCONUM,RCONKCON   LOAD ORIGINAL CONTRACT NUMBER                
         MVC   ETDSKADR,CON#KEY+28 LOAD ORIGINAL CONTRACT DISK ADDR             
         OC    ETFLAG(1),CNFORVER  SET CONFIRM-OR-VERSION FLAG                  
*                                                                               
         LA    RF,RCONELEM         SET A(1ST ELEMENT)                           
DISC0180 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    DISC0220            YES - FINISHED                               
         CLI   0(RF),X'18'         DEVELOPMENTAL ELEMENT?                       
         BE    DISC0200            YES                                          
         ZIC   RE,1(RF)                                                         
         AR    RF,RE               BUMP TO NEXT ELEMENT                         
         B     DISC0180            GO BACK FOR NEXT                             
DISC0200 EQU   *                                                                
         MVC   ETODVSAL,2(RF)      LOAD ORIGINAL DEV SALESPERSON                
         MVC   ETODVTYP,5(RF)      LOAD ORIGINAL DEV CON TYPE                   
DISC0220 EQU   *                                                                
*                                                                               
         MVC   EQIVKEY,KEY         SAVE KEY OF CONTRACT FOR RESTART             
*                                                                               
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED,            
*                                     TO ACCESS EQUIVALENCY RECORDS             
         MVC   SAVEAIO,AIO         SAVE ORIGINAL AIO                            
         MVC   AIO,AIO2                                                         
         L     R3,AIO2             USE SECOND IO AREA FOR TKO                   
         USING TKO,R3                                                           
*                                                                               
*                                  SET UP AGENCY/OFF FIELD                      
         XC    8(7,R4),8(R4)       CLEAR THE OUTPUT FIELD                       
         NI    4(R4),X'FF'-X'20'   TURN OFF PREVALID BIT                        
*                                                                               
         XC    KEY,KEY             CLEAR IO KEY                                 
         MVI   KEY,X'1F'           SET KEY ID                                   
         MVC   KEY+16(2),TWAAGY    SET REP ID INTO KEY                          
         MVC   KEY+18(2),SRCEREP   SET SOURCE REP ID INTO KEY                   
         MVI   KEY+20,1            SET AGENCY/OFFICE REC TYPE                   
         MVC   KEY+21(6),RCONKAGY                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     RECORD FOUND?                                
         BNE   DISC0260            NO  - LEAVE SCREEN FIELD BLANK               
         GOTO1 GETREC,DMCB,RTKOREC YES - RETRIEVE RECORD                        
         MVC   ETNAGYOF,RTKOEQIV   INSERT EQUIV CODE IN TABLE                   
         OI    ETCDEFLG,AGONFILE   TURN ON 'CODE ON FILE'                       
         MVC   8(4,R4),RTKOEQIV    INSERT EQUIV AGENCY                          
         CLC   RTKOEQIV+4(2),SPACES                                             
*                                  ANY OFFICE WITH THIS AGENCY?                 
         BE    DISC0240            NO                                           
         MVI   12(R4),C'-'         YES - INSERT SEPARATOR                       
         MVC   13(2,R4),RTKOEQIV+4                                              
*                                  INSERT AGENCY OFFICE                         
DISC0240 EQU   *                                                                
         OI    4(R4),X'20'         SET PREVIOUSLY VALID ON FIELD                
DISC0260 EQU   *                                                                
         FOUT  (R4)                SET FIELD TO TRANSMIT                        
         ZIC   RE,0(R4)            BUMP TO NEXT SCREEN FIELD                    
         AR    R4,RE                                                            
*                                  SET UP ADVERTISER FIELD                      
         XC    8(4,R4),8(R4)       CLEAR THE OUTPUT FIELD                       
         NI    4(R4),X'FF'-X'20'   TURN OFF PREVALID BIT                        
*                                                                               
         XC    KEY,KEY             CLEAR IO KEY                                 
         MVI   KEY,X'1F'           SET KEY ID                                   
         MVC   KEY+16(2),TWAAGY    SET REP ID INTO KEY                          
         MVC   KEY+18(2),SRCEREP   SET SOURCE REP ID INTO KEY                   
         MVI   KEY+20,2            SET ADVERTISER REC TYPE                      
         MVC   KEY+21(4),RCONKADV                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     RECORD FOUND?                                
         BNE   DISC0300            NO  - LEAVE SCREEN FIELD BLANK               
         GOTO1 GETREC,DMCB,RTKOREC YES - RETRIEVE RECORD                        
         MVC   ETNADVRT,RTKOEQIV   INSERT EQUIV CODE IN TABLE                   
         OI    ETCDEFLG,ADONFILE   TURN ON 'CODE ON FILE'                       
         MVC   8(4,R4),RTKOEQIV    INSERT EQUIV ADVERTISER                      
         OI    4(R4),X'20'         SET PREVIOUSLY VALID ON FIELD                
DISC0300 EQU   *                                                                
         FOUT  (R4)                TRANSMIT THE FIELD                           
         ZIC   RE,0(R4)            BUMP TO NEXT SCREEN FIELD                    
         AR    R4,RE                                                            
*                                                                               
         XC    8(3,R4),8(R4)       CLEAR THE OUTPUT FIELD                       
         NI    4(R4),X'FF'-X'20'   TURN OFF PREVALID BIT                        
*                                                                               
         XC    KEY,KEY             CLEAR IO KEY                                 
         MVI   KEY,X'1F'           SET KEY ID                                   
         MVC   KEY+16(2),TWAAGY    SET REP ID INTO KEY                          
         MVC   KEY+18(2),SRCEREP   SET SOURCE REP ID INTO KEY                   
         MVI   KEY+20,3            SET SALESPERSON REC TYPE                     
         MVC   KEY+21(3),RCONSAL                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     RECORD FOUND?                                
         BNE   DISC0320            NO  - LEAVE SCREEN FIELD BLANK               
         GOTO1 GETREC,DMCB,RTKOREC YES - RETRIEVE RECORD                        
         MVC   ETNSALEP,RTKOEQIV   INSERT EQUIV CODE IN TABLE                   
         MVC   ETNSPOFF,RTKOEQIV+3 INSERT S/P OFFICE IN TABLE                   
         MVC   ETNSPTEM,RTKOSPTM   INSERT S/P TEAM IN TABLE                     
         OI    ETCDEFLG,SPONFILE   TURN ON 'CODE ON FILE'                       
         MVC   8(3,R4),RTKOEQIV    INSERT EQUIV SALESPERSON                     
***>>>   OI    4(R4),X'20'         SET PREVIOUSLY VALID ON FIELD                
DISC0320 EQU   *                                                                
         FOUT  (R4)                TRANSMIT THE FIELD                           
         ZIC   RE,0(R4)            BUMP TO NEXT SCREEN FIELD                    
         AR    R4,RE                                                            
*                                                                               
         XC    8(1,R4),8(R4)       CLEAR THE OUTPUT FIELD                       
         NI    4(R4),X'FF'-X'20'   TURN OFF PREVALID BIT                        
*                                                                               
         XC    KEY,KEY             CLEAR IO KEY                                 
         MVI   KEY,X'1F'           SET KEY ID                                   
         MVC   KEY+16(2),TWAAGY    SET REP ID INTO KEY                          
         MVC   KEY+18(2),SRCEREP   SET SOURCE REP ID INTO KEY                   
         MVI   KEY+20,4            SET CONTRACT TYPE REC TYPE                   
         MVC   KEY+21(1),RCONTYPE                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     RECORD FOUND?                                
         BNE   DISC0340            NO  - LEAVE SCREEN FIELD BLANK               
         GOTO1 GETREC,DMCB,RTKOREC YES - RETRIEVE RECORD                        
         MVC   ETNCNTYP,RTKOEQIV   INSERT EQUIV CODE IN TABLE                   
         OI    ETCDEFLG,CTONFILE   TURN ON 'CODE ON FILE'                       
         MVC   8(1,R4),RTKOEQIV    INSERT EQUIV CONTRACT TYPE                   
         OI    4(R4),X'20'         SET PREVIOUSLY VALID ON FIELD                
         FOUT  (R4)                                                             
DISC0340 EQU   *                                                                
*                                                                               
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
         GOTO1 =A(CHKCOMBO),DMCB,(RC),(R2),(R5),RR=Y                            
*                                  CHECK IF PARTICIPATING IN COMBO              
*        WILL RETURN SET TO SOURCE FILE                                         
*                                                                               
         MVC   KEY,EQIVKEY         REACCESS LAST CONTRACT                       
         GOTO1 HIGH                                                             
         LA    R2,LEQUITBL(R2)     BUMP TO NEXT EQUIV ENTRY                     
         ST    R2,AEQUNTRY         SAVE A(NEXT SLOT)                            
         MVC   AIO,SAVEAIO         RESET ORIGINAL IO AREA                       
         XIT1                                                                   
*                                                                               
         DROP  R2,R3,R5                                                         
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*******************************************************************             
* GET ADVERTISER EXPANDED NAME                                                  
* P1 HAS ADV CODE                                                               
* WORK WILL CONTAIN EXPANDED NAME                                               
*******************************************************************             
ADVNAME  NTR1                                                                   
         MVC   SEQKEY,KEY                                                       
         XC    WORK,WORK                                                        
         L     R1,0(R1)                                                         
         MVC   WORK(L'RADVKADV),0(R1)                                           
                                                                                
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RADVKEY,R6                                                       
         MVI   RADVKTYP,X'08'                                                   
         MVC   RADVKADV,0(R1)                                                   
         MVC   RADVKREP,SRCEREP                                                 
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RADVKEY),KEYSAVE                                           
         BNE   ADVNAMX                                                          
                                                                                
         MVC   SAVEAIO,AIO         SAVE A(IO AREA)                              
         MVC   AIO,AIO2            RESET A(IO AREA) TO IOAREA2                  
         L     R6,AIO2                                                          
         USING RADVREC,R6                                                       
                                                                                
         GOTO1 GETREC,DMCB,RADVREC                                              
*                                                                               
         MVC   AIO,SAVEAIO         RESET ORIGINAL IO AREA                       
         MVC   WORK(L'RADVNAME),RADVNAME                                        
                                                                                
         DROP  R6                                                               
                                                                                
ADVNAMX  DS    0H                                                               
         MVC   KEY,SEQKEY          RE-ESTABLISH SEQ                             
         GOTO1 HIGH                                                             
         B     EXIT                                                             
         EJECT                                                                  
*******************************************************************             
* ERROR MESSAGES                                                                
*******************************************************************             
MISSFLD  MVC   RERROR,=AL2(MISSING)                                             
         B     ERREND                                                           
                                                                                
INVLFLD  MVC   RERROR,=AL2(INVALID)                                             
         B     ERREND                                                           
                                                                                
SLOCKOUT MVC   RERROR,=AL2(55)                                                  
         B     ERREND                                                           
                                                                                
INVLPER  MVC   RERROR,=AL2(441)    MMMYY(-MMMYY)                                
         B     ERREND                                                           
                                                                                
INVLDAT  MVC   RERROR,=AL2(INVDAT)                                              
         B     ERREND                                                           
                                                                                
INVLDAT2 MVC   RERROR,=AL2(706)                                                 
         B     ERREND                                                           
                                                                                
SRCCLOSD MVC   RERROR,=AL2(658)                                                 
         B     ERREND                                                           
                                                                                
NEXTLIST MVC   RERROR,=AL2(15)     PRESS ENTER FOR NEXT                         
         B     INFEND                                                           
                                                                                
NEXTUPDT MVC   RERROR,=AL2(167)    UPDATED: PRESS ENTER FOR NEXT                
*                                                                               
*   TEST DUMP                                                                   
***>>>   MVC   DIE(2),=X'0000'     FORCE DUMP                                   
*   TEST DUMP END                                                               
*                                                                               
         B     INFEND                                                           
MAXSERCH MVC   RERROR,=AL2(171)    MAX IO:  ENTER TO CONTINUE                   
         B     INFEND                                                           
MAXDATA  MVC   RERROR,=AL2(172)    MAX IO:  DATE FOUND                          
         B     INFEND                                                           
                                                                                
SRCENOGD MVC   RERROR,=AL2(709)    SOURCE REP NOT FOUND ON FILE                 
         B     ERREND                                                           
STABARRD EQU   *                                                                
         CLC   =C'JNDT',DUB        JOIN DATE NOT A MONDAY ERROR?                
         BE    STAD0020                                                         
         CLC   =C'HSDT',DUB        HISTORY TAKEOVER DATE ABSENT?                
         BE    STAD0040                                                         
         MVC   RERROR,=AL2(714)    TARGET STA NOT CLEARED FOR XFER              
         B     ERREND                                                           
STAD0020 EQU   *                                                                
         MVC   RERROR,=AL2(730)    TARGET STA JOIN DATE NOT MONDAY              
         B     ERREND                                                           
STAD0040 EQU   *                                                                
         MVC   RERROR,=AL2(848)    HISTORY T/O DATE MISSING                     
         B     ERREND                                                           
STABARRS EQU   *                                                                
         CLC   =C'HSDT',DUB        HISTORY TAKEOVER DATE ABSENT?                
         BE    STAB0020                                                         
         CLC   =C'INVT',DUB        INVOICE TAKEOVER FLAGS NOT SAME?             
         BE    STAB0040                                                         
         MVC   RERROR,=AL2(723)    SOURCE STA NOT CLEARED FOR XFER              
         B     ERREND                                                           
STAB0020 EQU   *                                                                
         MVC   RERROR,=AL2(849)    TARGET STA HIST T/O DATE UNSET               
         B     ERREND                                                           
STAB0040 EQU   *                                                                
         MVC   RERROR,=AL2(853)    OLD/NEW REP INVOICE FLAGS NOT SAME           
         B     ERREND                                                           
OLDBARRS MVC   RERROR,=AL2(823)    SOURCE REP BARS TAKEOVER                     
         B     ERREND                                                           
AGENCYNF MVC   RERROR,=AL2(633)    NO MATCHES TO FILTERS SUBMITTED              
         B     ERREND                                                           
INVSALPR EQU   *                                                                
         OC    DUB,DUB             OFF/TEAM ERROR MESSAGE?                      
         BZ    INVS0020            NO                                           
         MVC   RERROR,DUB+2        TEAM NOT ALLOWED TO SELL STATION             
         B     ERREND                                                           
INVS0020 EQU   *                                                                
         MVC   RERROR,=AL2(716)    SALESPERSON CODE NOT ON FILE                 
         B     ERREND                                                           
INVSPLEV EQU   *                                                                
         MVC   RERROR,=AL2(843)    SALESPERSON ERROR: LEAVE DATE                
         B     ERREND                                                           
INVTYPE  EQU   *                                                                
         MVC   RERROR,=AL2(726)    CONTRACT TYPE NOT ON FILE                    
         B     ERREND                                                           
INVAGY   MVC   RERROR,=AL2(718)    AGENCY CODE NOT ON FILE                      
         B     ERREND                                                           
                                                                                
INVOFF   MVC   RERROR,=AL2(782)    OFFICE CODE NOT ON FILE                      
         B     ERREND                                                           
                                                                                
ENDLIST  MVC   RERROR,=AL2(16)     END OF LIST                                  
         B     INFEND                                                           
                                                                                
ERREND   DS    0H                                                               
         MVI   RMSGTYPE,C'E'                                                    
         GOTO1 MYERROR             DO A GETTXT CALL                             
                                                                                
INFEND   DS    0H                                                               
         LA    R2,CTOSTAH          PUT CURSOR HERE                              
         MVI   RMSGTYPE,C'I'                                                    
         GOTO1 MYERROR             DO A GETTXT CALL                             
*                                                                               
*                                                                               
         DS    50F                 PUSH GETEL INTO 2ND BASE REGISTER            
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*******************************************************************             
* LOCAL STORAGE AREA                                                            
*******************************************************************             
RELO     DS    A                                                                
SAVEAIO  DS    A                                                                
SAVEAIO2 DS    A                                                                
VSWITCH  DS    F                                                                
VSYSFAC  DC    V(SYSFAC)                                                        
VREPFACS DS    V                                                                
VBINSRCH DS    F                                                                
AEQUITBL DS    A                   A(EQUIVALENT TABLE)                          
AEQUNTRY DS    A                   A(EQUIVALENT TABLE ENTRY IN USE)             
ACMBOTBL DS    A                   A(COMBO PARTICIPANT TABLE)                   
ACMBNTRY DS    A                   A(COMBO PARTIC TABLE ENTRY IN USE)           
ANEWCMBO DS    A                   A(NEW PARTIC 17 ELT)                         
CMBODISP DS    F                   COMBO TABLE DISPLACEMENT                     
ACONTROL DS    A                   A(CONTROL INFORMATION)                       
AMISFLGS DS    A                   A(MISCELLANEOUS FLAGS)                       
ACONTRCT DS    A                   A(CONTRACT RECORD)                           
ABUCKBLK DS    A                   A(BUCKET STORAGE AREA)                       
CONCTR   DS    F                                                                
TESTCTR  DS    F                                                                
TRGTREP  DS    CL3                 TARGET REP ID                                
NEXTSCRN DS    C                   Y=USER PRESSED ENTER/DISP NEXT SCRN          
UPDTDONE DS    CL1                                                              
SEQKEY   DS    CL(L'KEY)                                                        
ADVCODE  DS    CL(L'RCONKADV)                                                   
SAVCON#S DS    CL8                 SAVE AREA FOR OLD/NE CON #S                  
SAVGRPS  DS    CL4                 SAVE TARGET/SOURCE GROUPS                    
SAVSTYPS DS    CL2                 SAVE TARGET/SOURCE STATION TYPES             
CONTSTAT DS    X                                                                
BUCKFLGS DS    X                   BUCKETING FLAGS                              
CTLCOMBO DS    C                   N  =  NOT COMBO                              
*                                  Y  =  COMBO RECOGNIZED                       
*                                  OTHER  =  COUNT (NOT ZERO RELATIVE)          
CLEARFLG DS    C                                                                
CONSAVED DS    C                   Y  =  CONTRACT SAVED - NEEDS TO BE           
*                                        WRITTEN AFTER BUYS DONE                
*                                  N  =  CONTRACT NOT SAVED - OUTPUT            
*                                        AS FIRST RECORD                        
*                                                                               
COPYMGHD DS    C                   Y = COPY MAKEGOOD HEADER                     
*                                  N = DON'T COPY MAKEGOOD HEADER               
*                                  L = COPY FOR THE LAST TIME AND EXIT          
MGHEADCD DS    CL2                 MG HEADER GROUP CODE                         
MGSAVEKY DS    CL27                LAST MG KEY FOR SEQ                          
*                                                                               
CNFORVER DS    CL1                 0  =  CONFIRM                                
*                                  1  =  VERSION                                
TODAY    DS    CL3                 DATE OF RUN                                  
NEWCOMBO DS    CL38                NEW COMBO BUILD AREA:                        
*                                  FOUR STATIONS MAX                            
WORK2    DS    CL80                                                             
IOAREA   DS    CL128                                                            
CTYPTABL DS    CL64                CONTRACT TYPE TABLES                         
         DS    0F                                                               
WORK3    DS    CL80                                                             
LISTAR2  DS    CL80                                                             
         EJECT                                                                  
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMACD          (HEADER FILTER SCREEN OVERLAY)               
       ++INCLUDE RESFMWTWA                                                      
       ++INCLUDE RESFMWORKD                                                     
CONTRACT DSECT                                                                  
       ++INCLUDE REGENCON                                                       
ADV      DSECT                                                                  
       ++INCLUDE REGENADV                                                       
AGY      DSECT                                                                  
       ++INCLUDE REGENAGY                                                       
AGY2     DSECT                                                                  
       ++INCLUDE REGENAGY2                                                      
PRD      DSECT                                                                  
       ++INCLUDE REGENPRD                                                       
SALESMAN DSECT                                                                  
       ++INCLUDE REGENSAL                                                       
TKO      DSECT                                                                  
       ++INCLUDE REGENTKO                                                       
STATION  DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE REGENBUY                                                       
MKGRECD  DSECT                                                                  
       ++INCLUDE REGENMKG                                                       
CFCRECD  DSECT                                                                  
       ++INCLUDE REGENCFC                                                       
COVRECD  DSECT                                                                  
       ++INCLUDE REGENCOV                                                       
CONTYP   DSECT                                                                  
       ++INCLUDE REGENCTY                                                       
REPRECD  DSECT                                                                  
       ++INCLUDE REGENREPA                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE SFMISFLG                                                       
       ++INCLUDE RECNTPROF                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE REGLCON                                                        
*                                                                               
*                                                                               
*                                                                               
*                                                                               
EQUITABL DSECT                                                                  
ETFLAG   DS    CL1                 FLAG BYTE FOR CONTRACT                       
*                                  X'80'  =  'SELECTED'                         
*                                  X'40'  =  ALREADY PROCESSED                  
*                                  X'20'  =  OUTPUT-SCANNED FOR CODES           
*                                  X'10'  =  PAPERWORK GENERATED                
*                                  X'01'  =  CONFIRMED ORDER                    
ETCDEFLG DS    CL4                 FLAG BYTES FOR CODES                         
*                                                                               
*        BYTE 1 VALUES:                                                         
*                                  X'80'  =  AGENCY CODE ON FILE                
AGONFILE EQU   X'80'                                                            
*                                  X'40'  =  ADD CODE RECORD                    
ADDAGY   EQU   X'40'                                                            
*                                  X'20'  =  ADVERT CODE ON FILE                
ADONFILE EQU   X'20'                                                            
*                                  X'10'  =  ADD CODE RECORD                    
ADDADV   EQU   X'10'                                                            
*                                  X'08'  =  S/P    CODE ON FILE                
SPONFILE EQU   X'08'                                                            
*                                  X'04'  =  ADD CODE RECORD                    
ADDSP    EQU   X'04'                                                            
*                                  X'02'  =  CONTYP CODE ON FILE                
CTONFILE EQU   X'02'                                                            
*                                  X'01'  =  ADD CODE RECORD                    
ADDCT    EQU   X'01'                                                            
*                                                                               
*        BYTE 2:  COMBO CONTRACTS IN SET (EXCLUDING BASE ORDER)                 
*                                                                               
ETOAGYOF DS    CL6                 ORIG AGY/OFF CODES                           
ETNAGYOF DS    CL6                 NEW  AGY/OFF CODES                           
ETOADVRT DS    CL4                 ORIG ADVERT  CODE                            
ETNADVRT DS    CL4                 NEW  ADVERT  CODE                            
ETOSALEP DS    CL3                 ORIG S/P     CODE                            
ETNSALEP DS    CL3                 NEW  S/P     CODE                            
ETOCNTYP DS    CL1                 ORIG CONTYPE CODE                            
ETNCNTYP DS    CL1                 NEW  CONTYPE CODE                            
ETODVTYP DS    CL2                 ORIG DEVTYPE CODE                            
ETNDVTYP DS    CL2                 NEW  DEVTYPE CODE                            
ETODVSAL DS    CL3                 ORIG DEV S/P CODE                            
ETNDVSAL DS    CL3                 NEW  DEV S/P CODE                            
ETOCONUM DS    CL4                 ORIG CONTRACT NUM                            
ETNCONUM DS    CL4                 NEW  CONTRACT NUM                            
ETNSPOFF DS    CL2                 NEW SALESPERSON OFFICE                       
ETNSPTEM DS    CL2                 NEW SALESPERSON TEAM                         
ETDSKADR DS    CL4                 ORIG CONTRACT DISK ADDRESS                   
LEQUITBL EQU   *-ETFLAG            ENTRY LENGTH                                 
*                                                                               
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE CTGENSTAD                                                      
         EJECT                                                                  
       ++INCLUDE REGENDAR                                                       
         EJECT                                                                  
*                                                                               
*                                                                               
DISLIN1  DSECT                                                                  
DL1CON#  DS    CL8                                                              
         DS    CL2                                                              
DL1AGY   DS    CL20                                                             
         DS    CL1                                                              
DL1PROD  DS    CL20                                                             
         DS    CL1                                                              
DL1FLITE DS    CL20                                                             
*                                                                               
DISLIN2  DSECT                                                                  
         DS    CL10                                                             
DL2ADV   DS    CL20                                                             
         DS    CL1                                                              
DL1SALEP DS    CL3                                                              
         DS    CL2                                                              
DL1CTYPE DS    CL1                                                              
         DS    CL2                                                              
DL1OFFC  DS    CL2                                                              
DL2ACCT  DS    CL11                                                             
DL2DOLRS DS    CL12                                                             
*                                                                               
       ++INCLUDE FLDIND                                                         
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
SAVEKEY  DS    CL(L'KEY)           NEED FOR PAGING                              
CON#KEY  DS    CL(L'KEY)           DURING SCREEN HANDLING                       
FIRSTKEY DS    CL(L'KEY)           FOR WHEN WE LOOP AROUND AGAIN                
HIGHKEY  DS    CL(L'KEY)           FOR WHEN WE LOOP AROUND AGAIN                
SCRNKEY  DS    CL(L'KEY)           FIRST KEY ON SCREEN                          
EQIVKEY  DS    CL(L'KEY)           EQUIV KEY RESTART                            
CHECKKEY DS    CL(L'KEY)           EXTRA KEY FOR AGENCY OFFICE CHECK            
KEYA2SAV DS    CL(L'KEY)           EXTRA KEY FOR A202 LOCKOUT  CHECK            
SETKEYA2 DS    CL(L'KEY)           EXTRA KEY FOR A202 LOCKOUT  CHECK            
MAXIOKEY DS    CL(L'KEY)           EXTRA KEY FOR MAX IO SITUATIONS              
MAXIOFLG DS    CL1                                                              
LASTSCRN DS    C                                                                
TAKEALL  DS    CL1                 Y  =  S+ FOUND: TAKE ALL                     
CTKEYFLG DS    CL1                 Y  =  CONTYPE FILTER DONE                    
*                                                                               
CTRLAREA DS    0F                  CONTROL AREA TO SAVE                         
EFFDATE  DS    CL6                 START DATE IN EBCDIC                         
ENDDT    DS    CL6                 END DATE IN EBCDIC                           
EFDTCOMP DS    CL2                 COMPRESSED EFFECTIVE DATE                    
EFDTBIN  DS    XL3                 BINARY     EFFECTIVE DATE                    
EFDTBSUN DS    XL3                 BINARY SUNDAY (EFF DATE -1)                  
BENDDT   DS    XL3                 END DATE OF END B'CAST MONTH                 
HISTDATE DS    XL2                 HIST T/O DATE - COMPRESSED                   
HISTDATB DS    XL3                 HIST T/O DATE - BINARY                       
EARLYMOS DS    CL2                 START MONTH OF SERVICE CUTOFF DATE           
EACTDATE DS    CL2                 EARLY ACTIVITY DATE: COMPRESSED              
LACTDATE DS    CL2                 LATE  ACTIVITY DATE: COMPRESSED              
ERLYLATE DS    CL1                                                              
*                                  X'02' = CUT AT EARLY END                     
CUTEARLY EQU   02                                                               
*                                  X'01' = CUT AT LATE END                      
CUTLATE  EQU   01                                                               
*                                  X'00' = NO CUT TO ORDER                      
*                                                                               
MONDATE  DS    XL2                 COMPRESSED 'THIS WEEK' MONDAY                
SAVSTDAT DS    XL3                 TARGET STATION START DATE                    
OCONDATE DS    CL6                                                              
SRCECON  DS    CL4                 SOURCE CONTRACT NUMBER                       
TRGTCON  DS    CL4                 TARGET CONTRACT NUMBER                       
TARGREP  DS    CL2                 TARGET REP                                   
*                                                                               
*   SRCEGRP/TRGTGRP AND SRCETYPE/TRGTTYPE ARE SAVED AND RESTORED                
*        AS SINGLE FIELDS.  DO NOT INSERT OTHER FIELDS BETWEEN                  
*        THE PAIRS OF FIELDS                                                    
*                                                                               
SRCEGRP  DS    CL2                 SOURCE GROUP                                 
TRGTGRP  DS    CL2                 TARGET GROUP                                 
SRCETYPE DS    CL1                 SOURCE STATION TYPE:                         
*                                  O=OTHER                                      
*                                  G=GRAPHNET                                   
*                                  A=ACE                                        
TRGTTYPE DS    CL1                 TARGET STATION TYPE:                         
*                                  O=OTHER                                      
*                                  G=GRAPHNET                                   
*                                  A=ACE                                        
SRCEREP  DS    CL2                 SOURCE REP                                   
TEMPTGRP DS    CL2                 TEMPORARY GROUP STORAGE: NEW                 
TEMPSGRP DS    CL2                 TEMPORARY GROUP STORAGE: OLD                 
TEMPTTYP DS    CL1                 TEMPORARY STATION TYPE STORAGE: NEW          
TEMPSTYP DS    CL1                 TEMPORARY STATION TYPE STORAGE: OLD          
*                                                                               
SRCESTAT DS    CL5                 SOURCE STATION                               
SRCESIGN DS    CL5                 SOURCE SIGNON                                
SRCENAME DS    CL20                SHORT NAME                                   
SRCEUTL  DS    CL1                 SOURCE UTL NUMBR                             
ORIGUTL  DS    CL1                 ORIGINAL UTL NUMBER                          
SAVESALP DS    CL3                 DEFAULT SALESPERSON CODE                     
SAVESPOF DS    CL2                 DEFAULT SALESPERSON OFFICE                   
SAVESPTM DS    CL2                 DEFAULT SALESPERSON TEAM                     
SAVECTYP DS    CL1                 DEFAULT CONTRACT TYPE                        
SAVELUID DS    CL8                 LUID MAKING THE REQUEST                      
SAVEAGY  DS    CL6                 SAVE AREA FOR AGENCY FILTER                  
OFFTEAMS DS    CL80                OFFICE TEAMS ALLOWED FOR STATION             
*                                  POS 1 - 2  =  OFFICE                         
*                                  POS 3 - 4  =  TEAM ALLOWED                   
COVSHT#  DS    CL4                 COVERSHEET CONTRACT NUMBER                   
COVORG#  DS    CL4                 ORIGINAL COVERSHEET CON#                     
LCTLAREA EQU   *-CTRLAREA          LENGTH OF CONTROL AREA                       
*                                                                               
ALTP     DS    CL132               ALTERNATE PRINT AREA                         
GLOBSV   DS    CL24                GLOBBER SAVE AREA                            
*                                                                               
NEWADDR  DS    F                   NEW CONTRACT DISK ADDRESS                    
SAVAGASS DS    F                   SAVE AREA: AGENCY ASSIGNMENT                 
SAVEQUIV DS    F                   SAVE AREA: EQUIV TBL IN PROCESS              
SVTOTSPT DS    F                   TOTAL SPOTS WORK AREA                        
SVTOTWKS DS    F                                                                
CONCOUNT DS    F                   CONTRACT COUNTER                             
IOCOUNT  DS    F                   I/O COUNTER                                  
MAXIOCTR DS    F                   I/O COUNTER                                  
DARAGYS  DS    CL20                AGENCY ASSIGNMENT SAVE AREA                  
DARAGYCD DS    CL4                 AGENCY CODE FROM AGENCY RECORD               
DARKTYP  DS    XL1                                                              
CONCTFLG DS    XL1                                                              
EFFELTS  DS    CL1                                                              
MGBITFLG DS    CL1                 X'80' = X'56' ELT DELETED FROM REC           
*                                  X'40' = X'05' ELT DELETED FROM REC           
*                                  X'20' = X'07' ELT DELETED FROM REC           
*                                                                               
BARFLAG  DS    CL1                 X'80' - STATION BARRED BY OLD REP            
         EJECT                                                                  
*   UPDTCON:  MODIFY CONTRACT RECORD ON TARGET SIDE.                            
*                                                                               
T8182F   CSECT                                                                  
*******************************************************************             
* GET AGENCY EXPANDED NAME                                                      
* P1 HAS AGY CODE AND OFFICE                                                    
* WORK WILL CONTAIN EXPANDED NAME                                               
*******************************************************************             
AGYNAME  NTR1  BASE=*,LABEL=*                                                   
         MVC   SEQKEY,KEY                                                       
         XC    WORK,WORK                                                        
         L     R1,0(R1)                                                         
         MVC   WORK(L'RAGYKAGY+L'RAGYKAOF),0(R1)                                
                                                                                
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RAGYKEY,R6                                                       
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKAGY(L'RAGYKAGY+L'RAGYKAOF),0(R1)                            
         MVC   RAGYKREP,SRCEREP                                                 
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RAGYKEY),KEYSAVE                                           
         BNE   AGYNAMX                                                          
*                                                                               
         MVC   SAVEAIO,AIO         SAVE A(IO AREA)                              
         MVC   AIO,AIO2            RESET A(IO AREA) TO IOAREA2                  
         L     R6,AIO2                                                          
         USING RAGYREC,R6                                                       
                                                                                
         GOTO1 GETREC,DMCB,RAGYREC                                              
*                                                                               
         MVC   AIO,SAVEAIO         RESET ORIGINAL IO AREA                       
*                                                                               
         MVC   WORK(L'RAGYNAM1),RAGYNAM1                                        
                                                                                
         LA    RF,IO                                                            
         ST    RF,AIO                                                           
         DROP  R6                                                               
                                                                                
AGYNAMX  DS    0H                                                               
         MVC   KEY,SEQKEY          RE-ESTABLISH SEQ                             
         GOTO1 HIGH                                                             
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*******************************************************************             
* GET PRODUCT EXPANDED NAME                                                     
* P1 HAS ADV CODE                                                               
* P2 HAS PRD CODE                                                               
* WORK WILL CONTAIN EXPANDED NAME                                               
*******************************************************************             
PRDNAME  NMOD1 0,*PRDN*                                                         
         L     RC,8(R1)            RESET A(WORKSPACE)                           
         MVC   SEQKEY,KEY                                                       
         XC    WORK,WORK                                                        
         L     RF,0(R1)                                                         
         MVC   ADVCODE,0(RF)                                                    
         L     RF,4(R1)                                                         
         MVC   WORK(L'RCONPRD),0(RF)                                            
                                                                                
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RPRDKEY,R6                                                       
         MVI   RPRDKTYP,X'09'                                                   
         MVC   RPRDKADV,ADVCODE                                                 
         MVC   RPRDKPRD,WORK                                                    
         MVC   RPRDKREP,SRCEREP                                                 
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RPRDKEY),KEYSAVE                                           
         BNE   PRDNAMX                                                          
                                                                                
         MVC   SAVEAIO,AIO         SAVE A(IO AREA)                              
         MVC   AIO,AIO2            RESET A(IO AREA) TO IOAREA2                  
         L     R6,AIO2                                                          
         USING RPRDREC,R6                                                       
*                                                                               
         GOTO1 GETREC,DMCB,RPRDREC                                              
*                                                                               
         MVC   AIO,SAVEAIO         RESET ORIGINAL IO AREA                       
*                                                                               
         MVC   WORK(L'RPRDNAME),RPRDNAME                                        
                                                                                
         DROP  R6                                                               
                                                                                
PRDNAMX  DS    0H                                                               
         MVC   KEY,SEQKEY          RE-ESTABLISH SEQ                             
         GOTO1 HIGH                                                             
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   CHEKTSTA: ACCESS THE REP STATION RECORD TO DETERMINE IF STATION             
*        IS CLEARED FOR TAKEOVER PROCESSING.  THIS CODE CHECKS THE              
*        SIGNON REP'S STATION RECORD, WHERE                                     
*              1.  THE STATION MUST EXIST                                       
*              2.  JOIN DATE MUST BE EQUAL OR BEFORE THE EFFECTIVE              
*                  DATE (NO LONGER APPLICABLE)                                  
*                                                                               
CHEKTSTA NMOD1 0,*CTST*                                                         
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         L     RE,AMISFLGS         SET A(TWA WORK AREA)                         
         USING MISFLGS,RE                                                       
         MVI   ALTCAL,C'N'         SET 'ALTERNATE CALENDAR' OFF FOR             
*                                     PRIMARY STATION                           
*                                                                               
         DROP  RE                                                               
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,2               INSERT RECORD TYPE                           
         MVC   KEY+20(2),TWAAGY    INSERT REP CODE                              
         MVC   KEY+22(5),SRCESTAT  INSERT STATION CALL LETTERS                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     RECORD FOUND?                                
         BNE   TSTA0140            NO  - EXIT WITH ERROR                        
         GOTO1 GETREC              RETRIEVE STATION RECORD                      
         L     R5,AIO                                                           
         USING RSTAREC,R5                                                       
         MVC   TRGTGRP,RSTAGRUP    SAVE TARGET STATION GROUP/SUBGRP             
         TM    RSTAFLGS,X'80'      TARGET OKAYS INVOICE BUCKET TAKE?            
         BNO   TSTA0010            NO                                           
         L     RF,AMISFLGS         MISCELLANEOUS FLAGS STORAGE AREA             
*                                                                               
         USING MISFLGS,RF                                                       
*                                                                               
         MVI   TINVTAK,C'Y'        SET TARGET INVOICE FLAG ON                   
*                                                                               
         DROP  RF                                                               
TSTA0010 EQU   *                                                                
         CLC   =C'ALL',CONREC      ALLHIST REQUEST?                             
         BNE   TSTA0018            NO                                           
         GOTO1 DATCON,DMCB,(5,WORK),(3,WORK)                                    
***      ZIC   RF,WORK             ADD 20 TO PRESENT YEAR                       
***      A     RF,=F'20'                                                        
***      STC   RF,WORK             PUT YEAR BACK                                
*                                                                               
*   LATEST COMPRESSED YEAR CURRENTLY PERMISSIBLE ON SYSTEM IS 2027.             
*        FORCE THIS DATE.  IF/WHEN THE DATES ARE ADJUSTED, THE                  
*        ADDITION OF 20 MAY BE REINSTATED, AND THE FORCED YEAR                  
*        REMOVED.                                                               
*                                                                               
         MVI   WORK,X'7F'          SET DATE TO 2027                             
*                                                                               
         MVC   SAVSTDAT,WORK                                                    
         GOTO1 DATCON,DMCB,(3,SAVSTDAT),(0,WORK)                                
*                                  GET EBCDIC DATE OF JOIN                      
         GOTO1 GETDAY,DMCB,WORK,DUB                                             
         CLI   DMCB,X'1'           IS DATE A MONDAY?                            
         BE    TSTA0012            YES                                          
*                                                                               
         ZIC   RE,DMCB             GET DAY OF WEEK NUMBER                       
         BCTR  RE,0                SUBTRACT 1 DAY                               
         LNR   RE,RE               MAKE IT NEGATIVE                             
         ST    RE,DMCB+8           SET ADDAY ADJUST VALUE                       
         GOTO1 ADDAY,DMCB,WORK,WORK,,                                           
*                                                                               
TSTA0012 EQU   *                                                                
         B     TSTA0020                                                         
TSTA0018 EQU   *                                                                
         MVC   SAVSTDAT,RSTASTRT                                                
         GOTO1 DATCON,DMCB,(3,RSTASTRT),(0,WORK)                                
*                                  GET EBCDIC DATE OF JOIN                      
         GOTO1 GETDAY,DMCB,WORK,DUB                                             
         CLI   DMCB,X'1'           IS FIRST DAY MONDAY?                         
         BE    TSTA0020            YES                                          
*                                                                               
         MVC   DUB(4),=C'JNDT'     SET 'JOIN DATE NOT MONDAY' ERROR             
         B     TSTA0140            EXIT WITH ERROR                              
TSTA0020 EQU   *                                                                
         GOTO1 DATCON,DMCB,(0,WORK),(3,EFDTBIN)                                 
*                                  GENERATE 3-CHAR BINARY DATE                  
         LA    RE,1                                                             
         LNR   RE,RE               SET EFF DATE -1                              
         ST    RE,DMCB+8           SET ADDAY ADJUST VALUE                       
         GOTO1 ADDAY,DMCB,WORK,WORK3,,                                          
         GOTO1 DATCON,DMCB,(0,WORK3),(3,EFDTBSUN)                               
*                                  CALCULATE SUNDAY BEFORE EFF DATE             
         GOTO1 GETBROAD,DMCB,(1,WORK3),WORK3+24,GETDAY,ADDAY                    
         GOTO1 DATCON,DMCB,(0,WORK3+30),(3,BENDDT)                              
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(2,EFDTCOMP)                                
*                                  GENERATE 2-CHAR COMPRESSED DATE              
****>>>> GOTO1 DATCON,DMCB,(0,WORK),(5,CTODATE)                                 
         GOTO1 DATCON,DMCB,(3,RSTASTRT),(5,CTODATE)                             
*                                  DISPLAY EFFECTIVE DATE AS ENTERED,           
*                                     NOT AS ADJUSTED TO MONDAY                 
         OI    CTODATEH+6,X'80'     SET TO 'TRANSMIT'                           
***>>>                                                                          
*                                                                               
*   TEST:   INSERT A HISTORY DATE                                               
****     MVC   RSTAHIST,=X'C39D'                                                
*   TEST:   INSERT A HISTORY DATE:  END TEST                                    
*                                                                               
*   IF 'ALLHIST' FORCE A VERY EARLY DATE,IGNORE VALUE OF RSTAHIST.              
*                                                                               
         CLC   =C'ALL',CONREC      ALLHIST REQUEST?                             
         BNE   TSTA0025            NO                                           
**       GOTO1 DATCON,DMCB,(5,WORK),(3,WORK)                                    
**       ZIC   RF,WORK             BACK YEAR UP 20                              
**       S     RF,=F'20'                                                        
**       STC   RF,WORK             PUT YEAR BACK                                
**       GOTO1 DATCON,DMCB,(3,WORK),(2,WORK+3)                                  
**       MVC   HISTDATE,WORK+3     LOAD ALLHIST START DATE                      
         GOTO1 DATCON,DMCB,(3,RSTASTRT),(2,WORK+3)                              
         MVC   HISTDATE,WORK+3     LOAD ALLHIST START DATE                      
         B     TSTA0035                                                         
TSTA0025 EQU   *                                                                
         OC    RSTAHIST,RSTAHIST   ANY HISTORY TAKEOVER DATE?                   
         BNZ   TSTA0030            YES                                          
         MVC   DUB(4),=C'HSDT'     SET 'HIST T/O DATE ABSENT' ERROR             
         B     TSTA0140            EXIT WITH ERROR                              
TSTA0030 EQU   *                                                                
         MVC   HISTDATE,RSTAHIST   SAVE HISTORY TAKEOVER DATE                   
TSTA0035 EQU   *                                                                
         GOTO1 DATCON,DMCB,(2,HISTDATE),(3,HISTDATB)                            
*                                  SAVE HIST T/O DATE AS BINARY                 
         MVC   WORK(3),HISTDATB    CALCULATE EARLY MONTH OF SERVICE             
         MVI   WORK+3,15           FORCE MID-MONTH                              
         GOTO1 DATCON,DMCB,(3,WORK),(3,WORK+3)                                  
         MVC   EARLYMOS(2),WORK+3  SET EARLY MONTH OF SERVICE                   
*                                  FOR BUCKET PROCESSING                        
         GOTO1 DATCON,DMCB,(5,WORK),(0,WORK)                                    
*                                  GET EBCDIC DATE OF RUN                       
         GOTO1 GETDAY,DMCB,WORK,DUB                                             
         CLI   DMCB,X'1'           IS DATE OF RUN MONDAY?                       
         BE    TSTA0040            YES                                          
*                                                                               
         ZIC   RE,DMCB             GET DAY OF WEEK NUMBER                       
         BCTR  RE,0                SUBTRACT 1 DAY                               
         LNR   RE,RE               MAKE IT NEGATIVE                             
         ST    RE,DMCB+8           SET ADDAY ADJUST VALUE                       
         GOTO1 ADDAY,DMCB,WORK,WORK,,                                           
*                                  BACK UP TO MONDAY                            
*                                                                               
TSTA0040 EQU   *                                                                
         GOTO1 DATCON,DMCB,(0,WORK),(2,MONDATE)                                 
***>>>                                                                          
         MVI   TRGTTYPE,C'O'       SET STATION TYPE TO 'OTHER'                  
         LA    RF,RSTAELEM                                                      
TSTA0060 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    TSTA0120            YES - NO X'05' ELEMENT                       
         CLI   0(RF),5             X'05' ELEMENT?                               
         BE    TSTA0080            YES -                                        
         CLI   0(RF),8             X'08' ELEMENT?                               
         BNE   TSTA0100            NO  - NO X'05' ELEMENT                       
*                                                                               
         TM    RSTAOPTA-RSTAXXEL(RF),X'20'                                      
*                                  ALTERNATE CALENDAR STATION?                  
         BNO   TSTA0120            NO  - DON'T RESET - DONE WITH REC            
         L     RE,AMISFLGS         SET A(TWA WORK AREA)                         
         USING MISFLGS,RE                                                       
         MVI   ALTCAL,C'Y'         YES - SET 'ALTERNATE CALENDAR' ON            
*                                                                               
         DROP  RE                                                               
         B     TSTA0120            FINISHED WITH STATION RECORD                 
TSTA0080 EQU   *                                                                
         MVI   TRGTTYPE,C'G'       SET STATION TYPE TO 'GRAPH'                  
         CLC   10(2,RF),=X'0406'   GRAPHNET?                                    
         BE    TSTA0100            YES                                          
         MVI   TRGTTYPE,C'A'       SET STATION TYPE TO 'ACE'                    
TSTA0100 EQU   *                                                                
         ZIC   RE,1(RF)            TAKE ELEMENT LENGTH                          
         AR    RF,RE               BUMP TO NEXT ELEMENT                         
         B     TSTA0060            GO BACK FOR NEXT                             
TSTA0120 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         B     TSTA0160                                                         
TSTA0140 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
TSTA0160 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   CHEKOFF: ACCESS THE SOURCE REP FILE FOR THIS OFFICE.                        
*        IF NOT FOUND, CC NOT ZERO WILL PRODUCE ERROR MESSAGE.                  
*                                                                               
CHEKOFF  NMOD1 0,*COFF*                                                         
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         LA    R2,CTOOFFH          SET A(OFFICE FILTER HEADER)                  
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'04'           SET KEY FOR AGENCY RECORD                    
         MVC   KEY+23(2),SRCEREP   INSERT REP CODE                              
         MVC   KEY+25(2),CTOOFF    INSERT OFFICE CODE                           
         GOTO1 HIGH                READ FOR KEY                                 
*                                                                               
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
*                                                                               
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    COFF0160            YES - KEY VALID AS ENTERED                   
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     COFF0200                                                         
COFF0160 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
COFF0200 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
VKEYNMOD NMOD1 0,**VKEY**                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*   RETRIEVE ORIGINAL SYSTEM NUMBER, TO SUPPRESS SYSTEM SWITCH                  
*        IF SOURCE AND TARGET REPS ARE ON SAME SYSTEM                           
*        DO THIS ONLY IN VKNM, SO IT ONLY GETS ACCOMPLISHED ONCE                
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         MVC   ORIGUTL,FASYS       SAVE ORIGINAL SYSTEM NUMBER                  
         MVC   SAVELUID,FASYM      GET LUID                                     
         DROP  RF                                                               
*                                                                               
         GOTO1 =A(ACCSREP),DMCB,(RC),RR=Y                                       
*                                                                               
         MVI   NEXTSCRN,C'N'       WE'RE NOT SCROLLING                          
*                                                                               
*                                  IF NO FIELDS WERE CHANGED,                   
*                                     USER WANTS TO PAGE TO NEXT                
*                                     SET OF CONTRACTS                          
         XC    CONCOUNT,CONCOUNT   RESET CONTRACT COUNTER                       
         MVI   CONCTFLG,C'N'       SET CONTRACT COUNT FLAG 'NO'                 
         CLC   =C'COUNT',CTOOPT    CONTRACT COUNT REQUEST?                      
         BNE   VKNM0020            NO                                           
*                                                                               
*   COUNT REQUESTED:  START AGAIN AT THE BEGINNING - REVALIDATE                 
*        EVERYTHING, AND START AT THE FIRST RECORD.  OPTION IS                  
*        CLEARED, AND A FLAG IS SET TO PERFORM THE FUNCTION                     
*                                                                               
         MVC   CTOOPT,SPACES       YES - CLEAR OPTION FIELD                     
         MVI   LASTSCRN,C'N'       TURN OFF SWITCH                              
         MVI   CONCTFLG,C'Y'       SET CONTRACT COUNT FLAG 'YES'                
         FOUT  CTOOPTH             SET FIELD TO RETRANSMIT EMPTY                
         B     VKNM0060                                                         
VKNM0020 EQU   *                                                                
         TM    CTOREPH+4,X'20'                                                  
         BZ    VKNM0060                                                         
         TM    CTOSTAH+4,X'20'                                                  
         BZ    VKNM0060                                                         
         TM    CTOOFFH+4,X'20'                                                  
         BZ    VKNM0060                                                         
***      TM    CTODATEH+4,X'20'    NO LONGER AN UNPROTECTED FIELD               
***      BZ    VKNM0060                                                         
         TM    CTOSALH+4,X'20'                                                  
         BZ    VKNM0060                                                         
         TM    CTOTYPH+4,X'20'                                                  
         BZ    VKNM0060                                                         
         TM    CTOAGIH+4,X'20'                                                  
         BZ    VKNM0060                                                         
         CLI   CTOAGI,C'='         PREVALID, AND BROWSER CALL?                  
         BE    VKNM0060            YES - VALIDATE ENTIRE SCREEN AGAIN           
*                                                                               
*                                  IF FIELDS WERE CHANGED, BUT TO               
*                                     THE SAME DATA, USER WANTS TO              
*                                     LIST FROM THE BEGINNING                   
         TM    CTOREPH+4,X'80'                                                  
         BO    VKNM0060                                                         
         TM    CTOSTAH+4,X'80'                                                  
         BO    VKNM0060                                                         
         TM    CTOOFFH+4,X'80'                                                  
         BO    VKNM0060                                                         
***      TM    CTODATEH+4,X'80'    NO LONGER AN UNPROTECTED FIELD               
***      BO    VKNM0060                                                         
         TM    CTOSALH+4,X'80'                                                  
         BO    VKNM0060                                                         
         TM    CTOTYPH+4,X'80'                                                  
         BO    VKNM0060                                                         
         TM    CTOAGIH+4,X'80'                                                  
         BO    VKNM0060                                                         
                                                                                
         L     RF,ACONTROL         SET A(CONTROL INFORMATION)                   
         MVC   CTRLAREA(LCTLAREA),0(RF)                                         
*                                  RESET CONTROL AREA FROM PRIOR RUN            
         MVI   NEXTSCRN,C'Y'                                                    
         B     VKNM0800                                                         
                                                                                
VKNM0060 DS    0H                                                               
         XC    FIRSTKEY,FIRSTKEY                                                
         OI    CTOREPH+4,X'20'     SET VALIDATED                                
         OI    CTOSTAH+4,X'20'     SET VALIDATED                                
         OI    CTOOFFH+4,X'20'     SET VALIDATED                                
****     OI    CTODATEH+4,X'20'    SET VALIDATED (NOW PROTECTED)                
         OI    CTOSALH+4,X'20'     SET VALIDATED                                
         OI    CTOTYPH+4,X'20'     SET VALIDATED                                
         OI    CTOAGIH+4,X'20'     SET VALIDATED                                
         SR    R0,R0               SET CC  ZERO                                 
         B     VKNM0900                                                         
VKNM0800 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
VKNM0900 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
UPDTCON  NMOD1 0,*UPCN*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         CLC   TRGTTYPE,SRCETYPE   SRCE/TARGET SAME STA TYPES?                  
         BE    UPDT0010            YES - NO CHANGE NEEDED                       
         BAS   RE,CHNGTYPE         NO  - CHANGE TYPE, UPDATE AS NEEDED          
UPDT0010 EQU   *                                                                
         L     R5,AIO                                                           
         USING RCONREC,R5                                                       
*                                                                               
***>>>   GOTO1 DATCON,DMCB,(5,WORK),(3,RCONCREA)                                
*                                  RESET CREATE DATE TO RUN DATE                
***>>>   GOTO1 DATCON,DMCB,(5,WORK),(3,RCONHDRD)                                
*                                  RESET HDR CREATE DATE TO RUN DATE            
         CLI   CONSAVED,C'Y'       CONTRACT SAVED?  (FLT DATE CHANGE)           
         BNE   UPDT0060            NO  - LEAVE ORDER AS IS                      
         CLC   RCONDATE(3),HISTDATB                                             
*                                  CONTRACT CROSSES TAKEOVER DATE?              
         BNL   UPDT0030            NO  -  LEAVE FLIGHT START DATE               
*                                                                               
*                                                                               
*   BB DATE IS ALWAYS A MONDAY.  CHECK FLIGHT START DATE.  IF NOT               
*        MONDAY, ADJUST HISTDATB, INSERT INTO FLIGHT TO PROVIDE                 
*        OUT-OF-WEEK ROTATOR SUPPORT.                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,RCONDATE),(0,WORK3)                               
*                                  ORIGINAL FLT START DATE -> EBCDIC            
         GOTO1 GETDAY,DMCB,WORK3,DUB                                            
         CLI   DMCB,1              ORIGINAL START = MONDAY?                     
         BNE   UPDT0020            NO  - SET TO OOWR FLIGHT START               
         MVC   RCONDATE(3),HISTDATB                                             
*                                  YES - RESET FLIGHT START DATE TO             
*                                     TAKEOVER DATE                             
         B     UPDT0030                                                         
UPDT0020 EQU   *                   NO  - OFFSET TO ORIG FLT START DAY           
         ZIC   RE,DMCB             GET DAY OF WEEK NUMBER                       
         BCTR  RE,0                SUBTRACT 1 DAY                               
         ST    RE,DMCB+8           SET ADDAY ADJUST VALUE                       
         GOTO1 DATCON,DMCB,(3,HISTDATB),(0,WORK3)                               
*                                  CONVERT TKO-BB HIST DATE TO EBCDIC           
         GOTO1 ADDAY,DMCB,WORK3,WORK3,,                                         
         GOTO1 DATCON,DMCB,(0,WORK3),(3,RCONDATE)                               
*                                  CONVERT OOWR START TO BINARY                 
UPDT0030 EQU   *                                                                
***CUT FLIGHT END DATE                                                          
         CLC   RCONDATE+3(3),EFDTBSUN                                           
*                                  CONTRACT CROSSES TAKEOVER DATE?              
         BNH   UPDT0040            NO  -  LEAVE FLIGHT END   DATE               
*                                                                               
*                                                                               
*   TKO DATE IS ALWAYS A MONDAY.  CHECK FLIGHT START DATE.  ADJUST TO           
*        SUNDAY, OR OUT-OF-WEEK ROTATOR. INSERT INTO FLIGHT TO PROVIDE          
*        OUT-OF-WEEK ROTATOR SUPPORT.                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,WORK3)                             
*                                  ORIGINAL FLT END DATE -> EBCDIC              
         GOTO1 GETDAY,DMCB,WORK3,DUB                                            
         CLI   DMCB,7              ORIGINAL END = SUNDAY?                       
         BNE   UPDT0035            NO  - SET TO OOWR FLIGHT END                 
         MVC   RCONDATE+3(3),EFDTBSUN                                           
*                                  YES - RESET FLIGHT END   DATE TO             
*                                     TAKEOVER DATE                             
         B     UPDT0040                                                         
UPDT0035 EQU   *                   NO  - OFFSET TO ORIG FLT START DAY           
         ZIC   RE,DMCB             GET DAY OF WEEK NUMBER                       
         SH    RE,=H'7'            SUBTRACT 7: MAKE NEGATIVE                    
         ST    RE,DMCB+8           SET ADDAY ADJUST VALUE                       
         GOTO1 DATCON,DMCB,(3,EFDTBSUN),(0,WORK3)                               
*                                  CONVERT TKO-BB EFF DATE TO EBCDIC            
         GOTO1 ADDAY,DMCB,WORK3,WORK3,,                                         
         GOTO1 DATCON,DMCB,(0,WORK3),(3,RCONDATE+3)                             
*                                  CONVERT OOWR START TO BINARY                 
***CUT FLIGHT END DATE                                                          
UPDT0040 EQU   *                                                                
*        DROP ALL EST/INV ELEMENTS FROM ORDER                                   
*                                                                               
***      GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'03',RCONREC),0,0                
*                                  DELETE OLD ESTIMATE ELTS                     
***      GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'04',RCONREC),0,0                
*                                  DELETE OLD INVOICE  ELTS                     
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'53',RCONREC),0,0                
*                                  DELETE OLD ALT CAL EST ELTS                  
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'54',RCONREC),0,0                
*                                  DELETE OLD ALT CAL INV  ELTS                 
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'63',RCONREC),0,0                
*                                  DELETE OLD TRADE   EST ELTS                  
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'64',RCONREC),0,0                
*                                  DELETE OLD TRADE   INV  ELTS                 
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'A4',RCONREC),0,0                
*                                  DELETE  ALT CAL CTL  ELT                     
         MVI   ELCODE,X'A6'        FIND COVERSHEET ELEMENT                      
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   UPDT0050            ELEMENT NOT FOUND                            
*                                                                               
         USING RCONCVEL,R6                                                      
*                                                                               
         OC    COVSHT#,COVSHT#     ORIG CVRSHEET # SAVED?                       
         BNZ   UPDT0045            YES                                          
         MVC   COVSHT#,RCONCVNM+4  NO  - SAVE ORIG CVRSHEET #                   
         MVC   COVORG#,RCONKCON    SAVE NEW  CONTRACT NUMBER                    
*                                     IN CASE OF COMBO ORDER                    
UPDT0045 EQU   *                                                                
*                                                                               
         MVC   RCONCVNM+4(4),COVORG#                                            
*                                  INSERT NEW CONTRACT NUMBER                   
*                                     (USED IF COVERSHEET KEPT)                 
         CLI   RCONCVNM,X'FF'      CONTRACT NUMBER?                             
*                                                                               
         DROP  R6                                                               
*                                                                               
         BE    UPDT0050            YES - MUST TAKE OVER COVERSHEET              
*                                     (NEW CON# ALREADY INSERTED)               
*                                  NO  -  'NAMED' - DELETE ELEMENT              
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'A6',RCONREC),0,0                
*                                  DELETE  COVER SHEET ELEMENT                  
UPDT0050 EQU   *                                                                
         GOTOX (RFCHKALT,VREPFACS),DMCB,(0,RCONREC),ACOMFACS                    
         MVC   BUCKFLGS(1),0(R1)                                                
*                                  SET ALT CAL FLAGS FOR STATION                
UPDT0060 EQU   *                                                                
         MVI   ELCODE,X'20'        FIND SENDING INFO ID                         
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   UPDT0080            ELEMENT NOT FOUND                            
         XC    2(2,R6),2(R6)       FOUND:  SET SEND ID TO ZERO                  
         MVC   2(2,R6),USERID      INSERT TARGET REP SENDING ID                 
UPDT0080 EQU   *                                                                
         MVI   BYTE,0              SET FLAG TO 'NO'                             
*                                                                               
         LR    R2,RA               USE R2 TO COVER THE ENTRY                    
         AH    R2,=AL2(SFMPROFS-CONHEADH)                                       
         USING SVDSECT,R2                                                       
                                                                                
         TM    SVPGPBIT+SFDEVB,SFDEVA                                           
*                                  CLEAR DEVSAL/DEVTYP FIELDS?                  
         BZ    UPDT0090            NO                                           
*                                                                               
         DROP  R2                                                               
*                                                                               
         MVI   BYTE,1              SET FLAG TO 'YES'                            
UPDT0090 EQU   *                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'18'        FIND DEV SP/CONTYPE ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   UPDT0120            NO CONTYPE ELEMENT                           
         CLC   ORIGUTL,SRCEUTL     SAME SYSTEM?                                 
         BE    UPDT0140            YES - LEAVE ELEMENT AS ENTERED               
         USING RCONDVEL,R6         NO  - SET CODES TO NA                        
         OC    RCONDVSP,RCONDVSP   ANY DEV S/P?                                 
         BZ    UPDT0100            NO                                           
         MVC   RCONDVSP,=C'NA '    SET TAKEOVER VALUE                           
         CLI   BYTE,0              SEND A VALUE?                                
         BE    UPDT0100            YES - SEND 'NA'                              
         XC    RCONDVSP,RCONDVSP   NO  - CLEAR FIELD                            
UPDT0100 EQU   *                                                                
         OC    RCONDVCT,RCONDVCT   ANY DEV CONTRACT TYPE?                       
         BZ    UPDT0140            NO                                           
         MVC   RCONDVCT,=C'NA'                                                  
         CLI   BYTE,0              SEND A VALUE?                                
         BE    UPDT0110            YES - SEND 'NA'                              
         XC    RCONDVCT,RCONDVCT   NO  - CLEAR FIELD                            
UPDT0110 EQU   *                                                                
         B     UPDT0140                                                         
*                                                                               
         DROP  R6                                                               
*                                                                               
UPDT0120 EQU   *                                                                
*                                  NO ELEMENT FOUND:  IS IT NEEDED?             
         LA    RF,CTOLAST          FIND CONTRACT PROFILE STORED BY              
         A     RF,=F'11000'           SFM BASE MODULE                           
*                                                                               
         USING MISFLGS,RF                                                       
*                                                                               
         TM    CNTGPBIT+CNTRDEVB,CNTRDEVA                                       
*                                  REQUIRES DEVELOPMENTAL CODES?                
         DROP  RF                                                               
*                                                                               
         BNO   UPDT0140            NO  - LEAVE AS IS                            
*                                  YES - BUILD/INSERT NEW ELEMENT               
         XC    WORK2,WORK2         CLEAR ELEMENT BUILD AREA                     
         MVC   WORK2(2),=X'1808'                                                
         CLI   BYTE,1              DON'T SEND VALUES?                           
         BE    UPDT0130            YES - DON'T SEND VALUES                      
         MVC   WORK2+2(3),=C'NA '  INSERT NA DEV S/P                            
         MVC   WORK2+5(2),=C'NA'   INSERT NA DEV CONTYP                         
UPDT0130 EQU   *                                                                
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,WORK2,0                    
*                                  INSERT DEV CODE ELEMENT                      
UPDT0140 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,EFDTBSUN),(5,NWCOMDAT)                            
*                                  INSERT TERMINATE DATE INTO ELEMENT           
         MVC   NWOLDREP,SRCENAME   INSERT SOURCE (OLD) REP                      
         GOTO1 HEXOUT,DMCB,SRCECON,NWOLDCON,4,=C'TOG'                           
*                                  INSERT ORIGINAL CONTRACT NUMBER              
         SR    R1,R1               CLEAR COUNTER                                
         MVI   NWCOMELT,2          SET TO 'CONTRACT COMMENT ELEMENT'            
         CLC   =C'ALL',CONREC      ALLHIST REQUEST?                             
         BNE   UPDT0150            NO                                           
         MVC   NWCOMBBL,=C'ALLH'                                                
         MVC   NWCOMTRM,=C'IST '                                                
UPDT0150 EQU   *                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'02'        FIND CONTRACT COMMENT ELEMENT                
         BAS   RE,GETEL                                                         
         BNE   UPDT0220            NO COMMENT ELEMENT:  INSERT                  
*                                     TKO COMT AS CONTRACT COMMENT              
         B     UPDT0180            COMMENT FOUND: CHECK IT                      
UPDT0160 EQU   *                                                                
         BAS   RE,NEXTEL           LOOK FOR NEXT COMMENT                        
         BNE   UPDT0240            NO COMMENT ELEMENT                           
UPDT0180 EQU   *                                                                
         LA    R1,1(R1)            INCREMENT COUNTER                            
         CLC   =C'C=',2(R6)        STANDARD COMMENT?                            
         BE    UPDT0200            YES - REUSE IT                               
         CLC   =C'SC=',2(R6)       STORED COMMENT?                              
         BNE   UPDT0160            NO  - GO CHECK NEXT COMMENT                  
UPDT0200 EQU   *                                                                
         MVI   0(R6),X'FF'         SET CONTRACT COMMENT TO DELETE               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'FF',RCONREC),0,0                
*                                  DELETE CONTRACT COMMENT                      
UPDT0220 EQU   *                                                                
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,NWCOMELT,0                 
*                                  INSERT CONTRACT COMMENT ELEMENT              
         B     UPDT0320            EXIT                                         
UPDT0240 EQU   *                                                                
         STC   R1,BYTE             ONLY 1 COMMENT IN RECORD?                    
         CLI   BYTE,1                                                           
         BE    UPDT0220            YES - INSERT TKO CMT AS 2ND COMMENT          
*                                  NO  - NO ROOM: CHECK ORDER COMMENT           
         SR    R1,R1               CLEAR COUNTER                                
         MVI   NWCOMELT,X'82'      SET TO 'ORDER COMMENT ELEMENT'               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'82'        FIND ORDER COMMENT ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   UPDT0220            NO COMMENT ELEMENT:  INSERT                  
         B     UPDT0280                                                         
*                                     TKO COMT AS ORDER COMMENT                 
UPDT0260 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BNE   UPDT0300            NOT FOUND                                    
UPDT0280 EQU   *                                                                
         ST    R6,FULL             SAVE A(LAST R6 FOUND)                        
         LA    R1,1(R1)            INCREMENT COUNTER                            
         B     UPDT0260            GO BACK FOR NEXT                             
UPDT0300 EQU   *                                                                
         STC   R1,BYTE             CHECK COUNTER                                
         CLI   BYTE,10             ALL ORDER COMMENTS IN USE?                   
         BNE   UPDT0220            NO  - ADD COMMENT AS NEXT ONE                
         L     R6,FULL             YES - RESET A(LAST COMMENT)                  
         B     UPDT0200            DELETE LAST COMMENT, INSERT                  
*                                     TAKEOVER COMMENT AS LAST                  
UPDT0320 EQU   *                                                                
         XIT1                                                                   
*                                                                               
NWCOMELT DC    X'003C'     (2)     NEW COMMENT ELEMENT W/TAKEOVER               
         DC    X'FF'       (1)     FORCE TO SORT LAST                           
NWCOMBBL DC    C'BBL '     (4)                                                  
NWCOMTRM DC    C'TRM '     (4)                                                  
NWCOMDAT DS    CL8         (8)                                                  
         DC    C' OLD REP/CON '  (13)                                           
NWOLDREP DS    CL19              (19)                                           
         DC    C'#'              (1)                                            
NWOLDCON DS    CL8               (8)                                            
LNWCOMEL EQU   *-NWCOMELT                                                       
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*   CHNGTYPE:  SRCE AND TARGET STATION TYPES ARE DIFFERENT.  MAKE               
*        ADJUSTMENTS TO THE REQUIRED ELEMENTS.                                  
*                                                                               
CHNGTYPE NTR1                                                                   
         L     R5,AIO                                                           
         USING RCONREC,R5                                                       
         MVC   SAVEMODE,RCONMODR+1 SAVE ORIGINAL TYPE                           
         CLI   TRGTTYPE,C'O'       TARGET TYPE: OTHER                           
         BNE   CHNG0040            NO                                           
         NI    RCONMODR+1,X'FF'-ACEMASK-TWXMASK                                 
*                                  MAKE IT NEITHER                              
         B     CHNG0900            EXIT                                         
CHNG0040 EQU   *                                                                
         CLI   TRGTTYPE,C'A'       TARGET TYPE: ACE                             
         BNE   CHNG0080            NO                                           
         OI    RCONMODR+1,ACEMASK  MAKE IT ACE.                                 
         NI    RCONMODR+1,X'FF'-TWXMASK                                         
*                                  CLEAR TWX MASK                               
         B     CHNG0120                                                         
CHNG0080 EQU   *                                                                
         CLI   TRGTTYPE,C'G'       TARGET TYPE: GRAPH/TWX                       
         BE    *+6                 YES -                                        
         DC    H'0'                NO  - DUMP IT                                
         OI    RCONMODR+1,TWXMASK  MAKE IT GRAPH/TWX                            
         NI    RCONMODR+1,X'FF'-ACEMASK                                         
*                                  CLEAR ACE MASK                               
CHNG0120 EQU   *                                                                
         TM    SAVEMODE,ACEMASK+TWXMASK                                         
*                                  WAS ACE/GRAPH?                               
         BNZ   CHNG0900            YES - NO NEW ELTS NEEDED                     
         BAS   RE,MAKE20EL         NO  - CREATE NEW ELTS                        
         BAS   RE,UPDATE1F                                                      
         BAS   RE,MAKE82EL                                                      
         B     CHNG0900                                                         
CHNG0900 EQU   *                                                                
         XIT1                                                                   
SAVEMODE DS    CL1                                                              
ACEMASK  EQU   X'80'                                                            
TWXMASK  EQU   X'40'                                                            
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*- MAKE20EL --- MAKE A CONTRACT X'20' ELEMENT (SEND ORDER)                      
*                                                                               
MAKE20EL NTR1                                                                   
*                                                                               
         L     R5,AIO                                                           
         USING RCONREC,R5                                                       
         GOTO1 HELLO,DMCB,(C'D',=C'REPFIL'),(X'20',RCONREC),0,0                 
*                                  DELETE OLD X'20' ELT                         
         XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         USING RCONSEND,R2                                                      
         MVI   RCONSNCO,X'20'      BUILD WITH NEW VERSION LENGTH                
         MVI   RCONSNLN,RCONSN3Q                                                
****>>>  MVC   RCONSSID(2),FAKESSID                                             
*                                  NO SENDING ID IS SET                         
         MVI   RCONSENF,X'10'                                                   
         MVI   RCONSRV,X'01'                                                    
*        MVC   RCONSRDT(2),TODAY2BT                                             
*        UNPK  DUB,SENDTIME                                                     
*        MVC   RCONSRTI,DUB+1      TIME                                         
         MVI   RCONSSV,X'00'                                                    
*        MVC   RCONSSDT(2),TODAY2BT                                             
*        UNPK  DUB,SENDTIME                                                     
*        MVC   RCONSSTI,DUB+1      TIME                                         
         DROP  R2                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFIL'),(0,RCONREC),(0,WORK),0              
         CLI   DMCB+12,00                                                       
         BE    *+6                                                              
         DC    H'0'                CAN'T ADD?  DUMP IT OUT                      
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*- UPDATE1F --- UPDATE OR ADD A X'1F' ELEMENT (EXTENDED DESCRIPTION)            
*                                                                               
UPDATE1F NTR1                                                                   
*                                                                               
         L     R5,AIO                                                           
         USING RCONREC,R5                                                       
*                                                                               
UP1F10   EQU   *                                                                
         GOTO1 HELLO,DMCB,(C'G',=C'REPFIL'),(X'1F',RCONREC),0,0                 
         CLI   DMCB+12,00          ELEMENT FOUND?                               
         BE    UP1F20              YES - LEAVE IT ALONE                         
         XC    WORK(30),WORK       NO  - BUILD IT                               
         MVC   WORK(2),=X'1F18'                                                 
         GOTO1 HELLO,DMCB,(C'P',=C'REPFIL'),(0,RCONREC),(0,WORK),0              
         CLI   DMCB+12,00          ELEMENT ADDED?                               
         BE    UP1F10              YES - GO BACK AND GET IT AGAIN               
*                                     TO ADD TOTALS TO ELEMENT                  
         DC    H'0'                NO  - DUMP IT OUT                            
UP1F20   EQU   *                                                                
         L     R2,DMCB+12                                                       
         USING RCONXEL,R2                                                       
         MVI   RCONCONF,X'40'                                                   
         LA    R6,RCONREC                                                       
         SR    R4,R4                                                            
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
UP1F30   EQU   *                                                                
         BNE   UP1F40                                                           
         ZICM  R3,6(R6),4                                                       
         AR    R4,R3                                                            
         BAS   RE,NEXTEL                                                        
         B     UP1F30                                                           
UP1F40   EQU   *                                                                
         STCM  R4,15,RCONTOT                                                    
         XIT1                                                                   
*                                                                               
         DROP  R2,R5                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
*- MAKE82EL --- FORCE A CONTRACT X'82' ELEMENT (REP ORDER COMM)                 
*                                                                               
MAKE82EL NTR1                                                                   
*                                                                               
         L     R5,AIO                                                           
         USING RCONREC,R5                                                       
*                                                                               
MK8210   EQU   *                                                                
         GOTO1 HELLO,DMCB,(C'G',=C'REPFIL'),(X'82',RCONREC),0,0                 
         CLI   DMCB+12,00          82 ELT FOUND?                                
         BNE   MK8220              NO  - ADD NEW ONE                            
         GOTO1 HELLO,DMCB,(C'D',=C'REPFIL'),(X'82',RCONREC),0,0                 
*                                  YES - DELETE IT                              
MK8220   EQU   *                                                                
         XC    WORK(30),WORK                                                    
         MVC   WORK(2),=X'821A'                                                 
         MVC   WORK+2(16),=C'SWITCHED BY DDS '                                  
         MVC   WORK+18(8),RCDATE                                                
         GOTO1 HELLO,DMCB,(C'P',=C'REPFIL'),(0,RCONREC),(0,WORK),0              
         CLI   DMCB+12,00          ELT ADDED?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NO  - DUMP                                   
         XIT1                                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   NEXTCONS:  RETRIEVE, SET UP NEXT CONTRACT NUMBER(S) FOR                     
*        STANDALONES AND COMBOS.                                                
*                                                                               
NEXTCONS NMOD1 0,*NEXT*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R4,4(R1)            RESET A(CONTRACT)                            
         USING RCONREC,R4                                                       
         L     R5,8(R1)            RESET A(EQUITABL)                            
         USING EQUITABL,R5                                                      
*                                                                               
*              GET NEXT REP CONTRACT NUMBER                                     
*                                                                               
         ZAP   WORK(5),=P'99999999'                                             
         ZAP   WORK+5(5),=P'99999999'                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'           PASSIVE POINTER KEY TYPE                     
         MVC   KEY+21(2),TWAAGY    ALPHA REP CODE                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         MVI   RDUPDATE,C'Y'       LOCK 8C KEYS                                 
         GOTO1 HIGH                                                             
         CLC   KEY(23),KEYSAVE     SAME REP?                                    
         BNE   *+10                                                             
*                                                                               
*                                  GET NEXT CONTRACT NUMBER                     
*                                                                               
         MVO   WORK+5(5),KEY+23(4) K NUMBER                                     
         SP    WORK(5),WORK+5(5)   GET POSITIVE                                 
         AP    WORK(5),=P'2'       NEXT K NUMBER +1 (SKIP 1 NUMBER)             
         MVO   WORK+10(5),WORK(5)                                               
         MVC   RCONKCON,WORK+10    TO K KEY                                     
         MVC   TRGTCON,WORK+10     SAVE NEW CONTRACT NUMBER                     
         MVC   ETNCONUM,WORK+10    SAVE NEW CONTRACT NUMBER                     
*                                                                               
*   CHECK IF ANY COMBO ORDERS NEED NEW NUMBERS ALSO                             
*                                                                               
         XC    NEWCOMBO,NEWCOMBO                                                
         L     RF,ACMBNTRY         SET A(COMBO ENTRY)                           
         OC    0(LCMBBUCK,RF),0(RF)  ANYTHING IN COMBO ENTRY?                   
         BZ    NCON0040            NO  - SKIP TESTS                             
         MVC   NEWCOMBO(2),=X'170B'                                             
*                                  YES - SET NEW X'17' ELEMENT                  
         MVC   NEWCOMBO+2(5),RCONKSTA                                           
*                                  INSERT STATION INTO ELEMENT                  
         MVC   NEWCOMBO+7(4),RCONKCON                                           
*                                  INSERT NEW CONTRACT INTO ELEMENT             
         LA    RE,NEWCOMBO+11      SET A(1ST SLOT IN NEW ELT)                   
         LA    R0,3                MAX OF THREE CONTRACTS IN ORDER              
NCON0020 EQU   *                                                                
         OC    0(LCMBNTRY,RF),0(RF)  ANY ENTRY?                                 
         BZ    NCON0040            NO  - FINISHED                               
         AP    WORK(5),=P'1'       NEXT K NUMBER                                
         MVO   WORK+10(5),WORK(5)                                               
         MVC   ACMBNCON(4,RF),WORK+10                                           
*                                  SAVE NEXT CONTRACT NUMBER                    
         MVC   0(5,RE),ACMBSTAT(RF)                                             
*                                  INSERT STATION INTO NEW ELT                  
         MVC   5(4,RE),ACMBNCON(RF)                                             
*                                  INSERT NEW CON# INTO NEW ELT                 
         ZIC   R1,NEWCOMBO+1       BUMP ELEMENT LENGTH                          
         LA    R1,9(R1)                                                         
         STC   R1,NEWCOMBO+1       REINSERT LENGTH INTO ELEMENT                 
         LA    RE,9(RE)            BUMP TO NEXT SLOT IN ELEMENT                 
         LA    RF,LCMBNTRY(RF)     BUMP TO NEXT IN THREE                        
         BCT   R0,NCON0020         GO BACK FOR NEXT                             
NCON0040 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R4,R5                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PRODCODE: CHECK IF PRODUCT CODE HAS BEEN ENTERED.  IF SO,           *         
*        PRODUCT CODE FIELD MUST BE SET TO ZERO, AND A PRODUCT        *         
*        EXPANSION ELEMENT INSERTED INTO THE CONTRACT RECORD.         *         
* P1 HAS ADV CODE                                                     *         
* P2 HAS PRD CODE                                                     *         
* WORK WILL CONTAIN EXPANDED NAME                                     *         
***********************************************************************         
PRODCODE NMOD1 0,*PCOD*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XC    WORK,WORK                                                        
         L     R2,4(R1)            SET A(RCONREC)                               
         USING RCONREC,R2                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RPRDKEY,R6                                                       
         MVI   RPRDKTYP,X'09'                                                   
         MVC   RPRDKADV,RCONKADV   INSERT ADVERTISER CODE                       
         MVC   RPRDKPRD,RCONPRD    INSERT PRODUCT CODE                          
         MVC   RPRDKREP,SRCEREP                                                 
*                                                                               
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RPRDKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         MVC   SAVEAIO,AIO         SAVE A(IO AREA)                              
         MVC   AIO,AIO2            RESET A(IO AREA) TO IOAREA2                  
         L     R6,AIO2             SET A(IOAREA # 2)                            
         USING RPRDREC,R6                                                       
*                                                                               
         GOTO1 GETREC,DMCB,RPRDREC                                              
*                                                                               
         MVC   AIO,SAVEAIO         RESET ORIGINAL IO AREA                       
*                                                                               
         MVC   WORK(2),=X'0516'    SET ELEMENT CODE + LENGTH                    
         MVC   WORK+2(L'RPRDNAME),RPRDNAME                                      
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,WORK,0                     
*                                  INSERT PRODUCT EXPANSION                     
         MVC   RCONPRD,SPACES      CLEAR PRODUCT CODE                           
         DROP  R6                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ACCSREP:  RETRIEVE THE TARGET REP ID CODE                                     
*                                                                               
* OUTPUT - THREE-CHARACTER REP ID WILL BE PLACED IN 'TRGTREP'                   
***********************************************************************         
ACCSREP  NMOD1 0,*ACCS*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'           FIND CONTROL USER ID RECORD                  
         MVC   WORK+23(2),TWAORIG  INSERT TARGET REP SYS ID #                   
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AIO                      
         L     R1,AIO                                                           
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BE    *+6                 FOUND                                        
         DC    H'0'                REP NOT FOUND???                             
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
ACRP0020 EQU   *                                                                
         CLI   0(R1),X'02'         AGENCY ID ELEMENT?                           
         BNE   ACRP0040            NO                                           
         MVC   TRGTREP,2(R1)       YES - SAVE 3-CHAR REP ID                     
*                                     REP CODES IN STATION RECORD               
*                                     PERMIT ONLY THREE CHARACTERS              
         B     ACRP0060            FINISHED                                     
ACRP0040 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD                                
         BNE   ACRP0020            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
ACRP0060 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   PAPERWRK:  ROUTINE CYCLES THROUGH TABLE, AND GENERATES                      
*        GLOBBER CALL TO CONTRACT PROGRAM FOR EACH CONTRACT                     
*        TAKEN OVER BY THE RUN.                                                 
PAPERWRK NMOD1 0,*PAPR*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,AEQUITBL         SET A(EQUIV TABLE)                           
*                                                                               
PWRK0020 EQU   *                                                                
         OC    0(4,R2),0(R2)       ANY ENTRY IN SLOT?                           
         BZ    PWRK0800            NO  - PAPERWORK COMPLETE                     
         TM    0(R2),X'40'         ENTRY PROCESSED?                             
         BZ    PWRK0040            NO  - BOUNCE TO NEXT                         
         TM    0(R2),X'10'         YES - PAPERWORK ALREADY DONE?                
         BO    PWRK0040            YES - BOUNCE TO NEXT                         
         B     PWRK0060            NO  - DO PAPERWRK                            
PWRK0040 EQU   *                                                                
         LA    R2,LEQUITBL(R2)     BUMP TO NEXT TABLE ENTRY                     
         B     PWRK0020            GO BACK AND CHECK NEXT                       
PWRK0060 EQU   *                                                                
         OI    0(R2),X'10'         SET 'PAPERWORK ALREADY DONE'                 
         GOTO1 =A(SUBROUT),DMCB,(RC),('QGOCON',0),(R2),RR=YES                   
*                                                                               
*   SHOULD NOT RETURN FROM SUBROUT CALL, WHICH WILL TRANSFER TO                 
*        'CONTRACT' PROGRAM TO PRODUCE PAPERWORK                                
*                                                                               
PWRK0800 EQU   *                                                                
         XIT1                                                                   
*                                                                               
*   IF PWRK0800 REACHED, ALL ENTRIES HAVE BEEN PROCESSED.  JOB WILL             
*        TERMINATE VIA NORMAL MEANS                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
****>>>>                                                                        
*   CHKCOMBO:  CHECK FOR PRESENCE OF X'17' ELEMENT IN CONTRACT REC.             
*        IF PRESENT, INSPECT EACH ENTRY.  IF STATION ON BOTH SOURCE             
*        AND TARGET ARE PREP'D FOR TAKEOVER, INSERT AN ENTRY INTO               
*        THE EQUITABL, WHICH WILL GENERATE A TAKEOVER ORDER.                    
*                                                                               
CHKCOMBO NMOD1 0,*CMBO*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            RESET A(EQUITABL)                            
         USING EQUITABL,R2                                                      
*                                                                               
         L     R5,8(R1)            RESET A(CONTRACT)                            
         USING RCONREC,R5                                                       
*                                                                               
         XC    CMBODISP,CMBODISP   CLEAR DISP INTO COMBO TABLE                  
         MVI   ELCODE,X'17'        FIND COMBO CONTRACT ELEMENT                  
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   CMBO0800            ELEMENT NOT FOUND                            
         ZIC   RF,1(R6)            DETERMINE NUMBER OF PARTICIPATING            
         BCTR  RF,0                SUBTRACT TWO FOR CONTROL                     
         BCTR  RF,0                                                             
         SR    RE,RE                                                            
         D     RE,=F'9'            DIVIDE BY LENGTH OF STA/CON#                 
         LR    R0,RF               SET LOOP CONTROL                             
*                                                                               
         LA    R6,2(R6)            BUMP TO 1ST COMBO ENTRY                      
*                                                                               
*   R4 WILL SERVE AS A POSITIONAL INDICATOR.  UP TO FOUR ORDERS MAY             
*        PARTICIPATE IN A COMBO ORDER.  REG4 WILL INITIALLY CONTAIN             
*        8 (X'00001000').  IF THE FIRST ORDER IS PARTICIPATING (AND NOT         
*        THE 'BASE' ORDER), THE REG WILL BE OR'D WITH THE FLAG BYTE.            
*        IN ALL CASES, THE BIT WILL BE SLID RIGHT ONE POSITION FOR              
*        EACH TEST.  AT THE END, IF THE FIRST CONTRACT PARTICIPATES,            
*        THE X'08' BIT WILL BE SET.  IF THE SECOND, THE X'04' BIT               
*        WILL BE SET, ETC.                                                      
*                                                                               
         LA    R4,8                SET PARTICIPATING FLAG                       
*                                                                               
*                                  FIRST TEST IS ALREADY ON SOURCE SIDE         
CCOM0020 EQU   *                                                                
         CLC   5(4,R6),ETOCONUM    IS THIS BASE CONTRACT?                       
         BE    CCOM0120                                                         
*                                                                               
CCOM0040 EQU   *                                                                
         BAS   RE,CMBOSSTA         NO  - CHECK SOURCE STATION FOR PREP          
         BNZ   CCOM0120            REJECTED ON SOURCE: SKIP ORDER               
*                                     DON'T SWITCH FILES                        
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
         BAS   RE,CMBOTSTA         CHECK TARGET STATION FOR PREP                
         BNZ   CCOM0100            REJECTED ON TARGET: SKIP ORDER               
*                                     SWITCH BACK TO SOURCE FILE                
         ZIC   RF,ETCDEFLG+1       COMBO ORDER FOUND                            
*                                     UPDATE PARTICIPATING FLAGS                
         OR    RF,R4               MERGE FLAG INTO PREVIOUS                     
         STC   RF,ETCDEFLG+1       PUT COUNT BACK                               
*                                                                               
*                                                                               
*   EACH COMBO ENTRY CONSISTS OF UP TO THREE ORDERS.  EACH ORDER                
*      IS COMPOSED OF:                                                          
*        BYTES 0  -  4  = STATION CALL LETTERS                                  
*              5  -  8  = ORIGINAL CONTRACT NUMBER                              
*              9  - 12  = NEW      CONTRACT NUMBER                              
*             13  - 14  = OLD GROUP/SUBGROUP                                    
*             15  - 16  = NEW GROUP/SUBGROUP                                    
*             17  -     = OLD STATION TYPE                                      
*             18  -     = NEW STATION TYPE                                      
*   EQUATES FOR FIELDS                                                          
ACMBSTAT EQU   0                                                                
ACMBOCON EQU   5                                                                
ACMBNCON EQU   9                                                                
ACMBOGRP EQU   13                                                               
ACMBNGRP EQU   15                                                               
ACMBOSTY EQU   17                                                               
ACMBNSTY EQU   18                                                               
LCMBNTRY EQU   19                                                               
LCMBBUCK EQU   57                                                               
*                                                                               
         L     RF,ACMBNTRY         GET COMBO TABLE ENTRY IN USE                 
         A     RF,CMBODISP         ADD DISPLACEMENT TO NEXT SLOT                
         MVC   ACMBSTAT(5,RF),0(R6)   MOVE IN STATION LETTERS                   
         MVC   ACMBOCON(4,RF),5(R6)   MOVE ORIGINAL CON# TO SLOT                
         MVC   ACMBOGRP(2,RF),TEMPSGRP                                          
*                                  INSERT SOURCE GROUP INTO TABLE               
         MVC   ACMBNGRP(2,RF),TEMPTGRP                                          
*                                  INSERT TARGET GROUP INTO TABLE               
         MVC   ACMBOSTY(1,RF),TEMPSTYP                                          
*                                  INSERT SOURCE TYPE  INTO TABLE               
         MVC   ACMBNSTY(1,RF),TEMPTTYP                                          
*                                  INSERT TARGET TYPE  INTO TABLE               
         L     RF,CMBODISP         BUMP DISPLACEMENT TO NEXT SLOT               
         LA    RF,LCMBNTRY(RF)                                                  
         ST    RF,CMBODISP         PUT IT BACK                                  
CCOM0100 EQU   *                                                                
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
CCOM0120 EQU   *                                                                
         SRL   R4,1                MOVE BIT DOWN ONE POSITION                   
         LA    R6,9(R6)            BUMP TO NEXT PARTICIPATING STA               
         BCT   R0,CCOM0020         GO BACK FOR NEXT                             
*                                                                               
         DROP  R2,R5                                                            
*                                                                               
CMBO0800 EQU   *                                                                
         L     RF,ACMBNTRY         BUMP COMBO TABLE UP                          
         LA    RF,LCMBBUCK(RF)     BUMP PAST THREE ENTRIES FOR ORDER            
         ST    RF,ACMBNTRY         PUT IT BACK                                  
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***>>>>                                                                         
*                                                                               
*   CMBOSSTA: ACCESS THE SOURCE STA RECORD TO DETERMINE IF STATION              
*        IS CLEARED FOR TAKEOVER PROCESSING.  THIS CODE CHECKS THE              
*        SOURCE REP'S STATION RECORD, WHERE                                     
*              1.  THE STATION MUST EXIST                                       
*              2.  THE LEAVE DATE MUST BE EQUAL OR BEFORE THE                   
*                  EFFECTIVE DATE                                               
*              3.  THE NEW REP MUST BE THE TARGET REP                           
*              4.  SOURCE REP HAS NOT LOCKED STATION (OPT #15)                  
*                                                                               
CMBOSSTA NTR1                                                                   
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,2               INSERT RECORD TYPE                           
         MVC   KEY+20(2),SRCEREP   INSERT SOURCE REP CODE                       
         MVC   KEY+22(5),0(R6)     INSERT STATION CALL LETTERS                  
*                                     FROM COMBO CONTROL ELEMENT                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     RECORD FOUND?                                
         BNE   CSRC0160            NO  - EXIT WITH ERROR                        
*                                                                               
         MVC   SAVEAIO2,AIO        SAVE A(IO AREA)                              
         MVC   AIO,AIO2            RESET A(IO AREA) TO IOAREA2                  
*                                                                               
         L     R5,AIO2                                                          
         USING RSTAREC,R5                                                       
*                                                                               
         GOTO1 GETREC,DMCB,RSTAREC RETRIEVE STATION RECORD                      
*                                                                               
         MVC   AIO,SAVEAIO2        RESET ORIGINAL IO AREA                       
*                                                                               
         OC    RSTAEND,RSTAEND     LEAVE DATE ENTERED?                          
         BZ    CSRC0160            NO  - STA NOT MARKED FOR TAKEOVER            
         CLC   SAVSTDAT,RSTAEND    COMPARE AGAINST ORIGINAL END                 
         BL    CSRC0160            EFFECTIVE DATE BEFORE LEAVE:                 
*                                     REJECTED                                  
         DROP  R5                                                               
*                                                                               
         L     R6,AIO2                                                          
*                                                                               
         USING RSTAFNEL,R6                                                      
         MVI   ELCODE,X'0C'        GET FORMER/NEW REP ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   CSRC0160            NOT FOUND - REJECT                           
         LA    RF,2                SET COMPARE LENGTH = 3 CHARS                 
         CLI   RSTAFNNE+2,C' '     LAST CHAR NEW REP = SPACE?                   
         BE    CSRC0020            YES - DROP COMPARE 1 POSITION                
         CLI   RSTAFNNE+2,X'00'    LAST CHAR NEW REP = BINARY ZERO?             
         BNE   CSRC0040            NO  -                                        
CSRC0020 EQU   *                                                                
         LA    RF,1                SET COMPARE LENGTH = 2 CHARS                 
CSRC0040 EQU   *                                                                
         EX    RF,CSRC004X         COMPARE BY LENGTH                            
         B     CSRC0060                                                         
CSRC004X CLC   RSTAFNNE(0),TRGTREP                                              
*                                  NEW REP VS SCREEN TARGET REP                 
CSRC0060 EQU   *                                                                
         BNE   CSRC0160            NOST SAME REP - REJECT                       
*                                                                               
         DROP  R6                                                               
***>>>                                                                          
*                                                                               
*   DETERMINE IF OLD REP HAS BARRED TAKEOVER FROM THIS STATION                  
*                                                                               
         L     R6,AIO2                                                          
         USING RSTAXXEL,R6                                                      
         MVI   ELCODE,X'08'        GET EXTENDED DESCRIPTION ELEMENT             
         BAS   RE,GETEL                                                         
         BNE   CSRC0200            NOT FOUND - PERMIT                           
*                                     SHOULDN'T REALLY HAPPEN                   
*                                                                               
         TM    RSTAOPTA,X'04'      STATION BARRED BY OLD REP?                   
         BNO   CSRC0200            NO                                           
*                                                                               
         DROP  R6                                                               
***>>>                                                                          
*                                  LOCKED   - REJECT IT                         
*                                                                               
CSRC0160 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     CSRC0500                                                         
CSRC0200 EQU   *                                                                
         L     R5,AIO2             RESET A(STATION RECORD)                      
         USING RSTAREC,R5                                                       
*                                                                               
         MVC   TEMPSGRP,RSTAGRUP   SAVE SOURCE GROUP                            
***>>>                                                                          
         MVI   TEMPSTYP,C'O'       SET STATION TYPE TO 'OTHER'                  
         LA    RF,RSTAELEM                                                      
CSRC0220 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    CSRC0260            YES - NO X'05' ELEMENT                       
         CLI   0(RF),5             END OF RECORD?                               
         BNE   CSRC0240            YES - NO X'05' ELEMENT                       
         MVI   TEMPSTYP,C'G'       SET STATION TYPE TO 'GRAPH'                  
         CLC   10(2,RF),=X'0406'   GRAPHNET?                                    
         BE    CSRC0260            YES                                          
         MVI   TEMPSTYP,C'A'       SET STATION TYPE TO 'ACE'                    
         B     CSRC0260            YES                                          
CSRC0240 EQU   *                                                                
         ZIC   RE,1(RF)            TAKE ELEMENT LENGTH                          
         AR    RF,RE               BUMP TO NEXT ELEMENT                         
         B     CSRC0220            GO BACK FOR NEXT                             
CSRC0260 EQU   *                                                                
***>>>                                                                          
         SR    R0,R0               SET CC = ZERO                                
CSRC0500 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
****>>>                                                                         
*                                                                               
*   CMBOTSTA: ACCESS THE REP STATION RECORD TO DETERMINE IF STATION             
*        IS CLEARED FOR TAKEOVER PROCESSING.  THIS CODE CHECKS THE              
*        SIGNON REP'S STATION RECORD, WHERE                                     
*              1.  THE STATION MUST EXIST                                       
*              2.  THE JOIN DATE MUST BE EQUAL OR BEFORE THE                    
*                  EFFECTIVE DATE                                               
*              3.  CONTRACT MUST NOT HAVE A '9E' KEY ALREADY ON                 
*                  FILE.                                                        
*                                                                               
CMBOTSTA NTR1                                                                   
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,2               INSERT RECORD TYPE                           
         MVC   KEY+20(2),TWAAGY    INSERT REP CODE                              
         MVC   KEY+22(5),0(R6)     INSERT STATION CALL LETTERS                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     RECORD FOUND?                                
         BNE   CTAR0400            NO  - EXIT WITH ERROR                        
         GOTO1 GETREC              RETRIEVE STATION RECORD                      
         L     R5,AIO                                                           
         USING RSTAREC,R5                                                       
         CLC   RSTASTRT,EFDTBIN    SAME EFFECTIVE DATE AS SOURCE STA?           
         BNE   CTAR0400            NO  - DON'T USE IT                           
*                                                                               
*        COMBO STATION MUST BE SET UP TO START SAME DAY AS                      
*        BASE (SOURCE) STATION.  ORDERS MUST BE KEPT IN SYNCH.                  
*                                                                               
*        LOOK FOR A '9E' LOCKOUT KEY FOR THIS ORDER.  IF PRESENT,               
*        DON'T USE THE ORDER.                                                   
*                                                                               
         CLC   =C'NO9E',CTOOPT     'IGNORE 9E' OPTION SET?                      
         BE    CTAR0080            YES - NO 9E KEY GENERATED                    
         CLC   =C'NOPE',CTOOPT     'IGNORE 9E/NO PW' OPTION SET?                
         BE    CTAR0080            YES - NO 9E KEY GENERATED                    
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'9E'           INSERT RECORD TYPE                           
         MVC   KEY+12(2),TWAAGY    INSERT REP CODE                              
         MVC   KEY+14(5),0(R6)     INSERT STATION CALL LETTERS                  
         MVC   KEY+19(2),SRCEREP   INSERT SOURCE REP CODE                       
         GOTO1 HIGH                                                             
         B     CTAR0040                                                         
CTAR0020 EQU   *                                                                
         GOTO1 SEQ                                                              
CTAR0040 EQU   *                                                                
         CLC   KEY(19),KEYSAVE     SAME KEY THROUGH SOURCE REP?                 
         BNE   CTAR0080            NO  - NO 9E EXISTS                           
         CLC   5(4,R6),KEY+23      YES - SAME CONTRACT NUMBER?                  
         BE    CTAR0400            YES - 9E EXISTS: REJECT                      
         B     CTAR0020            NO  - GO BACK FOR NEXT KEY                   
CTAR0080 EQU   *                                                                
***>>>                                                                          
         MVC   TEMPTGRP,RSTAGRUP   SAVE TARGET GROUP                            
         MVI   TEMPTTYP,C'O'       SET STATION TYPE TO 'OTHER'                  
         LA    RF,RSTAELEM                                                      
CTAR0100 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    CTAR0140            YES - NO X'05' ELEMENT                       
         CLI   0(RF),5             END OF RECORD?                               
         BNE   CTAR0120            YES - NO X'05' ELEMENT                       
         MVI   TEMPTTYP,C'G'       SET STATION TYPE TO 'GRAPH'                  
         CLC   10(2,RF),=X'0406'   GRAPHNET?                                    
         BE    CTAR0140            YES                                          
         MVI   TEMPTTYP,C'A'       SET STATION TYPE TO 'ACE'                    
         B     CTAR0140            YES                                          
CTAR0120 EQU   *                                                                
         ZIC   RE,1(RF)            TAKE ELEMENT LENGTH                          
         AR    RF,RE               BUMP TO NEXT ELEMENT                         
         B     CTAR0100            GO BACK FOR NEXT                             
CTAR0140 EQU   *                                                                
***>>>                                                                          
         SR    R0,R0               SET CC = ZERO                                
         B     CTAR0420                                                         
CTAR0400 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
CTAR0420 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
***>>>>                                                                         
*   GENERIC ADDREC FOR CONTRACT, AND CREATION OF PASSIVE POINTERS               
*        TO BE CALLED REGARDLESS AT WHICH STAGE CONTRACT IS OUTPUT              
*                                                                               
CONCREAT NMOD1 0,*CCRE*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R4,AIO              SET A(CONTRACT RECORD)                       
         USING RCONREC,R4                                                       
*                                                                               
         MVC   RCONKGRP,TRGTGRP    INSERT TARGET GROUP                          
*                                                                               
***      LA    R1,RCONELEM                                                      
CCRE0050 EQU   *                                                                
***      CLI   0(R1),0             END OF RECORD?                               
***      BE    CCRE0200            YES - FINISHED                               
***      CLI   0(R1),3             ESTIMATE BUCKET?                             
***      BE    CCRE0100            YES - TEST DATE                              
***      CLI   0(R1),4             INVOICE BUCKET?                              
***      BNE   CCRE0150                                                         
CCRE0100 EQU   *                                                                
***      MVC   4(2,R1),MONDATE     INSERT MONDAY DATE AS ACTIVITY               
CCRE0150 EQU   *                                                                
**       ZIC   RF,1(R1)            BUMP TO NEXT ELEMENT                         
***      AR    R1,RF                                                            
***      B     CCRE0050            GO BACK FOR NEXT BUCKET                      
CCRE0200 EQU   *                                                                
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'17',RCONREC),0,0                
*                                  DELETE COMBO CONTROL ELT                     
         OC    NEWCOMBO,NEWCOMBO   ANY NEW COMBO ELT?                           
         BZ    CCRE0250            NO                                           
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,NEWCOMBO,0                 
*                                  INSERT NEW COMBO CONTROL ELEMENT             
CCRE0250 EQU   *                                                                
*                                                                               
                                                                                
         GOTO1 ADDREC,DMCB,RCONREC                                              
*                                                                               
*   FOLLOWING TWO INSTRUCTIONS SEEM TO BE MEANINGLESS.                          
*                                                                               
***      SP    WORK+5(5),=P'1'                                                  
***      MVO   WORK+15(5),WORK+5(5)                                             
         MVC   NEWADDR,KEY         SAVE CONTRACT DISK ADDRESS                   
*                                  PASSIVE PTR LOGIC                            
         L     R6,AIO3             OLD                                          
         LR    RE,R6                                                            
         XCEF  (RE),500                                                         
         MVC   0(27,R6),RCONREC    DO NOT ADD MASTER POINTER                    
*                                                                               
*                                  BUILD PASSIVE PTRS                           
*                                                                               
         LA    R8,500(R6)                                                       
         GOTO1 =A(PTRS),DMCB,(RC),(R8),0,RR=Y                                   
*                                                                               
*                                  ADD PTRS                                     
*                                                                               
         GOTO1 =A(ADDPTRS),DMCB,(RC),(R6),(R8),NEWADDR,RR=Y                     
*                                                                               
*                                                                               
*   TEST ONLY:  'NOPW/NO9E CONSIDERED AS SET!!                                  
*        REMOVE FOR FINAL TESTING                                               
*                                                                               
***>>>   B     CCRE0300            YES - NO 9E KEY TEST DONE                    
*                                                                               
         CLC   =C'NO9E',CTOOPT     'IGNORE 9E' OPTION SET?                      
         BE    CCRE0300            YES - NO 9E KEY GENERATED                    
         CLC   =C'NOPE',CTOOPT     'IGNORE 9E/NO PW' OPTION SET?                
         BE    CCRE0300            YES - NO 9E KEY GENERATED                    
         XC    KEY(27),KEY         CLEAR KEY:  LEAVE DISK ADDR                  
         MVI   KEY,X'9E'           ESTABLISH TAKEOVER/MOVE KEY                  
         MVC   KEY+12(2),TWAAGY    INSERT REP CODE                              
         MVC   KEY+14(5),RCONKSTA  INSERT STATION                               
         MVC   KEY+19(2),SRCEREP   INSERT SOURCE REP CODE                       
         GOTO1 DATCON,DMCB,(5,WORK),(2,KEY+21)                                  
*                                  INSERT TAKEOVER/MOVE DATE                    
         MVC   KEY+23(4),SRCECON   INSERT SOURCE CONTRACT NUMBER                
*                                                                               
         GOTO1 ADD                 ADD 9E KEY                                   
*                                                                               
         MVI   KEY,X'A2'           ESTABLISH TKO-BB HIST KEY                    
         MVI   KEY+1,X'02'                                                      
         XC    KEY+21(2),KEY+21    CLEAR OUT DATE                               
*                                                                               
         GOTO1 ADD                 ADD 2A02 KEY                                 
*                                                                               
         XC    KEY(27),KEY         CLEAR KEY:  LEAVE DISK ADDR                  
ADKEYD   USING RCONTTYP,KEY                                                     
         MVI   KEY,X'AD'           ESTABLISH DARE TAKEOVER KEY                  
         MVC   ADKEYD.RCONTREP,TWAAGY    INSERT REP CODE                        
         MVC   ADKEYD.RCONTSTA,RCONKSTA  INSERT STATION CALLS                   
         MVC   ADKEYD.RCONTOLD,SRCECON   INSERT SOURCE CON NUM                  
         MVC   ADKEYD.RCONTCON,RCONKCON  INSERT ORIGINAL CON NUM                
         DROP  ADKEYD                                                           
*                                                                               
         GOTO1 ADD                 ADD AD KEY                                   
*                                                                               
CCRE0300 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
****>>>>                                                                        
*                                                                               
*   RETRIEVE AND TABLE OFFICE TEAMS ALLOWED FOR THIS STATION                    
*                                                                               
OFFCTEAM NMOD1 0,*OFTM*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R3,OFFTEAMS         SET A(OFFICE/TEAM FIELD)                     
         XC    OFFTEAMS,OFFTEAMS   CLEAR THE FIELD                              
         L     R6,AIO                                                           
         USING RSTAOTEL,R6                                                      
         MVI   ELCODE,4            GET OFFICE/TEAM ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   OTEM0100                                                         
*                                                                               
OTEM0080 DS    0H                                                               
         MVC   0(2,R3),RSTAOTOF    INSERT OFFICE INTO TABLE                     
         MVC   2(2,R3),RSTAOTTM    INSERT TEAM   INTO TABLE                     
         OI    3(R3),X'40'         SET BINARY ZERO TO SPACE                     
         LA    R3,4(R3)            SET TO NEXT SLOT                             
         BAS   RE,NEXTEL           GO BACK FOR NEXT ELEMENT                     
         BE    OTEM0080                                                         
*                                                                               
OTEM0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   SETCTYPS:  SOURCE FILE CONTRACT TYPE RECORDS ARE READ.  IF                  
*        RCTYFPRA/X'10' BIT IS SET, RECORDS OF THIS CONTRACT                    
*        TYPE ARE TO BE SKIPPED.  TABLE THIS TYPE.                              
*                                                                               
SETCTYPS NMOD1 0,*CTYP*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XC    CTYPTABL,CTYPTABL   CLEAR TABLE                                  
         LA    R2,CTYPTABL         SET A(TABLE)                                 
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'32'           INSERT RECORD TYPE                           
         MVC   KEY+24(2),SRCEREP   INSERT SOURCE REP CODE                       
         GOTO1 HIGH                                                             
         B     SECT0040                                                         
SECT0020 EQU   *                                                                
         GOTO1 SEQ                                                              
SECT0040 EQU   *                                                                
         CLC   KEY(26),KEYSAVE     SAME RECORD TYPE/REP?                        
         BNE   SECT0160            NO  - FINISHED                               
         GOTO1 GETREC              RETRIEVE STATION RECORD                      
         L     R5,AIO                                                           
         USING RCTYREC,R5                                                       
*                                                                               
         CLI   RCTYFCDE,X'10'      NEW FORMAT RECORD?                           
         BNE   SECT0020            NO  - GO BACK FOR NEXT                       
         TM    RCTYFPRA,X'10'      'DON'T TAKE OVER CTYP' SET?                  
         BNO   SECT0020            NO  - GO BACK FOR NEXT                       
         MVC   0(1,R2),RCTYKCTY    YES - INSERT TYPE INTO TABLE                 
         LA    R2,1(R2)            BUMP TO NEXT ENTRY                           
         B     SECT0020            GO BACK FOR NEXT                             
SECT0160 EQU   *                                                                
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE REP                                                                  
* INPUT  - R2 POINTS TO PERIOD FIELD HEADER                                     
* OUTPUT - SRCEUTL CONTAINS ALTERNATE SYSTEM UTL#.  THIS IS IN TWA              
*          FOR SUBSEQUENT ENTERS.                                               
***********************************************************************         
VALIREP  NMOD1 0,*VREP*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'           FIND CONTROL FILE ID RECORD                  
         LA    RF,CTOREPH          A(SOURCE REP)                                
         ZIC   RE,5(RF)            L(SOURCE REP INPUT)                          
         BCTR  RE,0                -1 FOR EX                                    
         EX    RE,VREP0800         MOVE BY LENGTH                               
         OC    WORK+15(10),SPACES  SET REMAINDER TO SPACES                      
         EX    RE,VREP0810         MOVE BY LENGTH: SAVE CODE                    
         OC    SRCESIGN(5),SPACES  SET REMAINDER TO SPACES                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AIO                      
         CLI   8(R1),0             FOUND?                                       
         BNE   VREP0080            NOT FOUND - SHOW MESSAGE                     
         L     R1,AIO                                                           
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BNE   VREP0080            NOT FOUND - SHOW MESSAGE                     
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
VREP0020 EQU   *                                                                
         CLI   0(R1),X'06'         AGENCY ID ELEMENT?                           
         BNE   VREP0030            NO                                           
         MVC   SRCEREP,2(R1)       YES - SAVE 2-CHAR REP ID                     
         B     VREP0040            BUMP TO NEXT ELEMENT                         
VREP0030 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   VREP0040            NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    VREP0060            YES                                          
VREP0040 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD                                
         BNE   VREP0020            NO                                           
         B     VREP0080            NO X'21' - ERROR MESSAGE                     
VREP0060 EQU   *                                                                
         MVC   SRCEUTL,3(R1)       SAVE SOURCE UTL NUMBER                       
         GOTO1 =A(GETREPNM),DMCB,(RC),RR=Y                                      
*                                  RETRIEVE REP'S SHORT NAME                    
         SR    R0,R0               SET CC = ZERO                                
         B     VREP0100                                                         
VREP0080 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO:  ERROR                      
VREP0100 EQU   *                                                                
         XIT1                                                                   
VREP0800 MVC   WORK+15(0),CTOREP   LOAD SOURCE REP BY LENGTH                    
VREP0810 MVC   SRCESIGN(0),CTOREP  SAVE SOURCE REP BY LENGTH                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   GETREPNM:  READ REP RECORD FROM ORIGINAL FILE, SAVE SHORT REP               
*        NAME FOR CONTRACT COMMENT.                                             
*                                                                               
GETREPNM NMOD1 0,*RPNM*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'01'           INSERT REP KEY TYPE                          
         MVC   KEY+25(2),SRCEREP   INSERT SOURCE REP CODE                       
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
         GOTO1 HIGH                ACCESS  KEY                                  
         CLC   KEY(22),KEYSAVE     REP RECORD KEY FOUND?                        
         BE    *+6                 YES -                                        
         DC    H'0'                NO  - MUST BE ON FILE                        
         MVC   SAVEAIO,AIO         SAVE ORIGINAL IO AREA                        
         MVC   AIO,AIO3                                                         
         L     R4,AIO3                                                          
         USING RREPREC,R4                                                       
*                                                                               
         GOTO1 GETREC,DMCB,RREPREC                                              
*                                  RETRIEVE ORIGINAL BUY RECORD                 
         MVC   AIO,SAVEAIO         RESET ORIGINAL IO AREA                       
*                                                                               
         MVC   SRCENAME,RREPSHRT   SAVE SHORT REP NAME                          
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
         XIT1                                                                   
*                                                                               
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   TAKEBUYS:  READ ALL BUYS FROM ORIGINAL ORDER, MOVE TO NEW FILE              
*        WITH NEW CONTRACT NUMBER.                                              
*        IF CONSAVED = Y, BUYS MUST BE ADJUSTED TO REFLECT REVISED              
*        FLIGHT START DATE, AND BUCKETS WITHIN ORDER MUST BE UPDATED            
*        LIKEWISE.  THIS RESULTS IN A LOSS OF PACING FOR ESTIMATES,             
*        AND ANY INVOICE BUCKETS (WHICH WOULD BE MISMATCHED FOR                 
*        PACING ANYWAY.)                                                        
*                                                                               
TAKEBUYS NMOD1 0,*TKBY*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
***>>>                                                                          
         L     R5,4(R1)            RESET A(CONTRACT RECORD)                     
         USING RCONREC,R5                                                       
         MVI   ERLYLATE,0          SET CUT FLAG:  SEE NOTE                      
*                                                                               
         CLC   OCONDATE(3),EFDTBSUN                                             
*                                  FLIGHT START POST?                           
         BH    TKBY0004                                                         
         CLC   OCONDATE+3(3),HISTDATB                                           
*                                  FLIGHT END PRE?                              
         BNL   TKBY0008                                                         
TKBY0004 EQU   *                                                                
         DC    H'0'                SHOULDN'T HAPPEN:  ALREADY ACCEPTED          
TKBY0008 EQU   *                                                                
         CLC   OCONDATE+3(3),EFDTBSUN                                           
*                                  FLIGHT END POST?                             
         BL    TKBY0012            NO  - NO CUT AT END                          
         OI    ERLYLATE,CUTLATE    YES - CUT AT END                             
TKBY0012 EQU   *                                                                
         CLC   OCONDATE(3),HISTDATB                                             
*                                  FLIGHT DATE PRE?                             
         BNL   TKBY0016            NO                                           
         OI    ERLYLATE,CUTEARLY   CUT EARLY                                    
*                                                                               
TKBY0016 EQU   *                                                                
***>>>   CLI   ERLYLATE,0          ANY CUTBACK IN THIS ORDER?                   
***>>>   BE    TKBY0020            NO  - JUST TAKE THE ORDER                    
         GOTO1 =A(SAVEBUKS),DMCB,(RC),RCONREC,RR=Y                              
*                                  YES - SAVE ORIGINAL BUCKETS                  
TKBY0020 EQU   *                                                                
         NI    DMINBTS,X'FF'-X'08' RESET                                        
*                                  DON'T READ DELETED BUYS                      
*                                  COMPLEMENT/REVERSE CONTRACT NUMBER           
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),SRCECON(4)                                            
                                                                                
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   WORK+32(4),WORK+15                                               
*                                  REVERSE THE COMPLIMENT                       
         PACK  WORK+32(1),WORK+18(1)                                            
         PACK  WORK+33(1),WORK+17(1)                                            
         PACK  WORK+34(1),WORK+16(1)                                            
         PACK  WORK+35(1),WORK+15(1)                                            
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'0B'           INSERT BUY KEY TYPE                          
         MVC   KEY+16(2),SRCEREP   INSERT SOURCE REP CODE                       
         MVC   KEY+18(4),WORK+32   INSERT CON#, COMP/REVERSED                   
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
         GOTO1 HIGH                ACCESS FIRST KEY                             
         B     TKBY0080                                                         
TKBY0040 EQU   *                                                                
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
         MVC   KEY(27),KEYSAVE     RESTART SOURCE FILE                          
         GOTO1 HIGH                REPOSITION TO LAST KEY ACCESSED              
*                                                                               
         GOTO1 SEQ                 ACCESS NEXT KEY                              
TKBY0080 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME REC TYPE/REP/CON#?                      
         BNE   TKBY0320            NO  - FINISHED WITH CONTRACT                 
         MVC   KEYSAVE,KEY         SAVE KEY ACCESSED FOR RESTART                
         MVC   SAVEAIO,AIO         SAVE ORIGINAL IO AREA                        
         MVC   AIO,AIO3                                                         
         L     R4,AIO3                                                          
         USING RBUYREC,R4                                                       
*                                                                               
         GOTO1 GETREC,DMCB,RBUYREC                                              
*                                  RETRIEVE ORIGINAL BUY RECORD                 
         MVC   AIO,SAVEAIO         RESET ORIGINAL IO AREA                       
*                                                                               
*   CANCELLED BUYS MAY BE RETAINED ON THE FILE.  THESE WILL NOT BE              
*        TAKEN OVER.  IF IT IS DESIRED TO TAKE THEM OVER, THESE                 
*        TESTS SHOULD BE RELOCATED TO THE REGENBUC CODE, AND                    
*        CAUSE IT TO BE BYPASSED.                                               
*                                                                               
         CLI   RBUYCHGI,C'C'       CANCELLED BUY?                               
         BE    TKBY0040            YES - DON'T BRING THIS OVER                  
         CLI   RBUYCHGI+1,C'C'     CANCELLED BUY?                               
         BE    TKBY0040            YES - DON'T BRING THIS OVER                  
*                                                                               
*                                  COMPLEMENT/REVERSE NEW CON NUMBER            
*                                                                               
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),RCONKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   WORK+32(4),WORK+15                                               
*                                  REVERSE THE COMPLIMENT                       
         PACK  WORK+32(1),WORK+18(1)                                            
         PACK  WORK+33(1),WORK+17(1)                                            
         PACK  WORK+34(1),WORK+16(1)                                            
         PACK  WORK+35(1),WORK+15(1)                                            
*                                                                               
         MVC   RBUYKCON,WORK+32    INSERT NEW CON# INTO BUY                     
         MVC   RBUYKREP,TARGREP    INSERT NEW REP CODE                          
*                                                                               
*                                  SWITCH BACK TO ORIGINAL FILE TO              
*                                     ADD THE NEW BUY RECORD                    
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
         MVC   AIO,AIO3                                                         
         L     R4,AIO3                                                          
         CLI   CONSAVED,C'Y'       CONTRACT SAVED?  (FLIGHTS CHANGED)           
         BNE   TKBY0120            NO                                           
         BAS   RE,BUYBACK          YES - CUT BACK BUYS AND REGENERATE           
*                                     ESTIMATE BUCKETS                          
TKBY0120 EQU   *                                                                
         SR    R0,R0                                                            
         LA    R6,RBUYELEM                                                      
TKBY0160 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD?                               
         BE    TKBY0240            YES                                          
         CLI   0(R6),X'56'         MAKEGOOD ELEMENT?                            
         BNE   TKBY0180            NO                                           
         USING RBYMGSEL,R6                                                      
*                                                                               
         CLC   RBYMGSDT,HISTDATB   MISSED DATE < EARLY DATE                     
         BL    TKBY0170            YES - DOESN'T BELONG                         
         CLC   RBYMGSDT,EFDTBSUN   MISSED DATE > TKO DATE?                      
         BNH   TKBY0200            NO  - KEEP ELEMENT                           
*                                                                               
         DROP  R6                                                               
*                                                                               
TKBY0170 EQU   *                                                                
         MVI   0(R6),X'FF'         YES - SET ELEMENT FOR DELETE                 
         LA    R0,1                SET ELEMENTS FOUND FOR DELETE FLAG           
         B     TKBY0200            BUMP TO NEXT ELEMENT                         
*                                                                               
TKBY0180 EQU   *                                                                
         CLI   0(R6),X'66'         MAKEGOOD OFFER ELEMENT?                      
         BNE   TKBY0200            NO                                           
         USING RBMGMSEL,R6                                                      
*                                                                               
         CLC   RBMGMSDT,HISTDATB   MISSED DATE < EARLY DATE?                    
         BL    TKBY0190            YES - DOESN'T BELONG                         
         CLC   RBMGMSDT,EFDTBSUN   MISSED DATE > TKO DATE?                      
         BNH   TKBY0200            NO  - KEEP ELEMENT                           
*                                                                               
         DROP  R6                                                               
*                                                                               
TKBY0190 EQU   *                                                                
         MVI   0(R6),X'FF'         YES - SET ELEMENT FOR DELETE                 
         LA    R0,1                SET ELEMENTS FOUND FOR DELETE FLAG           
TKBY0200 EQU   *                                                                
         ZIC   RF,1(R6)                                                         
         AR    R6,RF               BUMP TO NEXT ELEMENT                         
         B     TKBY0160            GO BACK FOR NEXT ELEMENT                     
TKBY0240 EQU   *                                                                
         LTR   R0,R0               ANYTHING TO DELETE?                          
         BZ    TKBY0280                                                         
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'FF',RBUYREC),0,0                
*                                  DELETE MKG OFFR ELTS PRIOR TO                
*                                     TKO DATE                                  
TKBY0280 EQU   *                                                                
         CLI   EFFELTS,C'Y'        EFFECTIVE DATE ELTS IN RECORD?               
         BE    TKBY0300            YES - OUTPUT THE BUY                         
         CLI   CONSAVED,C'Y'       NO  - CONTRACT SAVED?  (FLTS CHGED)          
         BNE   TKBY0300            NO  - OUTPUT VIRGIN BUY                      
*                                                                               
*   TEST EFFELTS                                                                
***>>>   MVI   DIE,X'00'                                                        
         B     TKBY0310                                                         
*   TEST EFFELTS END                                                            
*                                                                               
TKBY0300 EQU   *                                                                
         GOTO1 ADDREC,DMCB,RBUYREC                                              
*                                  ADD BUY RECORD FOR NEW CONTRACT              
TKBY0310 EQU   *                                                                
         MVC   AIO,SAVEAIO         RESET ORIGINAL IO AREA                       
*                                                                               
         B     TKBY0040            GO BACK FOR NEXT BUY                         
*                                                                               
***>>>                                                                          
TKBY0320 EQU   *                                                                
***>>>   CLI   ERLYLATE,0          ANY CUTBACK IN THIS ORDER?                   
***>>>   BE    TKBY0330            NO  - DON'T PLAY W/BUCKS ANY MORE            
         GOTO1 =A(RESETBUK),DMCB,(RC),RCONREC,RR=Y                              
*                                  YES - RESET ORIGINAL BUCKETS                 
TKBY0330 EQU   *                                                                
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
         DROP  R4,R5                                                            
         XIT1                                                                   
         EJECT                                                                  
*INSRT*>                                                                        
*                                                                               
*   BUYBACK:  ADJUSTS BUY FLIGHT DATES, BASED ON TAKEOVER DATE,                 
*        AND REGENERATES ESTIMATE BUCKETS IN THE CONTRACT RECORD                
*                                                                               
BUYBACK  NTR1                                                                   
         MVI   EFFELTS,C'N'        SET 'NO EFFECTIVE DATE ELEMENTS'             
         L     R4,AIO3                                                          
         USING RBUYREC,R4                                                       
         XC    SVTOTSPT,SVTOTSPT   CLEAR ACCUMULATORS                           
         XC    SVTOTWKS,SVTOTWKS                                                
*                                                                               
****>>>  GOTO1 DATCON,DMCB,(5,WORK),(3,RBUYCREA)                                
*                                  SET BUY CREATE TO DATE OF RUN                
***      MVC   RBUYCREA,EFDTBIN    SET BUY CREATE DATE TO TKO                   
         OC    RBUYCHGD,RBUYCHGD   HAS BUY BEEN CHANGED?                        
         BZ    BUYB0020            NO  - LEAVE AS ZERO                          
****>>>  MVC   RBUYCHGD,RBUYCREA   YES - SET BUY CHANGE DATE                    
BUYB0020 EQU   *                                                                
*                                                                               
*   PROCESS EACH 03 (EFFECTIVE DATE) ELEMENT:                                   
*        1.  IF START NOT EARLIER THAN TKO DATE, DROP ELEMENT                   
*        2.  IF END BEFORE HIST TKO DATE, DROP ELEMENT                          
*        3.  IF START LATER THAN HIST TKO DATE, USE AS-IS                       
*        4.  IF START EARLIER, USE TKO DATE, ADJUST ELEMENT                     
*        5.  IF END EARLIER THAN EFF TKO DATE, USE AS-IS                        
*        6.  IF END LATER, USE EFF TKO DATE, ADJUST ELEMENT                     
*        7.  SAVE ALL MONTHS' BUCKETS NOT CUT, REINSERT LATER                   
*        8.  RECALCULATE TOTAL BUY FIGURES FROM 03 ELEMENT DETAILS              
*                                                                               
         LA    R3,RBUYELEM                                                      
BUYB0040 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    BUYB0200            YES                                          
         CLI   0(R3),3             EFFECTIVE DATE ELEMENT?                      
         BNE   BUYB0180            NO  - BUMP TO NEXT ELEMENT                   
         USING RBUYDTEL,R3                                                      
*                                                                               
         CLC   RBUYDTED,HISTDATB   ELEMENT END DATE VS EARLY DATE               
         BNH   BUYB0160            ELT END DATE PRIOR EARLY                     
*                                     DROP THE ELEMENT                          
*                                  EXAMPLE:  HIST T/O DATE = JUL1/97            
*                                            ELT END       = JUN1/97            
*                                                                               
         CLC   RBUYDTST,EFDTBSUN   ELEMENT START DATE VS EFF T/O DATE           
         BH    BUYB0160            ELT START DATE AFTER TKO DATE                
*                                     DROP THE ELEMENT                          
*                                  EXAMPLE:  ELT DATE = JUL1/97                 
*                                            EFDTBSUN = JAN1/97                 
*                                                                               
         CLC   RBUYDTST,HISTDATB   ELEMENT START DATE VS EARLY DATE             
         BNL   BUYB0050            ELT START DATE AFTER HIST TKO DATE           
*                                     USE EARLIER BUCKETS AS IS                 
*                                  EXAMPLE:  ELT DATE = JUL1/97                 
*                                            HISTDATE = JUL1/97                 
         BAS   RE,CUTHIST          CUT START DATE TO HIST DATE                  
         BNZ   BUYB0160            SKIP THIS BUYLINE                            
*                                                                               
BUYB0050 EQU   *                                                                
*                                                                               
         CLC   RBUYDTED,EFDTBSUN   ELEMENT END DATE VS EFF T/O DATE             
         BNH   BUYB0140            ELT START DATE <= EFF TKO DATE               
*                                     USE AS IS - REGEN BUCKETS                 
*                                  EXAMPLE:  ELT END DATE = JUL1/97             
*                                            BEFF DATE    = SEP1/97             
*                                            HISTDATE = JUL1/97                 
         BAS   RE,CUTEFF           CUT END DATE TO EFF DATE                     
         BNZ   BUYB0160            SKIP THIS BUYLINE                            
*                                                                               
*                                                                               
BUYB0140 EQU   *                                                                
         ZIC   RE,RBUYDTWK         GET NUMBER OF WEEKS                          
         L     RF,SVTOTWKS         CALCULATE TOTAL NUMBER OF WEEKS              
         AR    RF,RE                                                            
         ST    RF,SVTOTWKS         SAVE TOTAL NUMBER OF WEEKS                   
         MVC   HALF,RBUYDTNW       GET SPOTS/WEEK                               
         MH    RE,HALF             NUM WKS * SPTS/WK = TOTAL SPOTS              
         L     RF,SVTOTSPT         CALCULATE TOTAL NUMBER SPOTS                 
         AR    RF,RE                                                            
         ST    RF,SVTOTSPT         SAVE TOTAL NUMBER SPOTS                      
         MVI   EFFELTS,C'Y'        SET 'EFFECTIVE DATE ELEMENTS'                
         B     BUYB0180                                                         
*                                                                               
         DROP  R3                                                               
*                                                                               
BUYB0160 EQU   *                                                                
         MVI   0(R3),X'FF'         SET ELEMENT FOR DELETION                     
BUYB0180 EQU   *                                                                
         ZIC   RF,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RF                                                            
         B     BUYB0040            GO BACK FOR NEXT ELEMENT                     
BUYB0200 EQU   *                                                                
*                                                                               
*   PROCESS EACH 05 (BUY MG REF ELT)/56 (MG SPLITOUT ELT)                       
*        1.  IF DATE BEFORE TKO DATE, DROP ELEMENT                              
*                                                                               
         LA    R3,RBUYELEM                                                      
         SR    RF,RF               CLEAR REG FOR INDICATOR                      
         MVI   MGBITFLG,0          CLEAR FLAG BITS                              
         XC    ELEM,ELEM           CLEAR ELEMENT BUILD AREA                     
BUYB0220 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    BUYB0300            YES                                          
         CLI   0(R3),4             BUY COMMENT ELEMENT?                         
         BNE   BUYB0230            NO  -                                        
         LTR   RF,RF               YES - PRIOR COMMENT FOUND?                   
         BNZ   BUYB0230            YES - DON'T SAVE ADDRESS                     
         LR    RF,R3               NO  - SAVE A(COMMENT RECORD)                 
BUYB0230 EQU   *                                                                
         CLI   0(R3),5             BUY MG REF ELT?                              
         BE    BUYB0240            YES - PROCESS IT                             
         CLI   0(R3),X'56'         BUY MG SPLITOUT ELT?                         
         BE    BUYB0260            YES - PROCESS IT                             
         CLI   0(R3),X'07'         CREDIT XREF ELEMENT?                         
         BE    BUYB0232            YES - PROCESS IT                             
         B     BUYB0280            NO  - BUMP TO NEXT ELT                       
BUYB0232 EQU   *                                                                
         USING RBUYCREL,R3                                                      
*                                                                               
         CLC   RBUYCRDT,HISTDATB   CREDIT MISSED DATE < EARLY DATE?             
         BL    BUYB0234            YES - DROP ELEMENT FROM RECORD               
         CLC   RBUYCRDT,EFDTBSUN   NO  - CREDIT MISSED DATE < TKO DATE?         
         BNH   BUYB0280            NO  - KEEP ELEMENT                           
*                                                                               
         DROP  R3                                                               
*                                                                               
BUYB0234 EQU   *                                                                
         MVI   0(R3),X'FF'         MARK ELEMENT FOR DELETION                    
         B     BUYB0280                                                         
BUYB0240 EQU   *                                                                
         USING RBUYMGEL,R3                                                      
*                                                                               
         CLC   =C'CR=',2(RF)       COMMENT INDICATES CREDIT?                    
         BE    BUYB0242            YES - CHECK CREDIT DATE IN 05                
         CLC   =C'CR>',2(RF)       NO  - COMMENT = CREDIT   MODIFIED?           
         BE    BUYB0242            YES - CHECK CREDIT DATE IN 05                
*                                  NO  - MAKEGOOD                               
         TM    MGBITFLG,X'40'      HAS ONE X'05' BEEN DROPPED                   
*                                     FOR THIS BUYLINE?                         
         BO    BUYB0245            YES - DROP THEM ALL                          
*                                                                               
BUYB0242 EQU   *                                                                
         CLC   RBUYMGD1,HISTDATB   MG MISSED DATE < EARLY DATE?                 
         BL    BUYB0244            YES - DROP ELEMENT FROM RECORD               
         CLC   RBUYMGD1,EFDTBSUN   NO  - MG MISSED DATE < EFF DATE?             
         BNH   BUYB0280            YES - LEAVE ELT ALONE                        
*                                                                               
         DROP  R3                                                               
*                                                                               
BUYB0244 EQU   *                                                                
         OI    MGBITFLG,X'40'      INDICATE AT LEAST ONE X'05' ELT              
*                                     HAS BEEN DROPPED FOR BUY                  
BUYB0245 EQU   *                                                                
         MVI   0(R3),X'FF'         YES - MARK ELEMENT FOR DELETE                
         CLC   =C'MG=',2(RF)       COMMENT INDICATES MAKEGOOD?                  
         BE    BUYB0250            YES                                          
         CLC   =C'CR=',2(RF)       COMMENT INDICATES CREDIT?                    
         BE    BUYB0250            YES                                          
         CLC   =C'MG>',2(RF)       NO  - COMMENT = MAKEGOOD MODIFIED?           
         BE    BUYB0280            YES                                          
         CLC   =C'CR>',2(RF)       NO  - COMMENT = CREDIT   MODIFIED?           
         BE    BUYB0280            YES                                          
***      DC    H'0'                NO  - WHAT IS HERE???                        
*   NEW M/G FORMATS DON'T USE MG=/CR=.  MUST BE PROCESSED ON A FLAG             
*        THERE ALSO MIGHT NOT BE A COMMENT, SO RF MAY BE ZERO                   
*                                                                               
         MVC   RBUYKMLN,RBUYKLIN   SET MASTER LIN# = MAIN LIN#                  
         LTR   RF,RF               ANY COMMENT ELEMENT?                         
         BNZ   BUYB0247            YES                                          
         MVC   ELEM(2),=X'0402'    NO  - DUMMY UP ELEMENT                       
         LA    RE,2                SET LENGTH OF DUMMY ELEMENT                  
         B     BUYB0249                                                         
BUYB0247 EQU   *                                                                
         ZIC   RE,1(RF)            GET LENGTH OF ELEMENT                        
         BCTR  RE,0                BACK UP 1 FOR EX                             
         EX    RE,BUYB0255         MOVE BY LENGTH                               
         MVI   4(RF),C'>'          SET MODIFIED MAKEGOOD/CREDIT FLAG            
         MVI   0(RF),X'FF'         SET ELEMENT FOR DELETE                       
         LA    RE,1(RE)            RESET ORIGINAL LENGTH                        
BUYB0249 EQU   *                                                                
         LA    R1,ELEM             SET A(NEW ELEMENT)                           
         AR    R1,RE               SET TO PAST LAST CHAR                        
         MVC   0(12,R1),=C'-MG TAKEOVER'                                        
         ZIC   RE,ELEM+1                                                        
         LA    RE,12(RE)           ADD 12 TO ELEMENT LENGTH                     
         STC   RE,ELEM+1           PUT NEW LENGTH BACK                          
         B     BUYB0258                                                         
BUYB0250 EQU   *                                                                
         MVC   RBUYKMLN,RBUYKLIN   SET MASTER LIN# = MAIN LIN#                  
         MVI   4(RF),C'>'          SET MODIFIED MAKEGOOD/CREDIT FLAG            
         ZIC   RE,1(RF)            GET LENGTH OF ELEMENT                        
         BCTR  RE,0                BACK UP 1 FOR EX                             
         EX    RE,BUYB0255         MOVE BY LENGTH                               
         MVI   0(RF),X'FF'         SET ELEMENT FOR DELETE                       
         LA    RE,1(RE)            RESET ORIGINAL LENGTH                        
         LA    R1,ELEM             SET A(NEW ELEMENT)                           
         AR    R1,RE               SET TO PAST LAST CHAR                        
         MVC   0(9,R1),=C'-TAKEOVER'                                            
         ZIC   RE,ELEM+1                                                        
         LA    RE,9(RE)            ADD 9 TO ELEMENT LENGTH                      
         STC   RE,ELEM+1           PUT NEW LENGTH BACK                          
         B     BUYB0258                                                         
BUYB0255 EQU   *                                                                
         MVC   ELEM(0),0(RF)       MOVE COMMENT BY LENGTH                       
BUYB0258 EQU   *                                                                
         B     BUYB0280            BUMP TO NEXT ELEMENT                         
BUYB0260 EQU   *                                                                
         USING RBYMGSEL,R3                                                      
*                                                                               
         TM    MGBITFLG,X'40'      HAS ONE X'05' BEEN DROPPED                   
*                                     FOR THIS BUYLINE?                         
         BO    BUYB0270            YES - DROP THEM ALL                          
*                                                                               
         CLC   RBYMGSDT,HISTDATB   MG MISSED DATE < EARLY DATE?                 
         BL    BUYB0265            YES - DROP ELEMENT FROM RECORD               
         CLC   RBYMGSDT,EFDTBSUN   NO  - CREDIT MISSED DATE < TKO DATE?         
         BNH   BUYB0280            NO  - KEEP ELEMENT                           
*                                                                               
         DROP  R3                                                               
*                                                                               
BUYB0265 EQU   *                                                                
         OI    MGBITFLG,X'80'      INDICATE AT LEAST ONE X'56' ELT              
*                                     HAS BEEN DROPPED FOR BUY                  
BUYB0270 EQU   *                                                                
         MVI   0(R3),X'FF'         YES - MARK ELEMENT FOR DELETE                
BUYB0280 EQU   *                                                                
         ZIC   RE,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RE                                                            
         B     BUYB0220            GO BACK FOR NEXT ELEMENT                     
BUYB0300 EQU   *                                                                
         OC    ELEM,ELEM           ANY NEW ELEMENT TO ADD?                      
         BZ    BUYB0320            NO  - SKIP ADDELEM                           
         GOTO1 ADDELEM                                                          
*                                  INSERT NEW ELEMENT INTO RECORD               
BUYB0320 EQU   *                                                                
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'FF',RBUYREC),0,0                
*                                  DELETE EFF DATE ELTS PRIOR TO                
*                                     TKO DATE                                  
         SR    RE,RE                                                            
         L     RF,SVTOTSPT         LOAD TOTAL # SPOTS                           
         M     RE,SVTOTWKS         TOT SPOTS * # WEEKS =                        
         STCM  RF,15,RBUYTCOS         TOTAL COST OF BUY                         
         MVC   RBUYTSPT,SVTOTSPT   LOAD TOTAL NUMBER OF SPOTS                   
         MVC   RBUYTWKS,SVTOTWKS   LOAD TOTAL NUMBER OF WEEKS                   
         GOTO1 BUCKUP,DMCB,RBUYREC                                              
         XIT1                                                                   
*                                                                               
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
**CUTEFF                                                                        
CUTEFF   NTR1                                                                   
*                                                                               
         USING RBUYDTEL,R3                                                      
*                                                                               
*   TKO SUNDAY DATE IS ALWAYS A SUNDAY.  CHECK BUY START DATE.  IF NOT          
*        SUNDAY, ADJUST EFDTBSUN INSERT INTO BUY TO PROVIDE                     
*        OUT-OF-WEEK ROTATOR SUPPORT.                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,RBUYDTED),(0,WORK)                                
*                                  ORIGINAL BUY END   DATE -> EBCDIC            
         GOTO1 GETDAY,DMCB,WORK,DUB                                             
         CLI   DMCB,7              ORIGINAL START = SUNDAY?                     
         BE    CUTE0080            YES - USE TKO DATE AS IS                     
*                                  NO  - OFFSET TO ORIG BUY START DAY           
         ZIC   RE,DMCB             GET DAY OF WEEK NUMBER                       
         SH    RE,=H'7'            SUBTRACT 7: MAKE NEGATIVE                    
         ST    RE,DMCB+8           SET ADDAY ADJUST VALUE                       
         GOTO1 DATCON,DMCB,(3,EFDTBSUN),(0,WORK)                                
*                                  CONVERT SUNDAY TKO DATE TO EBCDIC            
         GOTO1 ADDAY,DMCB,WORK,WORK,,                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,RBUYDTED)                                
*                                  CONVERT OOWR END TO BINARY                   
         CLC   RBUYDTED,RBUYDTST                                                
         BL    CUTE0160            END BEFORE START:  DROP BUYLINE              
*                                     OOWR IN SINGLE WEEK:  SETS                
*                                     START AFTER END DATE                      
         B     CUTE0100                                                         
CUTE0080 EQU   *                                                                
         MVC   RBUYDTED,EFDTBSUN   RESET EFF END   TO TKO DATE                  
CUTE0100 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,WORK)                                
*                                  EBCDIC START DATE                            
         GOTO1 DATCON,DMCB,(3,RBUYDTED),(0,WORK+6)                              
*                                  EBCDIC END   DATE                            
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
*                                  CALCULATE DATE RANGE FIGURES                 
         ZICM  RF,DMCB+12,2        GET # WEEKS (DAYS/7)                         
         ZICM  RE,DMCB+10,2        CHECK REMAINDER DAYS/7                       
         LTR   RE,RE                                                            
         BZ    CUTE0120            NO REMAINDER                                 
         LA    RF,1(RF)            REMAINDER:  ADD 1 TO NUM WEEKS               
CUTE0120 EQU   *                                                                
         STC   RF,RBUYDTWK         RESET NUMBER OF WEEKS                        
         TM    RBUYDTIN,X'40'      ALTERNATE WEEK ELEMENT?                      
         BNO   CUTE0140            NO                                           
         TM    RBUYDTWK,X'01'      ODD NUMBER OF WEEKS?                         
*                                     (LOW-ORDER BIT SET?)                      
         BO    CUTE0130            YES - JUST RECALC # WEEKS                    
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,WORK)                                
*                                  CONVERT BUY START DATE TO EBCDIC             
         GOTO1 ADDAY,DMCB,WORK,WORK,7                                           
*                                  BUMP TO NEXT WEEK                            
         GOTO1 DATCON,DMCB,(0,WORK),(3,RBUYDTST)                                
*                                  CONVERT NEW START TO BINARY                  
         ZIC   RF,RBUYDTWK         DECREASE WEEKS BY 1                          
         BCTR  RF,0                                                             
         STC   RF,RBUYDTWK                                                      
CUTE0130 EQU   *                   RECALCULATE NUMBER OF WEEKS                  
         ZIC   RF,RBUYDTWK         EXTRACT NUMBER OF WEEKS                      
         LA    RF,1(RF)            MAKE NUMBER OF WEEKS EVEN                    
         SRL   RF,1                DIVIDE NUMBER OF WEEKS BY 2                  
         STC   RF,RBUYDTWK         REPLACE NUMBER OF WEEKS                      
CUTE0140 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         B     CUTE0180                                                         
CUTE0160 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
CUTE0180 EQU   *                                                                
         XIT1                                                                   
         DROP  R3                                                               
**CUTEFF                                                                        
         EJECT                                                                  
**CUTHIST                                                                       
CUTHIST  NTR1                                                                   
*                                                                               
         USING RBUYDTEL,R3                                                      
*                                                                               
*   HISTBB DATE IS ALWAYS A MONDAY.  CHECK BUY START DATE.  IF NOT              
*        MONDAY, ADJUST HISTDATB INSERT INTO BUY TO PROVIDE                     
*        OUT-OF-WEEK ROTATOR SUPPORT.                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,WORK)                                
*                                  ORIGINAL BUY START DATE -> EBCDIC            
         GOTO1 GETDAY,DMCB,WORK,DUB                                             
         CLI   DMCB,1              ORIGINAL START = MONDAY?                     
         BE    CUTH0080            YES - USE TKO DATE AS IS                     
*                                  NO  - OFFSET TO ORIG BUY START DAY           
         ZIC   RE,DMCB             GET DAY OF WEEK NUMBER                       
         BCTR  RE,0                SUBTRACT 1 DAY                               
         ST    RE,DMCB+8           SET ADDAY ADJUST VALUE                       
         GOTO1 DATCON,DMCB,(3,HISTDATB),(0,WORK)                                
*                                  CONVERT TKO DATE TO EBCDIC                   
         GOTO1 ADDAY,DMCB,WORK,WORK,,                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,RBUYDTST)                                
*                                  CONVERT OOWR START TO BINARY                 
         CLC   RBUYDTED,RBUYDTST                                                
         BL    CUTH0160            END BEFORE START:  DROP BUYLINE              
*                                     OOWR IN SINGLE WEEK:  SETS                
*                                     START AFTER END DATE                      
         B     CUTH0100                                                         
CUTH0080 EQU   *                                                                
         MVC   RBUYDTST,HISTDATB   RESET EFF START TO TKO DATE                  
CUTH0100 EQU   *                                                                
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,WORK)                                
*                                  EBCDIC START DATE                            
         GOTO1 DATCON,DMCB,(3,RBUYDTED),(0,WORK+6)                              
*                                  EBCDIC END   DATE                            
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
*                                  CALCULATE DATE RANGE FIGURES                 
         ZICM  RF,DMCB+12,2        GET # WEEKS (DAYS/7)                         
         ZICM  RE,DMCB+10,2        CHECK REMAINDER DAYS/7                       
         LTR   RE,RE                                                            
         BZ    CUTH0120            NO REMAINDER                                 
         LA    RF,1(RF)            REMAINDER:  ADD 1 TO NUM WEEKS               
CUTH0120 EQU   *                                                                
         STC   RF,RBUYDTWK         RESET NUMBER OF WEEKS                        
         TM    RBUYDTIN,X'40'      ALTERNATE WEEK ELEMENT?                      
         BNO   CUTH0140            NO                                           
         TM    RBUYDTWK,X'01'      ODD NUMBER OF WEEKS?                         
*                                     (LOW-ORDER BIT SET?)                      
         BO    CUTH0130            YES - JUST RECALC # WEEKS                    
         GOTO1 DATCON,DMCB,(3,RBUYDTST),(0,WORK)                                
*                                  CONVERT BUY START DATE TO EBCDIC             
         GOTO1 ADDAY,DMCB,WORK,WORK,7                                           
*                                  BUMP TO NEXT WEEK                            
         GOTO1 DATCON,DMCB,(0,WORK),(3,RBUYDTST)                                
*                                  CONVERT NEW START TO BINARY                  
         ZIC   RF,RBUYDTWK         DECREASE WEEKS BY 1                          
         BCTR  RF,0                                                             
         STC   RF,RBUYDTWK                                                      
CUTH0130 EQU   *                   RECALCULATE NUMBER OF WEEKS                  
         ZIC   RF,RBUYDTWK         EXTRACT NUMBER OF WEEKS                      
         LA    RF,1(RF)            MAKE NUMBER OF WEEKS EVEN                    
         SRL   RF,1                DIVIDE NUMBER OF WEEKS BY 2                  
         STC   RF,RBUYDTWK         REPLACE NUMBER OF WEEKS                      
CUTH0140 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         B     CUTH0180                                                         
CUTH0160 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
CUTH0180 EQU   *                                                                
         XIT1                                                                   
         DROP  R3                                                               
**CUTHIST                                                                       
         EJECT                                                                  
*                                                                               
* ROUTINE TO ADD REC TO CONTRACT   P1=A(BUYREC OR PLNREC)                       
*                                     IF BYTE 0=X'FF'-SUBTRACTION               
BUCKUP   NTR1                                                                   
         L     R4,AIO1                                                          
         USING RCONREC,R4                                                       
*                                                                               
         L     R2,0(R1)            A(BUYREC)                                    
         L     R0,VRECUP                                                        
         GOTOX (RFBUCKUP,VREPFACS),DMCB,(R2),(BUCKFLGS,RCONREC),       +        
               ACOMFACS,GETBROAD,(R0)                                           
         BNE   BUUP0100                                                         
         XIT1                                                                   
*                                                                               
BUUP0100 EQU   *                                                                
         LA    R2,CTOSTAH                                                       
         L     R3,0(R1)                                                         
         GOTO1 GETTXT,DMCB+12,(R3),0,(C'E',DMCB),0,0,0                          
         DC    H'0',C'$ABEND'                                                   
*                                                                               
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**SAVEBUKS                                                                      
******************************************************************              
*  SAVEBUKS:  STRIP ALL BUCKETS FROM RECORD FOR REINSERTION                     
*        NOTE:  BUCKETS MUST BE DROPPED BY MONTH OF SERVICE.                    
*        BECAUSE THE EARLIEST START DATE MAY BE IN THE MONTH                    
*        PRIOR TO THE MONTH OF SERVICE, A SEPARATE DATE IS                      
*        REQUIRED FOR THIS TESTING.  FOR INSTANCE, START DATE                   
*        OF THE HISTORY IS BROADCAST MONTH JAN/98, WHICH BEGINS                 
*        DEC29/97.  A SEPARATE DATE TO DROP BUCKETS PRIOR TO                    
*        JAN/98 IS NEEDED.                                                      
*                                                                               
******************************************************************              
SAVEBUKS NMOD1 0,*SBUK*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            RESET A(RCONREC)                             
         USING RCONREC,R2                                                       
*                                                                               
         L     RF,ABUCKBLK         CLEAR ANY PREVIOUS BUCKETS                   
         XCEFL 0(RF),2000                                                       
         L     RF,ABUCKBLK                                                      
         A     RF,=F'1992'                                                      
         OC    0(4,RF),=X'FFFFFFFF'                                             
         OC    4(4,RF),=X'FFFFFFFF'                                             
*                                  SET LAST ENTRY TO FOXES                      
         L     RF,ABUCKBLK         RESET A(WORKSTORAGE SPACE)                   
         LA    R3,RCONELEM                                                      
SBUK0020 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    SBUK0200            YES -                                        
         CLI   0(R3),3             ESTIMATE BUCKET?                             
         BNE   SBUK0040            NO                                           
         CLC   0(4,RF),=X'FFFFFFFF'                                             
*                                  END OF TABLE REACHED?                        
         BNE   *+6                 NO                                           
         DC    H'0'                YES - TOO MANY BUCKETS:                      
         MVC   0(10,RF),0(R3)      YES - MOVE TO STORAGE                        
         LA    RF,10(RF)           BUMP STORAGE                                 
         MVI   0(R3),X'FF'         SET ELEMENT TO DELETE                        
         B     SBUK0080                                                         
SBUK0040 EQU   *                                                                
         CLI   0(R3),4             INVOICE BUCKET?                              
         BNE   SBUK0080            NO                                           
         CLC   2(2,R3),EARLYMOS                                                 
         BNL   SBUK0060                                                         
         MVI   0(R3),X'FF'         EARLIER THAN EARLY: DROP IT                  
SBUK0060 EQU   *                                                                
         CLC   BENDDT(2),2(R3)                                                  
         BNL   SBUK0080                                                         
         MVI   0(R3),X'FF'         LATER THAN LATE: DROP IT                     
SBUK0080 EQU   *                                                                
         ZIC   RE,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RE                                                            
         B     SBUK0020            GO BACK FOR NEXT                             
SBUK0200 EQU   *                                                                
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'FF',RCONREC),0,0                
*                                  DELETE OLD 03/04 ELEMENTS                    
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**SAVEBUKS                                                                      
**RESETBUK                                                                      
******************************************************************              
*  RESETBUK:                                                                    
*        AT THIS POINT, THE BUCKETS HAVE BEEN REBUILT FROM THE                  
*        BUY RECORDS.  ONLY THE BUCKETS REPRESENTING THOSE MONTHS               
*        WHICH HAVE BEEN CUT WILL BE KEPT WITHIN THE RECORD.                    
*        THE OTHERS WILL BE DROPPED.  THESE MONTHS WILL BE REPLACED             
*        BY THE BUCKETS FROM THE ORIGINAL RECORD/PRE PROCESSING.                
*                                                                               
*        1.  DROP ALL NON-CUT BUCKETS FROM CONTRACT RECORD.                     
*        2.  MOVE ALL NON-CUT BUCKETS BACK TO CONTRACT FROM                     
*            STORAGE AREA.                                                      
*        3.  SET ACTIVITY DATE IN ALL CUT MONTH BUCKETS TO                      
*            THE EARLIEST ACTIVITY DATE IN THE STORAGE BUCKET                   
*            FOR THAT MONTH.                                                    
*                                                                               
******************************************************************              
RESETBUK NMOD1 0,*RBUK*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            POINT TO STORED CONTRACT                     
         USING RCONREC,R2                                                       
         LA    R3,RCONELEM         SET A(01 ELEMENT OF CONTRACT)                
RBUK0020 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    RBUK0060            YES -                                        
         CLI   0(R3),3             ESTIMATE BUCKET?                             
         BNE   RBUK0040            NO                                           
         CLC   2(2,R3),EARLYMOS    YES - EQUAL TO EARLY MONTH?                  
         BE    RBUK0040            YES - LEAVE ALONE                            
         CLC   2(2,R3),BENDDT      YES - EQUAL TO LATE  MONTH?                  
         BE    RBUK0040            YES - LEAVE ALONE                            
         MVI   0(R3),X'FF'         NO  - SET ELEMENT TO DELETE                  
RBUK0040 EQU   *                                                                
         ZIC   RE,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RE                                                            
         B     RBUK0020            GO BACK FOR NEXT                             
RBUK0060 EQU   *                                                                
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'FF',RCONREC),0,0                
*                                  DELETE 03 ELTS FOR NON-CUTBACK               
*                                     MONTHS OF SERVICE                         
         L     R3,ABUCKBLK         SET A(BUCKET STORAGE)                        
         XC    EACTDATE(4),EACTDATE                                             
*                                  CLEAR ACTIVITY DATES, EARLY/LATE             
*                                  DERIVE EARLY/LATE ACTIVITY DATES             
*                                     FROM ORIGINAL DATA                        
RBUK0080 EQU   *                                                                
         OC    0(10,R3),0(R3)      ANY BUCKET IN SLOT?                          
         BZ    RBUK0160            NO  - FINISHED                               
         CLC   2(2,R3),EARLYMOS    YES - EQUAL TO EARLY MONTH?                  
         BNE   RBUK0100            NO  - CHECK FOR LATE ACT DATE                
         OC    EACTDATE,EACTDATE   YES - EARLY ACTIVITY DATE FOUND?             
         BNZ   RBUK0140            YES - DON'T SET AGAIN                        
         MVC   EACTDATE,4(R3)      NO  - SAVE EARLY ACTIVITY DATE               
         B     RBUK0140                                                         
RBUK0100 EQU   *                                                                
         CLC   2(2,R3),BENDDT      YES - EQUAL TO LATE  MONTH?                  
         BNE   RBUK0120            NO  - SKIP TO NEXT BUCKET                    
         OC    LACTDATE,LACTDATE   LATE  ACTIVITY DATE FOUND?                   
         BNZ   RBUK0140            YES - DON'T SET AGAIN                        
         MVC   LACTDATE,4(R3)      NO  - SAVE LATE  ACTIVITY DATE               
         B     RBUK0140                                                         
RBUK0120 EQU   *                                                                
         CLC   2(2,R3),EARLYMOS    BUCKET PRE-EARLY DATE?                       
         BL    RBUK0140            YES - DON'T REINSERT INTO CONTRACT           
         CLC   2(2,R3),BENDDT      BUCKET POST-LATE DATE?                       
         BH    RBUK0140            YES - DON'T REINSERT INTO CONTRACT           
         PRINT GEN                                                              
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,0(R3),0                    
         PRINT NOGEN                                                            
*                                  NO  - INSERT 03 ELEMENT INTO RECORD          
RBUK0140 EQU   *                                                                
         LA    R3,10(R3)           BUMP TO NEXT SLOT                            
         B     RBUK0080            GO BACK FOR NEXT                             
RBUK0160 EQU   *                                                                
         LA    R3,RCONELEM         SET A(01 ELEMENT OF CONTRACT)                
RBUK0180 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    RBUK0240            YES -                                        
         CLI   0(R3),3             ESTIMATE BUCKET?                             
         BNE   RBUK0220            NO                                           
         CLC   2(2,R3),EARLYMOS    YES - EQUAL TO EARLY MONTH?                  
         BNE   RBUK0200            NO  -                                        
         MVC   4(2,R3),EACTDATE    YES - INSERT ORIGINAL EARLY ACT DATE         
         B     RBUK0220                                                         
RBUK0200 EQU   *                                                                
         CLC   2(2,R3),BENDDT      YES - EQUAL TO LATE  MONTH?                  
         BNE   RBUK0220            NO  -                                        
         MVC   4(2,R3),LACTDATE    YES - INSERT ORIGINAL LATE ACT DATE          
RBUK0220 EQU   *                                                                
         ZIC   RE,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RE                                                            
         B     RBUK0180            GO BACK FOR NEXT                             
RBUK0240 EQU   *                                                                
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**RESETBUK                                                                      
*INSRT*>                                                                        
*                                                                               
*   TAKECFC:  LOOK FOR CONFIRM COMMENT RECORD.  IF PRESENT, MOVE                
*        WITH NEW CONTRACT NUMBER.                                              
*                                                                               
TAKECFC  NMOD1 0,*TKCF*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
***>>>                                                                          
         L     R5,4(R1)            RESET A(CONTRACT RECORD)                     
         USING RCONREC,R5                                                       
         L     R2,AIO2             SET A(IOAREA 2)                              
         USING RCFCREC,R2          SET ADDRESSABILITY TO CFC RECORD             
*                                                                               
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
         XC    RCFCREC(32),RCFCREC                                              
         MVI   RCFCKTYP,RCFCKTYQ                                                
         MVC   RCFCKREP,SRCEREP    INSERT SOURCE REP INTO KEY                   
         MVC   RCFCKCON,SRCECON    INSERT SOURCE CON # INTO KEY                 
         MVC   KEY,RCFCREC                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCFCKEY),KEYSAVE                                           
         BE    TKCF0020            FOUND - COPY IT OVER                         
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  NOT FOUND:  SWITCH BACK                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
         B     TKCF0100            NOT FOUND - EXIT                             
TKCF0020 EQU   *                                                                
         MVC   SAVEAIO,AIO         SAVE ORIGINAL IO AREA                        
         MVC   AIO,AIO2            SET IO AREA = IO2                            
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
         MVC   RCFCKREP,TARGREP    INSERT TARGET REP INTO KEY                   
         MVC   RCFCKCON,RCONKCON   NO  - INSERT NEW CONTRACT NUMBER             
*                                                                               
         GOTO1 ADDREC,DMCB,RCFCREC                                              
*                                  ADD CFC RECORD                               
         MVC   AIO,SAVEAIO         RESET ORIGINAL IO AREA                       
*                                                                               
TKCF0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*INSRT*>                                                                        
**TKCV**                                                                        
*                                                                               
*   TAKECOV:  MOVE COVERSHEET RECORDS.  'A6' COVERSHEET ELEMENT                 
*        WAS FOUND.                                                             
*                                                                               
TAKECOV  NMOD1 0,*TKCV*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         L     R5,4(R1)            RESET A(CONTRACT RECORD)                     
         USING RCONREC,R5                                                       
*                                                                               
         MVI   ELCODE,X'A6'        FIND COVERSHEET ELEMENT                      
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                 MUST BE THERE!!                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO2             SET A(IOAREA 2)                              
         USING RCOVREC,R2          SET ADDRESSABILITY TO COV RECORD             
*                                                                               
         MVC   SAVEAIO,AIO         SAVE ORIGINAL IO AREA                        
*                                                                               
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
         XC    RCOVREC(32),RCOVREC                                              
         MVI   RCOVKTYP,X'49'      SET COVER SHEET RECORD TYPE                  
         MVC   RCOVKREP,SRCEREP    INSERT SOURCE REP INTO KEY                   
         MVI   RCOVKNAM,X'FF'      SET 'CONTRACT COVERSHEET' FLAG               
         MVC   RCOVKNAM+4(4),COVSHT#                                            
*                                  INSERT SOURCE CONTRACT NUMBER                
*                                     FROM ORIGINAL A6 ELEMENT                  
         MVC   KEY,RCOVREC                                                      
         GOTO1 HIGH                                                             
         B     TKCV0040                                                         
TKCV0020 EQU   *                                                                
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
         MVC   KEY(27),KEYSAVE     RESET KEY                                    
         GOTO1 HIGH                REREAD KEY TO START ON SRCE SIDE             
*                                                                               
         GOTO1 SEQ                 READ NEXT KEY                                
TKCV0040 EQU   *                                                                
         CLC   KEY(26),KEYSAVE     SAME KEY, UP TO SEQ #?                       
         BNE   TKCV0080            NO  - FINISHED WITH COVERSHEET               
         MVC   KEYSAVE,KEY         SAVE KEY ACCESSED FOR RESTART                
TKCV0060 EQU   *                                                                
         MVC   AIO,AIO2            SET IO AREA = IO2                            
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
         MVC   RCOVKREP,TARGREP    INSERT TARGET REP INTO KEY                   
         MVC   RCOVKNAM+4(4),RCONKCON                                           
*                                  INSERT NEW CONTRACT NUMBER                   
         GOTO1 ADDREC,DMCB,RCOVREC                                              
*                                  ADD COV RECORD                               
         B     TKCV0020            GO BACK FOR NEXT RECORD                      
TKCV0080 EQU   *                                                                
         MVC   AIO,SAVEAIO         RESET ORIGINAL IO AREA                       
*                                                                               
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
         XIT1                                                                   
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**TKCV**                                                                        
*                                                                               
*                                                                               
*   TAKEDARE:  READ ALL DARE FOR ORIGINAL ORDER, MOVE TO NEW FILE               
*        WITH NEW CONTRACT NUMBER.                                              
*                                                                               
* THIS ROUTINE WILL TAKEOVER X'41' RECORDS ONLY IF THERE ARE NO                 
* EXISTING X'41' RECORDS IN THE NEW REP'S INBOX. THE REASON IS IF THERE         
* IS A REVISED ORDER SITTING IN THE NEW REP'S INBOX, COPYING THE X'41'          
* RECORDS WILL BLOW AWAY ANY CHANGES THE AGENCY MAY HAVE MADE                   
*                                                                               
TAKEDARE NMOD1 0,*TKDR*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R5,4(R1)            RESET A(CONTRACT RECORD)                     
         MVC   SAVEQUIV,8(R1)      SAVE A(EQUIV TBL IN PROCESS)                 
*                                                                               
         USING RCONREC,R5                                                       
*                                  COMPLEMENT/REVERSE CONTRACT NUMBER           
         MVI   ELCODE,X'1D'        FIND DARE ELEMENT                            
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   TKDR0180            ELEMENT NOT FOUND: NOT DARE                  
*                                  FOUND:  COPY DARE RECORD(S)                  
         TM    RCONDRFG-RCONDREL(R6),X'04'                                      
*                                  KATZ EDI ORDER?                              
         BO    TKDR0180            YES - TREAT AS NOT DARE                      
*                                                                               
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
*                                                                               
         L     R2,AIO2             SET A(IOAREA 2)                              
         USING RAGY2REC,R2         SET ADDRESSABILITY TO AGENCY RECORD          
*                                                                               
         XC    RAGY2REC(32),RAGY2REC                                            
         MVI   RAGK2TYP,RAGK2TYQ                                                
         L     RF,SAVEQUIV         SET A(EQUIVALENCY TBL ENTRY)                 
         MVC   RAGK2AGY(6),ETOAGYOF-EQUITABL(RF)                                
*                                  INSERT ORIGINAL AGENCY/OFFICE                
         MVC   RAGK2REP,SRCEREP    INSERT SOURCE REP INTO KEY                   
         MVC   KEY,RAGY2REC                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(L'RAGY2KEY),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE!                               
         MVC   SAVEAIO,AIO         SAVE ORIGINAL IO AREA                        
         MVC   AIO,AIO2            SET IO AREA = IO2                            
         GOTO1 GETREC                                                           
*                                                                               
         MVC   DARAGYS,RAGY2DAR    PULL OUT FOUR (MAX) AGY ASSIGNS              
         MVC   DARAGYCD,RAGK2AGY   SAVE AGENCY CODE                             
*                                                                               
*   DARE TYPE X'41' RECORDS WILL BE PROCESSED ONLY FOR NEVER BEEN               
*     CONFIRMED CONTRACTS. ALL ELSE, X'51' RECORDS WILL BE TAKEN                
*        OVER.  THEREFORE, THE TYPE IS SET AT THE TOP OF THE LOOP.              
*                                                                               
         MVC   DARKTYP,=X'41'      SET DARE TYPE RECORD                         
*                                  COMPLEMENT/REVERSE CONTRACT NUMBER           
         MVI   ELCODE,X'1F'        FIND DARE ELEMENT                            
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    RCONCONF-RCONXEL(R6),X'40'+X'20'                                 
         BZ    TKDR0010                                                         
*                                                                               
         MVC   DARKTYP,=X'51'      SET DARE TYPE RECORD: FORCE 51S              
*                                                                               
TKDR0010 EQU   *                                                                
         MVC   AIO,SAVEAIO         RESTORE A(ORIGINAL IO AREA)                  
*                                                                               
         DROP  R2                                                               
*                                                                               
         LA    R4,KEY                                                           
         USING RDARKEY,R4                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   RDARKTYP(1),DARKTYP                                              
         MVC   RDARKREP,SRCEREP    INSERT ORIGINAL REP INTO KEY                 
         MVC   RDARKSTA(5),RCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   TKDR0015                                                         
         MVC   RDARKSTA+4(2),=C'T '                                             
TKDR0015 EQU   *                                                                
         OC    RDARKSTA(6),SPACES                                               
         MVC   RDARKAGY(5),DARAGYS                                              
*                                  LOAD THE 1ST EQUIVALENCY CODE                
         LA    R6,RCONREC                                                       
         USING RCONDREL,R6                                                      
         MVI   ELCODE,X'1D'        GET DARE ELEMENT                             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   RDARKORD,RCONDRLK   ORDER NUMBER                                 
         MVI   RDARKRT,X'10'       AGENCY HEADER ONLY                           
*                                                                               
         DROP  R6                                                               
*                                                                               
         LA    RF,DARAGYS          THERE ARE MAX 4 AGENCY ASSIGNMENT            
*                                     1ST IS ALREADY LOADED                     
         LA    RF,5(RF)            SKIP THE 1ST CODE                            
         ST    RF,SAVAGASS         SAVE A(2ND AGENCY ASSIGNMENT)                
         LA    R0,3                COMBINATIONS WE NEED TO CHECK                
*                                                                               
TKDR0020 DS    0H                                                               
         GOTO1 HIGH                                                             
****     GOTO1 READ                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    TKDR0040                                                         
         MVC   KEY,KEYSAVE         RESET KEY NOT FOUND                          
         L     RF,SAVAGASS         RESTORE A(AGENCY ASSIGNMENT)                 
         MVC   RDARKAGY(5),0(RF)   INSERT NEXT EQUIVALENCY CODE                 
         LA    RF,5(RF)            BUMP TO NEXT EQUIV CODE                      
         ST    RF,SAVAGASS         SAVE A(AGENCY ASSIGNMENT)                    
*                                                                               
*                                                                               
         BCT   R0,TKDR0020                                                      
*                                                                               
         CLI   DARKTYP,X'51'       PROCESSING X'51' RECS?                       
         BE    TKDR0160            YES - MAY BE NO TYPE 51'S                    
*                                                                               
         CLC   =C'SZ',SRCEREP      SELTEL+AGENCY 1342 CHECK                     
         BNE   TKDR0150            NO  - GO CHECK TYPE X'51'S                   
         CLC   =C'1342  ',DARAGYCD YES - CHECK SPECIAL CODE                     
         BNE   TKDR0150            NO  - GO CHECK TYPE X'51'S                   
         MVC   RDARKAGY(5),=C'ED2DE'                                            
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    TKDR0040                                                         
         B     TKDR0150            NO  - GO CHECK TYPE X'51'S                   
*                                                                               
         DROP  R4                                                               
*                                                                               
TKDR0040 DS    0H                                                               
*                                                                               
         MVC   SAVEAIO,AIO         SAVE ORIGINAL IO AREA                        
         MVC   AIO,AIO2            SET IO AREA TO IO2                           
TKDR0050 DS    0H                                                               
         L     R6,AIO2                                                          
         GOTO1 GETREC,DMCB,(R6)                                                 
         USING RDARREC,R6                                                       
*                                                                               
TKDR0080 DS    0H                                                               
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
         MVC   RDARKREP,TARGREP    INSERT TARGET REP INTO KEY                   
         CLI   RDARKRT,X'10'       AGENCY HEADER RECORD?                        
         BNE   TKDR0100            NO  - DON'T INSERT NEW CON#                  
         BAS   RE,AGYORDEX         YES - CHECK: AGY ORDER ON NEW REP?           
         BNZ   TKDR0150            YES - DON'T BRING OVER                       
         MVC   RDARREP#,RCONKCON   NO  - INSERT NEW CONTRACT NUMBER             
TKDR0100 EQU   *                                                                
*                                                                               
         GOTO1 ADDREC,DMCB,RDARREC                                              
*                                  ADD DARE RECORD                              
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
         MVC   KEY(27),KEYSAVE     RESTART SOURCE FILE                          
         GOTO1 HIGH                REPOSITION TO LAST KEY ACCESSED              
*                                                                               
         GOTO1 SEQ                 ACCESS NEXT KEY                              
         CLC   KEY(24),KEYSAVE     SAME SET OF DARE RECORDS?                    
         BNE   TKDR0150            FINISHED: CHECK TYPE DARE                    
         MVC   KEYSAVE(27),KEY     SAVE KEY                                     
         B     TKDR0050            GO BACK FOR NEXT DARE                        
TKDR0150 EQU   *                                                                
         CLI   DARKTYP,X'41'       TYPE 41S IN PROGRESS?                        
         BNE   TKDR0160            NO  - ALL FINISHED                           
         MVI   DARKTYP,X'51'       YES - NOW DO TYPE 51S                        
         B     TKDR0010            GO BACK AND DO 51S                           
*                                                                               
TKDR0160 EQU   *                                                                
         MVC   AIO,SAVEAIO         RESET ORIGINAL IO AREA                       
*                                                                               
TKDR0180 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   AGYORDEX:  CHECK IF AGENCY ORDER EXISTS ALREADY ON THE NEW                  
*        REP'S FILE.  IF IT DOES, INSERT THE NEW CONTRACT NUMBER                
*        AND REWRITE THE RECORD.  THEN SKIP THE RECORDS ON THE                  
*        OLD SIDE.                                                              
*                                                                               
AGYORDEX NTR1                                                                   
         MVC   ELEM+200(27),KEYSAVE                                             
*                                  SAVE KEYSAVE FOR RESTART                     
         MVC   KEY(27),RDARKEY     DARE RECORD ALREADY ON FILE?                 
         MVI   KEY,X'41'           CHECK UNCONFIRMED DARE ORDER                 
         OI    DMINBTS,X'88'       READ DELETED                                 
         OI    GENSTAT1,RDUPAPPL                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08'                                              
         CLC   KEY(27),KEYSAVE     KEY FOUND ON NEW FILE?                       
         BNE   AORD0100            NO  - USE NEW FILE                           
         XC    0(64,R6),0(R6)      CLEAR IO AREA AS INDICATOR                   
         OI    DMINBTS,X'88'       READ DELETED                                 
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC,DMCB,(R6)    YES - GET NEW FILE RECORD                    
         NI    DMINBTS,X'FF'-X'08'                                              
         TM    RDARCNTL,X'80'      RECORD DELETED?                              
         BO    AORD0080            YES - DON'T DO ANYTHING:  SKIP               
*                                     THIS ORDER (ILLOGICAL)                    
         MVC   RDARREP#,RCONKCON   NO  - INSERT NEW CONTRACT NUMBER             
*        MVC   SAVEAIO,AIO         SAVE CURRENT A(IO AREA)                      
         ST    R6,AIO              SET A(RECORD TO BE REWRITTEN)                
         GOTO1 PUTREC              REWRITE THE RECORD WITH                      
*                                     NEW CONTRACT NUMBER                       
*        MVC   AIO,SAVEAIO         RESTORE IO AREA                              
AORD0080 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     AORD0120            EXIT                                         
AORD0100 EQU   *                                                                
         MVC   KEYSAVE(27),ELEM+200                                             
*                                  RESET ORIG SRCE KEY: DON'T READ!             
         SR    R0,R0               SET CC ZERO FOR RETURN                       
AORD0120 EQU   *                                                                
         XIT1                                                                   
         DROP  R5,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   TAKEMGS:  READ ALL MG'S FOR ORIGINAL ORDER, MOVE TO NEW FILE                
*        WITH NEW CONTRACT NUMBER.                                              
*                                                                               
TAKEMGS  NMOD1 0,*TKMG*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                  COMPLEMENT/REVERSE CONTRACT NUMBER           
         L     R5,AIO              SET A(CONTRACT RECORD AREA)                  
         USING RCONREC,R5                                                       
*                                                                               
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),SRCECON(4)                                            
                                                                                
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   WORK+32(4),WORK+15                                               
*                                  REVERSE THE COMPLIMENT                       
         PACK  WORK+32(1),WORK+18(1)                                            
         PACK  WORK+33(1),WORK+17(1)                                            
         PACK  WORK+34(1),WORK+16(1)                                            
         PACK  WORK+35(1),WORK+15(1)                                            
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'11'           INSERT MAKEGOOD BUY KEY TYPE                 
         MVC   KEY+06(2),SRCEREP   INSERT SOURCE REP CODE                       
         MVC   KEY+08(2),RCONKOFF  INSERT SOURCE OFFICE CODE                    
         MVC   KEY+10(5),RCONKSTA  INSERT SOURCE STATION CODE                   
         MVC   KEY+15(4),WORK+32   INSERT CON#, COMP/REVERSED                   
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
         GOTO1 HIGH                ACCESS FIRST KEY                             
         MVI   COPYMGHD,C'N'                                                    
         MVC   MGHEADCD,KEY+19                                                  
         B     TKMG0080                                                         
TKMG0040 EQU   *                                                                
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
         MVC   KEY(27),MGSAVEKY    RESTART SOURCE FILE                          
         GOTO1 HIGH                REPOSITION TO LAST KEY ACCESSED              
*                                                                               
         GOTO1 SEQ                 ACCESS NEXT KEY                              
TKMG0080 EQU   *                                                                
         CLC   KEY(19),KEYSAVE     SAME REC TYPE/REP/OFF/STA/CON#?              
         BE    TKMG0085            NO  - FINISHED WITH CONTRACT                 
         CLI   COPYMGHD,C'N'                                                    
         BE    TKMG0240                                                         
         MVI   COPYMGHD,C'L'       WRITE FOR THE LAST TIME                      
         B     TKMG0100            NO  - FINISHED WITH CONTRACT                 
*                                                                               
TKMG0085 EQU   *                                                                
         MVC   MGSAVEKY,KEY        SAVE KEY ACCESSED FOR RESTART                
         MVC   SAVEAIO,AIO         SAVE ORIGINAL IO AREA                        
         MVC   AIO,AIO3                                                         
         L     R4,AIO3                                                          
         USING RMKGREC,R4                                                       
         GOTO1 GETREC,DMCB,RMKGREC                                              
*                                  RETRIEVE ORIGINAL M/G RECORD                 
         MVC   AIO,SAVEAIO         RESTORE ORIGINAL IO AREA                     
*                                                                               
         OC    RMKGKPLN(5),RMKGKPLN                                             
         BNZ   TKMG0110            HEADER RECORD ?                              
*                                                                               
TKMG0090 EQU   *                                                                
         CLI   COPYMGHD,C'Y'       WRITE PREVIOUS HEADER RECORD?                
         BNE   TKMG0040                                                         
*                                  FIND HEADER RECORD                           
TKMG0100 EQU   *                                                                
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY(19),MGSAVEKY                                                 
         MVC   KEY+19(2),MGHEADCD                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   MGHEADCD,RMKGKGRP   SAVE FOR NEXT TIME AROUND                    
         CLI   COPYMGHD,C'L'       UNLESS IT'S THE LAST ITERATION               
         BE    *+8                                                              
         MVI   COPYMGHD,C'N'       RESET FOR NEXT ROUND                         
*                                                                               
         MVC   SAVEAIO,AIO         SAVE ORIGINAL IO AREA                        
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,SAVEAIO         RESTORE ORIGINAL IO AREA                     
*                                                                               
         B     TKMG0200                                                         
*                                                                               
TKMG0110 EQU   *                                                                
         LA    R6,RMKGELEM         DETAIL:  SCAN FOR OFFER TARGET DATE          
TKMG0120 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD?                               
         BNE   TKMG0130            YES - ACCEPT RECORD                          
         MVI   COPYMGHD,C'Y'       WRITE OUT HEADER ALSO                        
         B     TKMG0200                                                         
*                                                                               
TKMG0130 EQU   *                                                                
         CLI   0(R6),5             MG REFERENCE ELEMENT?                        
         BNE   TKMG0160            NO  - BUMP TO NEXT                           
         USING RMKGMGEL,R6                                                      
*                                                                               
         CLC   RMKGMGD1,EFDTBSUN   MAKEGOOD TARGET DATE < TKO DATE?             
*                                                                               
         DROP  R6                                                               
*                                                                               
         BL    TKMG0040            YES - BYPASS THIS RECORD                     
TKMG0160 EQU   *                                                                
         ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         B     TKMG0120            GO BACK FOR NEXT                             
TKMG0200 EQU   *                                                                
*                                                                               
*   CONTRACT NUMBER REVERSAL/COMPLEMENT LOGIC (NEW CONTRACT NUMBER)             
*                                                                               
*                                  COMPLEMENT/REVERSE CONTRACT NUMBER           
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),RCONKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   WORK+32(4),WORK+15                                               
*                                  REVERSE THE COMPLIMENT                       
         PACK  WORK+32(1),WORK+18(1)                                            
         PACK  WORK+33(1),WORK+17(1)                                            
         PACK  WORK+34(1),WORK+16(1)                                            
         PACK  WORK+35(1),WORK+15(1)                                            
*                                                                               
         MVC   RMKGKCON,WORK+32    INSERT NEW CON# INTO BUY                     
         MVC   RMKGKREP,TARGREP    INSERT NEW REP CODE                          
*                                                                               
*                                  SWITCH BACK TO ORIGINAL FILE TO              
*                                     ADD THE NEW BUY RECORD                    
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
         MVC   SAVEAIO,AIO         SAVE ORIGINAL IO AREA                        
         MVC   AIO,AIO3                                                         
         GOTO1 ADDREC,DMCB,RMKGREC                                              
*                                  ADD M/G RECORD FOR NEW CONTRACT              
         MVC   AIO,SAVEAIO         RESET ORIGINAL IO AREA                       
         CLI   COPYMGHD,C'L'                                                    
         BNE   TKMG0040            GO BACK FOR NEXT BUY                         
*                                                                               
***>>>                                                                          
TKMG0240 EQU   *                                                                
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
         DROP  R4,R5                                                            
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*********************************************************************           
*  CHEKALT:  SCAN ALL SELECTED CONTRACTS TO ENSURE THAT FLIGHT DATES*           
*        ARE COMPLETELY COVERED BY ALTERNATE CALENDARS.  STATION    *           
*        RECORD INDICATES THAT ALTERNATE BUCKETS ARE NEEDED.        *           
*********************************************************************           
CHEKALT  NMOD1 0,*CALT*                                                         
         L     RC,0(R1)                                                         
         L     R5,AEQUITBL         SET A(EQUITABL)                              
         USING EQUITABL,R5                                                      
         L     R2,ACMBOTBL         SET A(COMBO TABLE FOR THIS ORDER:            
*                                     PARTICIPATING STATIONS WHICH              
*                                        ARE PREP'D FOR TAKEOVER)               
*                                                                               
CALT0020 EQU   *                                                                
         OC    0(6,R5),0(R5)       ANY ENTRY IN SLOT?                           
         BZ    CALT0120            NO  - EXIT CC ZERO                           
         TM    ETFLAG,X'80'        CONTRACT SELECTED?                           
         BNO   CALT0080            NO  - SKIP IT                                
         L     R4,AIO                                                           
         USING RCONREC,R4                                                       
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY+28(4),ETDSKADR  SET DISK ADDR OF CONTRACT                    
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
         GOTO1 GETREC,DMCB,RCONREC                                              
*                                  RETRIEVE THE CONTRACT RECORD                 
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'03',RCONREC),0,0                
*                                  DELETE OLD ESTIMATE ELTS                     
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'04',RCONREC),0,0                
*                                  DELETE OLD INVOICE  ELTS                     
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'53',RCONREC),0,0                
*                                  DELETE OLD ALT CAL EST ELTS                  
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'54',RCONREC),0,0                
*                                  DELETE OLD ALT CAL INV ELTS                  
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'A4',RCONREC),0,0                
*                                  DELETE ALT CALENDAR ELT                      
*                                                                               
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
         XC    WORK,WORK                                                        
         MVC   WORK(4),ACOMFACS                                                 
         MVC   WORK+4(2),TWAAGY                                                 
*                                                                               
         GOTOX (RFVALTCL,VREPFACS),DMCB,(X'FF',RCONREC),GETBROAD,0,WORK         
         BNE   CALT0100            ALT CALENDAR MISSING: ERROR                  
         LA    R3,RCONELEM         LOOK FOR COMBO CONTROL ELT                   
CALT0040 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    CALT0080            YES - NOT COMBO ORDER                        
         CLI   0(R3),X'17'         COMBO CONTROL ELEMENT?                       
         BE    CALT0060            YES - PROCESS PARTICIPANTS                   
         ZIC   RF,1(R3)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R3,RF                                                            
         B     CALT0040            GO BACK FOR NEXT                             
CALT0060 EQU   *                                                                
         GOTO1 CHKALT2             CHECK PARTICIPATING CONTRACTS                
         BNZ   CALT0100            ALT CALENDAR MISSING: ERROR                  
CALT0080 EQU   *                                                                
         LA    R5,LEQUITBL(R5)     BUMP TO NEXT SLOT                            
         LA    R2,LCMBBUCK(R2)     BUMP TO NEXT COMBO SLOT                      
         B     CALT0020            GO BACK FOR NEXT                             
CALT0100 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
         B     CALT0160            EXIT WITH ERROR RETURN                       
CALT0120 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
CALT0160 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
*  CHKALT2:  SCAN COMBO CONTROL ELEMENT.  CHECK OTHER THAN PRIME                
*        ORDER FOR EXISTENCE OF ALTERNATE CALENDAR COVERAGE.                    
*        R2  -->  COMBO PARTICIPATING ORDER TABLE                               
*        R3  -->  COMBO CONTROL ELEMENT                                         
*        R4  -->  CONTRACT RECORD AREA                                          
*        R5  -->  EQUITABLE AREA IN PROCESS                                     
*                                                                               
CHKALT2  NTR1                                                                   
         ZIC   RF,1(R3)            GET L(COMBO CONTROL ELEMENT)                 
         SR    RE,RE                                                            
         D     RE,=F'9'            CALCULATE NUMBER OF ENTRIES                  
         LR    R0,RE               SET LOOP CONTROL                             
         LA    R3,2(R3)            POINT TO 1ST COMBO PARTICIPANT               
CHAL0020 EQU   *                                                                
         CLC   5(4,R3),ETOCONUM    IS THIS BASE CONTRACT?                       
         BE    CHAL0040            YES - SKIP IT                                
         LR    RF,R2               SET A(COMBO PARTICIPATING ORDERS)            
         LA    RE,3                SET LOOP                                     
CHAL0030 EQU   *                                                                
         CLC   5(4,R3),ACMBOCON(RF)                                             
*                                  ORDER CAN BE TAKEN OVER?                     
         BE    CHAL0038            YES                                          
         LA    RF,LCMBNTRY(RF)     NO  - BUMP TO NEXT SLOT                      
         BCT   RE,CHAL0030         GO BACK AND CHECK NEXT                       
         B     CHAL0040            NOT FOR TAKEOVER - SKIP IT                   
CHAL0038 EQU   *                                                                
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'8C'           INSERT KEY CODE                              
         MVC   KEY+21(2),SRCEREP   INSERT SOURCE REP CODE                       
*                                  COMPLEMENT CONTRACT NUMBER                   
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),5(4,R3)                                               
                                                                                
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   WORK+32(4),WORK+15                                               
         MVC   KEY+23(4),WORK+32   INSERT CONTRACT NUMBER                       
*                                                                               
         GOTO1 =A(SETFIL),DMCB,(RC),RR=Y                                        
*                                  SWITCH TO SOURCE FILE, IF NEEDED             
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     RECORD FOUND?                                
         BE    *+6                 NO  - MUST BE THERE                          
         DC    H'0'                ABORRT                                       
*                                                                               
         GOTO1 GETREC,DMCB,RCONREC                                              
*                                  RETRIEVE THE CONTRACT RECORD                 
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'03',RCONREC),0,0                
*                                  DELETE OLD ESTIMATE ELTS                     
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'04',RCONREC),0,0                
*                                  DELETE OLD INVOICE  ELTS                     
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'53',RCONREC),0,0                
*                                  DELETE OLD ALT CAL EST ELTS                  
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'54',RCONREC),0,0                
*                                  DELETE OLD ALT CAL INV ELTS                  
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'A4',RCONREC),0,0                
*                                  DELETE ALT CALENDAR ELT                      
*                                                                               
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
         XC    WORK,WORK                                                        
         MVC   WORK(4),ACOMFACS                                                 
         MVC   WORK+4(2),TWAAGY                                                 
         GOTOX (RFVALTCL,VREPFACS),DMCB,(X'FF',RCONREC),GETBROAD,0,WORK         
         BNE   CHAL0060            ALT CALENDAR MISSING: ERROR                  
CHAL0040 EQU   *                                                                
         LA    R3,9(R3)            BUMP TO NEXT COMBO PARTICIPANT               
         BCT   R0,CHAL0020         GO BACK AND CHECK NEXT                       
         SR    R0,R0               SET CC = ZERO                                
         B     CHAL0080            EXIT CC ZERO                                 
CHAL0060 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
CHAL0080 EQU   *                                                                
         XIT1                                                                   
         DROP  R4,R5                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*********************************************************************           
*  VALSCRN:  VALIDATES NEW SCREEN FIELDS, ADDS INTO EQUITABL,       *           
*        PASSES BACK ERROR ADDR/MESSAGE #, IF ENCOUNTERED, FOR      *           
*        EACH LINE SELECTED.  PROPAGATES CODES AS NECESSARY         *           
*********************************************************************           
VALSCRN  NMOD1 0,*VSCR*                                                         
         L     RC,0(R1)                                                         
         GOTO1 =A(RESETFIL),DMCB,(RC),RR=Y                                      
*                                  SWITCH TO TARGET FILE, IF NEEDED             
         L     R2,AEQUITBL         SET A(EQUIVALENCY TABLE)                     
         USING EQUITABL,R2                                                      
*                                                                               
         LA    R3,CTOSELH          SET A(1ST SCREEN SELECT FIELD)               
VASC0020 EQU   *                                                                
         CLI   8(R3),C'S'          CONTRACT SELECTED?                           
         BE    VASC0040            YES - PROCESS FURTHER                        
         CLI   TAKEALL,C'Y'        ALL CONTRACTS SELECTED?                      
         BE    VASC0040            YES - PROCESS FURTHER                        
*                                  NO  - BUMP TO NEXT EQUIV ENTRY               
         NI    ETFLAG,X'FF'-X'80'  TURN OFF 'SELECTED' FLAG                     
VASC0030 EQU   *                                                                
         LA    R2,LEQUITBL(R2)     BUMP TO NEXT EQUIV ENTRY                     
         LA    R3,CTOSEL1H-CTOSELH(R3)                                          
*                                  BUMP TO NEXT SELECT FIELD                    
         LA    RF,CTOTAGH          CHECK FOR END OF SCREEN                      
         CR    R3,RF               END OF SCREEN REACHED?                       
         BL    VASC0020            NO  - GO BACK AND CHECK NEXT                 
         B     VASC0800            YES - FINISHED                               
VASC0040 EQU   *                                                                
         OC    0(LEQUITBL,R2),0(R2)                                             
*                                  ANY ENTRY IN SLOT?                           
         BZ    VASC0030            NO  - SKIP OVER IT                           
         TM    ETFLAG,X'40'        FIELD ALREADY PROCESSED?                     
         BNO   VASC0050            NO                                           
         CLI   TAKEALL,C'Y'        YES - 'PROCESS ALL (S+)' REQUEST?            
         BE    VASC0030            YES - SKIP OVER ALREADY DONE                 
         ST    R3,DUB              SET A(FIELD IN ERROR)                        
         MVC   RERROR,=AL2(717)    FIELD ALREADY PROCESSED                      
         B     VASC0780                                                         
VASC0050 EQU   *                                                                
         OI    ETFLAG,X'80'        TURN ON 'SELECTED' FLAG                      
         ST    R3,ASELFLD          SAVE A(SELECT FIELD IN PROCESS)              
         LA    R3,CTONAGH-CTOSELH(R3)                                           
*                                  BUMP TO A(NEW AGENCY FIELD)                  
         BAS   RE,AGYCHECK         VALIDATE THE AGENCY CODE                     
         BNZ   VASC0780            ERROR RETURNED:  EXIT                        
         ZIC   RF,0(R3)            BUMP TO ADVERTISER FIELD                     
         AR    R3,RF                                                            
         BAS   RE,ADVCHECK         VALIDATE THE ADVERT CODE                     
         BNZ   VASC0780            ERROR RETURNED:  EXIT                        
         ZIC   RF,0(R3)            BUMP TO SALESPERSON FIELD                    
         AR    R3,RF                                                            
         BAS   RE,SALCHECK         VALIDATE THE SALESPERSON CODE                
         BNZ   VASC0780            ERROR RETURNED:  EXIT                        
         ZIC   RF,0(R3)            BUMP TO CONTRACT TYPE FIELD                  
         AR    R3,RF                                                            
         BAS   RE,TYPCHECK         VALIDATE THE CONTRACT TYPE CODE              
         BNZ   VASC0780            ERROR RETURNED:  EXIT                        
         L     R3,ASELFLD          RESET A(SELECT FIELD IN PROCESS)             
         B     VASC0030            NO ERROR - DO NEXT LINE                      
**                                                                              
VASC0780 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
         B     VASC0820            EXIT WITH ERROR                              
VASC0800 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
VASC0820 EQU   *                                                                
         XIT1                                                                   
ASELFLD  DS    F                                                                
         EJECT                                                                  
*                                                                               
*   AGYCHECK:  VALIDATE THE NEW AGENCY FIELD.  IF NOT PRESENT,                  
*        ATTEMPT TO FILL IT FROM OTHER INFORMATION ON THE SCREEN.               
*        R2 ->  A(EQUIVALENCY TABLE ENTRY FOR CONTRACT)                         
*        R3 ->  AGENCY CODE FIELD IN QUESTION                                   
*                                                                               
AGYCHECK NTR1                                                                   
*                                                                               
         LR    R5,R3               SET A(AGENCY FIELD HEADER)                   
         CLI   8(R5),C'='          REQUEST FOR BROWSER CALL?                    
         BNE   AGYC0020            NO                                           
**       MVC   DMCB+20(1),SRCEUTL                                               
**       MVC   DMCB+21(1),ORIGUTL                                               
**       MVC   DMCB+22(2),SRCEREP                                               
         GOTOX (RFBROWSE,VREPFACS),DMCB,ACOMFACS,BASERD,(R5),0,        +        
               (X'40',C' AGY'),0                                                
**       GOTO1 =V(REBROWSE),DMCB,ACOMFACS,BASERD,(R5),0,(0,C' AGY'),0,+         
**             RR=Y                                                             
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
AGYC0020 EQU   *                                                                
         TM    4(R3),X'20'         FIELD PREVIOUSLY VALID?                      
         BO    AGYC0800            YES - EXIT CC ZERO                           
         CLI   5(R3),0             ANYTHING ON LINE?                            
         BNE   AGYC0200            YES                                          
         L     R4,AEQUITBL         SET A(EQUIVALENCY TABLE)                     
AGYC0040 EQU   *                                                                
         CLC   ETOAGYOF,ETOAGYOF-EQUITABL(R4)                                   
*                                  ORIGINAL LINE AGY/OFC = TBL ENTRY?           
         BNE   AGYC0050            NO  - BUMP TO NEXT ENTRY                     
         OC    ETNAGYOF-EQUITABL(6,R4),ETNAGYOF-EQUITABL(R4)                    
*                                  YES - NEW CODE FOR TABLE ENTRY?              
         BZ    AGYC0050            NO  - CAN'T USE IT                           
         CR    R4,R2               YES - TABLE ENTRY ALREADY PROCESSED?         
         BNL   AGYC0720            NO  - CONSIDER ITEM NOT ENTERED              
         TM    ETFLAG,X'80'        YES - TABLE ENTRY SELECTED?                  
         BNE   AGYC0060            NO  -                                        
         MVC   ETNAGYOF,ETNAGYOF-EQUITABL(R4)                                   
*                                  YES - ALREADY VALIDATED -                    
*                                     USE ITS CODE FOR THIS LINE                
         MVC   8(6,R3),ETNAGYOF    INSERT INTO SCREEN OUTPUT                    
         FOUT  (R3)                SET TO TRANSMIT                              
         OI    4(R3),X'20'         SET FIELD PREV VALID                         
         B     AGYC0800            EXIT CC ZERO                                 
AGYC0050 EQU   *                                                                
         LA    R4,LEQUITBL(R4)     BUMP TO NEXT TABLE ENTRY                     
         OC    0(12,R4),0(R4)      ANY ENTRY?                                   
         BZ    AGYC0720            NO  - TREAT AS 'NO FIND'                     
         B     AGYC0040            YES - GO BACK AND CHECK IT                   
AGYC0060 EQU   *                                                                
*                                  TABLE ENTRY NOT SELECTED:                    
*                                     CODE MUST HAVE BEEN ON FILE -             
*    NOTE:  SCREEN FIELD MAY HAVE BEEN CHANGED ON UNSELECTED LINE               
*        HOWEVER, ON-FILE VALUE OF CODE IS IN TABLE, AND                        
*        WILL BE USED.                                                          
*                                                                               
         TM    ETCDEFLG,AGONFILE   AGENCY CODE ON FILE?                         
         BO    AGYC0780            NO  - EXIT CC NOT ZERO                       
         MVC   ETNAGYOF,ETNAGYOF-EQUITABL(R4)                                   
*                                  YES - ON-FILE CODE FOR AGY/OFF-              
*                                     USE ITS CODE FOR THIS LINE                
         MVC   8(6,R3),ETNAGYOF    INSERT INTO SCREEN OUTPUT                    
         FOUT  (R3)                SET TO TRANSMIT                              
         OI    4(R3),X'20'         SET FIELD PREV VALID                         
         B     AGYC0800            EXIT CC ZERO                                 
AGYC0200 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'           SET KEY FOR AGENCY RECORD                    
         MVC   KEY+19(6),SPACES    SPACE FILL AGY PORTION                       
         LR    RF,R3               SET A(FIELD IN PROCESS)                      
         LA    RF,8(RF)            SET TO DATA IN FIELD                         
         ZIC   R0,5(R3)            SET COUNT FOR LOOP - USE FIELD LEN           
         LA    RE,KEY+19           SET A(KEY FIELD)                             
AGYC0220 EQU   *                                                                
         CLI   0(RF),C'-'          SEPARATOR ENCOUNTERED?                       
         BNE   AGYC0240            NO  - MOVE CHARACTER                         
         LA    RF,1(RF)            YES - SKIP SEPARATOR                         
         LA    RE,KEY+23           NEXT CHARS MUST BE OFFICE                    
*                                                                               
*    NOTE:  FIELD MAY RUN OVER ACTUAL KEY.  THIS SHOULDN'T BE                   
*        A PROBLEM, AND IT SHOULD BE CLEANED UP WITHIN KEY BUILDING             
*                                                                               
AGYC0240 EQU   *                                                                
         MVC   0(1,RE),0(RF)       MOVE CHARACTER TO KEY                        
         LA    RF,1(RF)            BUMP TO NEXT POSITION                        
         LA    RE,1(RE)            BUMP TO NEXT POSITION                        
         BCT   R0,AGYC0220         GO BACK FOR NEXT CHARACTER                   
*                                  ALL CHARS PROCESSED                          
         MVC   KEY+25(2),TWAAGY    INSERT TARGET REP CODE                       
         MVC   CHECKKEY,KEY        SAVE EXTRA KEY COPY                          
         GOTO1 HIGH                READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   AGYC0740            NO  - KEY INVALID AS ENTERED                 
         CLC   CHECKKEY+23(2),SPACES                                            
*                                  KEY CONTAIN OFFICE?                          
         BNE   AGYC0280            YES - CAN USE KEY AS ENTERED                 
*                                  NO  - DOES IT NEED OFFICE?                   
         MVC   ETNAGYOF,KEY+19     INSERT CODE INTO TABLE AS RECEIVED           
AGYC0260 EQU   *                                                                
         GOTO1 SEQ                 READ NEXT KEY                                
         CLC   KEY(23),KEYSAVE     SAME RECORD TYPE/AGY CODE?                   
         BNE   AGYC0300            NO  - DOESN'T NEED OFFICE -                  
*                                     ALREADY IN TABLE                          
         CLC   KEY+25(2),TWAAGY    YES - SAME REP?                              
         BNE   AGYC0260            NO  - CHECK NEXT AGENCY RECORD               
         B     AGYC0760            YES - NEEDS AGENCY/OFFICE                    
AGYC0280 EQU   *                                                                
         MVC   ETNAGYOF,KEY+19     INSERT CODE INTO TABLE                       
AGYC0300 EQU   *                                                                
         TM    ETCDEFLG,AGONFILE   CODE ALREADY ON FILE?                        
         BO    AGYC0800            YES - DON'T SET 'ADD IT'                     
         OI    ETCDEFLG,ADDAGY     NO  - SET 'ADD IT'                           
         B     AGYC0800                                                         
AGYC0720 EQU   *                                                                
         MVC   RERROR,=AL2(001)    FIELD REQUIRED/MISSING                       
         B     AGYC0780                                                         
AGYC0740 EQU   *                                                                
         MVC   RERROR,=AL2(002)    FIELD INVALID AS ENTERED                     
         B     AGYC0780                                                         
AGYC0760 EQU   *                                                                
         MVC   RERROR,=AL2(715)    FIELD REQUIRES AGENCY OFFICE                 
         B     AGYC0780                                                         
AGYC0780 EQU   *                                                                
         ST    R3,DUB              SET A(ERROR FIELD)                           
         LTR   RB,RB               SET CC NOT ZERO                              
         B     AGYC0820            EXIT WITH ERROR                              
AGYC0800 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
AGYC0820 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   ADVCHECK:  VALIDATE THE NEW ADVERT FIELD.  IF NOT PRESENT,                  
*        ATTEMPT TO FILL IT FROM OTHER INFORMATION ON THE SCREEN.               
*        R2 ->  A(EQUIVALENCY TABLE ENTRY FOR CONTRACT)                         
*        R3 ->  ADVERT CODE FIELD IN QUESTION                                   
*                                                                               
ADVCHECK NTR1                                                                   
*                                                                               
         LR    R5,R3               SET A(AGENCY FIELD HEADER)                   
         CLI   8(R5),C'='          REQUEST FOR BROWSER CALL?                    
         BNE   ADVC0020            NO                                           
**       MVC   DMCB+20(1),SRCEUTL                                               
**       MVC   DMCB+21(1),ORIGUTL                                               
**       MVC   DMCB+22(2),SRCEREP                                               
         GOTOX (RFBROWSE,VREPFACS),DMCB,ACOMFACS,BASERD,(R5),0,        +        
               (0,C' ADV'),0                                                    
**       GOTO1 =V(REBROWSE),DMCB,ACOMFACS,BASERD,(R5),0,(0,C' ADV'),0,+         
**             RR=Y                                                             
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
ADVC0020 EQU   *                                                                
                                                                                
         TM    4(R3),X'20'         FIELD PREVIOUSLY VALID?                      
         BO    ADVC0800            YES - EXIT CC ZERO                           
         CLI   5(R3),0             ANYTHING ON LINE?                            
         BNE   ADVC0200            YES                                          
         L     R4,AEQUITBL         SET A(EQUIVALENCY TABLE)                     
ADVC0040 EQU   *                                                                
         CLC   ETOADVRT,ETOADVRT-EQUITABL(R4)                                   
*                                  ORIGINAL LINE ADVERT = TBL ENTRY?            
         BNE   ADVC0050            NO  - BUMP TO NEXT ENTRY                     
         OC    ETNADVRT-EQUITABL(4,R4),ETNADVRT-EQUITABL(R4)                    
*                                  YES - NEW CODE FOR TABLE ENTRY?              
         BZ    ADVC0050            NO  - CAN'T USE IT                           
         CR    R4,R2               YES - TABLE ENTRY ALREADY PROCESSED?         
         BNL   ADVC0720            NO  - CONSIDER ITEM NOT ENTERED              
         TM    ETFLAG,X'80'        YES - TABLE ENTRY SELECTED?                  
         BNE   ADVC0060            NO  -                                        
         MVC   ETNADVRT,ETNADVRT-EQUITABL(R4)                                   
*                                  YES - ALREADY VALIDATED -                    
*                                     USE ITS CODE FOR THIS LINE                
         MVC   8(4,R3),ETNADVRT    INSERT INTO SCREEN OUTPUT                    
         FOUT  (R3)                SET TO TRANSMIT                              
         OI    4(R3),X'20'         SET FIELD PREV VALID                         
         B     ADVC0800            EXIT CC ZERO                                 
ADVC0050 EQU   *                                                                
         LA    R4,LEQUITBL(R4)     BUMP TO NEXT TABLE ENTRY                     
         OC    0(12,R4),0(R4)      ANY ENTRY?                                   
         BZ    ADVC0720            NO  - TREAT AS 'NO FIND'                     
         B     ADVC0040            YES - GO BACK AND CHECK IT                   
ADVC0060 EQU   *                                                                
*                                  TABLE ENTRY NOT SELECTED:                    
*                                     CODE MUST HAVE BEEN ON FILE -             
*    NOTE:  SCREEN FIELD MAY HAVE BEEN CHANGED ON UNSELECTED LINE               
*        HOWEVER, ON-FILE VALUE OF CODE IS IN TABLE, AND                        
*        WILL BE USED.                                                          
*                                                                               
         TM    ETCDEFLG,X'20'      ADVERT CODE ON FILE?                         
         BO    ADVC0780            NO  - EXIT CC NOT ZERO                       
         MVC   ETNADVRT,ETNADVRT-EQUITABL(R4)                                   
*                                  YES - ON-FILE CODE FOR ADVERT-               
*                                     USE ITS CODE FOR THIS LINE                
         MVC   8(4,R3),ETNADVRT    INSERT INTO SCREEN OUTPUT                    
         FOUT  (R3)                SET TO TRANSMIT                              
         OI    4(R3),X'20'         SET FIELD PREV VALID                         
         B     ADVC0800            EXIT CC ZERO                                 
ADVC0200 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'08'           SET KEY FOR AGENCY RECORD                    
         MVC   KEY+21(4),SPACES    SPACE FILL ADVERT PORTION                    
         ZIC   RF,5(R3)            GET L(FIELD IN PROCESS)                      
         BCTR  RF,0                MINUS 1 FOR EX STATEMENT                     
         EX    RF,ADVC0210         MOVE FIELD TO KEY BY LENGTH                  
         B     ADVC0220                                                         
ADVC0210 EQU   *                                                                
         MVC   KEY+21(0),8(R3)     INSERT KEY BY LENGTH                         
ADVC0220 EQU   *                                                                
         MVC   KEY+25(2),TWAAGY    INSERT TARGET REP CODE                       
         GOTO1 HIGH                READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   ADVC0740            NO  - KEY INVALID AS ENTERED                 
         MVC   ETNADVRT,KEY+21     INSERT CODE INTO TABLE                       
         TM    ETCDEFLG,ADONFILE   CODE ALREADY ON FILE?                        
         BO    ADVC0800            YES - DON'T SET 'ADD IT'                     
         OI    ETCDEFLG,ADDADV     NO  - SET 'ADD IT'                           
         B     ADVC0800                                                         
ADVC0720 EQU   *                                                                
         MVC   RERROR,=AL2(001)    FIELD REQUIRED/MISSING                       
         B     ADVC0780                                                         
ADVC0740 EQU   *                                                                
         MVC   RERROR,=AL2(002)    FIELD INVALID AS ENTERED                     
         B     ADVC0780                                                         
ADVC0780 EQU   *                                                                
         ST    R3,DUB              SET A(ERROR FIELD)                           
         LTR   RB,RB               SET CC NOT ZERO                              
         B     ADVC0820            EXIT WITH ERROR                              
ADVC0800 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
ADVC0820 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SALCHECK:  VALIDATE THE NEW S/P FIELD.  IF NOT PRESENT,                     
*        ATTEMPT TO FILL IT FROM OTHER INFORMATION ON THE SCREEN.               
*        R2 ->  A(EQUIVALENCY TABLE ENTRY FOR CONTRACT)                         
*        R3 ->  S/P CODE FIELD IN QUESTION                                      
*                                                                               
SALCHECK NTR1                                                                   
         OC    SAVESALP,SAVESALP   ANY OVERRIDE S/P CODE?                       
         BZ    SALC0020            NO                                           
         MVC   ETNSALEP,SAVESALP   YES - OVERRIDE TABLE ENTRY WITH              
*                                     DEFAULT CODE                              
         MVC   ETNSPOFF,SAVESPOF   OVERRIDE OFFICE WITH DEFAULT CODE            
         MVC   ETNSPTEM,SAVESPTM   OVERRIDE TEAM   WITH DEFAULT CODE            
         MVC   8(3,R3),ETNSALEP    INSERT INTO SCREEN OUTPUT                    
         FOUT  (R3)                SET TO TRANSMIT                              
         OI    4(R3),X'20'         SET FIELD PREV VALID                         
         B     SALC0800            EXIT CC ZERO                                 
SALC0020 EQU   *                                                                
         TM    4(R3),X'20'         FIELD PREVIOUSLY VALID?                      
         BO    SALC0800            YES - EXIT CC ZERO                           
         CLI   5(R3),0             ANYTHING ON LINE?                            
         BNE   SALC0200            YES                                          
         L     R4,AEQUITBL         SET A(EQUIVALENCY TABLE)                     
SALC0040 EQU   *                                                                
         CLC   ETOSALEP,ETOSALEP-EQUITABL(R4)                                   
*                                  ORIGINAL LINE ADVERT = TBL ENTRY?            
         BNE   SALC0050            NO  - BUMP TO NEXT ENTRY                     
         OC    ETNSALEP-EQUITABL(3,R4),ETNSALEP-EQUITABL(R4)                    
*                                  YES - NEW CODE FOR TABLE ENTRY?              
         BZ    SALC0050            NO  - CAN'T USE IT                           
         CR    R4,R2               YES - TABLE ENTRY ALREADY PROCESSED?         
         BNL   SALC0720            NO  - CONSIDER ITEM NOT ENTERED              
         TM    ETFLAG,X'80'        YES - TABLE ENTRY SELECTED?                  
         BNE   SALC0060            NO  -                                        
         MVC   ETNSALEP,ETNSALEP-EQUITABL(R4)                                   
*                                  YES - ALREADY VALIDATED -                    
*                                     USE ITS CODE FOR THIS LINE                
         MVC   ETNSPOFF,ETNSPOFF-EQUITABL(R4)                                   
*                                  USE OFFICE CODE FOR THIS LINE                
         MVC   ETNSPTEM,ETNSPTEM-EQUITABL(R4)                                   
*                                  USE TEAM   CODE FOR THIS LINE                
         MVC   8(3,R3),ETNSALEP    INSERT INTO SCREEN OUTPUT                    
         FOUT  (R3)                SET TO TRANSMIT                              
         OI    4(R3),X'20'         SET FIELD PREV VALID                         
         B     SALC0800            EXIT CC ZERO                                 
SALC0050 EQU   *                                                                
         LA    R4,LEQUITBL(R4)     BUMP TO NEXT TABLE ENTRY                     
         OC    0(12,R4),0(R4)      ANY ENTRY?                                   
         BZ    SALC0720            NO  - TREAT AS 'NO FIND'                     
         B     SALC0040            YES - GO BACK AND CHECK IT                   
SALC0060 EQU   *                                                                
*                                  TABLE ENTRY NOT SELECTED:                    
*                                     CODE MUST HAVE BEEN ON FILE -             
*    NOTE:  SCREEN FIELD MAY HAVE BEEN CHANGED ON UNSELECTED LINE               
*        HOWEVER, ON-FILE VALUE OF CODE IS IN TABLE, AND                        
*        WILL BE USED.                                                          
*                                                                               
         TM    ETCDEFLG,SPONFILE   SALESP CODE ON FILE?                         
         BO    SALC0780            NO  - EXIT CC NOT ZERO                       
         MVC   ETNSALEP,ETNSALEP-EQUITABL(R4)                                   
*                                  YES - ON-FILE CODE FOR SALESP-               
*                                     USE ITS CODE FOR THIS LINE                
         MVC   ETNSPOFF,ETNSPOFF-EQUITABL(R4)                                   
*                                  USE OFFICE CODE FOR THIS LINE                
         MVC   ETNSPTEM,ETNSPTEM-EQUITABL(R4)                                   
*                                  USE TEAM   CODE FOR THIS LINE                
         MVC   8(3,R3),ETNSALEP    INSERT INTO SCREEN OUTPUT                    
         FOUT  (R3)                SET TO TRANSMIT                              
         OI    4(R3),X'20'         SET FIELD PREV VALID                         
         B     SALC0800            EXIT CC ZERO                                 
SALC0200 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'           SET KEY FOR SALESPERSON RECORD               
         MVC   KEY+22(2),TWAAGY    INSERT TARGET REP CODE                       
         MVC   KEY+24(3),SPACES    SPACE FILL SALESPERSON PORTION               
         ZIC   RF,5(R3)            GET L(FIELD IN PROCESS)                      
         BCTR  RF,0                MINUS 1 FOR EX STATEMENT                     
         EX    RF,SALC0220         MOVE FIELD TO KEY BY LENGTH                  
         B     SALC0240                                                         
SALC0220 EQU   *                                                                
         MVC   KEY+24(0),8(R3)     INSERT KEY BY LENGTH                         
SALC0240 EQU   *                                                                
         GOTO1 HIGH                READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   SALC0740            NO  - KEY INVALID AS ENTERED                 
         MVC   SAVEAIO,AIO         SAVE ORIGINAL AIO                            
         MVC   AIO,AIO2                                                         
         L     R6,AIO2             USE SECOND IO AREA FOR TKO                   
         USING RSALREC,R6          RETRIEVE S/P REC INTO IO AREA 2              
*                                                                               
         GOTO1 GETREC,DMCB,RSALREC                                              
*                                                                               
         MVC   AIO,SAVEAIO         RESET ORIGINAL AIO                           
*                                                                               
*                                                                               
         OC    RSALLEAV,RSALLEAV   S/P LEAVE DATE?                              
         BNZ   SALC0770            EXIT ERROR                                   
SALC0250 EQU   *                                                                
         OC    OFFTEAMS,OFFTEAMS   ANY OFFICE/TEAMS?                            
         BZ    SALC0320            NO  - USE AS IS                              
         LA    R4,15               15 SETS MAXIMUM                              
         LA    R5,OFFTEAMS                                                      
*                                                                               
SALC0260 EQU   *                                                                
         CLC   RSALOFF,0(R5)       OFFICE IN LIST?                              
         BE    SALC0280            YES                                          
         LA    R5,4(R5)            BUMP TO NEXT ENTRY                           
         BCT   R4,SALC0260                                                      
         B     SALC0320            OFFICE NOT IN LIST - USE                     
*                                                                               
*                                  OFFICE MAY BE IN LIST MORE                   
*                                     THAN ONE TIME                             
*                                                                               
SALC0280 DS    0H                  OFFICE IS IN OFFTEAM LIST,                   
         CLC   RSALOFF,0(R5)       OFFICE IN LIST?                              
         BNE   SALC0300            NO                                           
         CLC   RSALTEAM,2(R5)      YES - TEAM FOR THIS OFFICE?                  
         BE    SALC0320            YES - USE                                    
SALC0300 LA    R5,4(R5)            NO  - BUMP TO NEXT OFFICE                    
         BCT   R4,SALC0280         GO BACK FOR NEXT OFF/TEAM                    
         B     SALC0760                                                         
SALC0320 EQU   *                   MATCHUP:  USE IT                             
         MVC   ETNSALEP,KEY+24     INSERT CODE INTO TABLE                       
         MVC   ETNSPOFF,RSALOFF    INSERT S/P OFFICE INTO TABLE                 
         MVC   ETNSPTEM,RSALTEAM   INSERT S/P TEAM   INTO TABLE                 
*                                                                               
         DROP  R6                                                               
*                                                                               
         TM    ETCDEFLG,SPONFILE   CODE ALREADY ON FILE?                        
         BO    SALC0800            YES - DON'T SET 'ADD IT'                     
         OI    ETCDEFLG,ADDSP      NO  - SET 'ADD IT'                           
         B     SALC0800                                                         
SALC0720 EQU   *                                                                
         MVC   RERROR,=AL2(001)    FIELD REQUIRED/MISSING                       
         B     SALC0780                                                         
SALC0740 EQU   *                                                                
         MVC   RERROR,=AL2(002)    FIELD INVALID AS ENTERED                     
         B     SALC0780                                                         
SALC0760 EQU   *                                                                
         MVC   RERROR,=AL2(288)    OFFICE/TEAM MISMATCH                         
         B     SALC0780                                                         
SALC0770 EQU   *                                                                
         MVC   RERROR,=AL2(843)    S/P INACTIVE                                 
         B     SALC0780                                                         
SALC0780 EQU   *                                                                
         ST    R3,DUB              SET A(ERROR FIELD)                           
         LTR   RB,RB               SET CC NOT ZERO                              
         B     SALC0820            EXIT WITH ERROR                              
SALC0800 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
SALC0820 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   TYPCHECK:  VALIDATE THE NEW CONTRACT TYPE FIELD.  IF NOT PRESENT,           
*        ATTEMPT TO FILL IT FROM OTHER INFORMATION ON THE SCREEN.               
*        R2 ->  A(EQUIVALENCY TABLE ENTRY FOR CONTRACT)                         
*        R3 ->  C/T CODE FIELD IN QUESTION                                      
*                                                                               
*    NEED SOME RULES FOR HANDLING CONTRACT TYPE HERE!!  DOES A TYPE             
*        HAVE TO ENTERED IN ALL CASES?  IF NOT, WHAT ARE CONDITIONS             
*        IN WHICH IT MUST BE ENTERED?                                           
*                                                                               
TYPCHECK NTR1                                                                   
         OC    SAVECTYP,SAVECTYP   ANY OVERRIDE S/P CODE?                       
         BZ    TYPC0020            NO                                           
         MVC   ETNCNTYP,SAVECTYP   YES - OVERRIDE TABLE ENTRY WITH              
*                                     DEFAULT CODE                              
         MVC   8(1,R3),ETNCNTYP    INSERT INTO SCREEN OUTPUT                    
         FOUT  (R3)                SET TO TRANSMIT                              
         OI    4(R3),X'20'         SET FIELD PREV VALID                         
         B     TYPC0800            EXIT CC ZERO                                 
TYPC0020 EQU   *                                                                
                                                                                
         TM    4(R3),X'20'         FIELD PREVIOUSLY VALID?                      
         BO    TYPC0800            YES - EXIT CC ZERO                           
         CLI   5(R3),0             ANYTHING ON LINE?                            
         BNE   TYPC0200            YES                                          
         L     R4,AEQUITBL         SET A(EQUIVALENCY TABLE)                     
TYPC0040 EQU   *                                                                
         CLC   ETOCNTYP,ETOCNTYP-EQUITABL(R4)                                   
*                                  ORIGINAL LINE CONTYP = TBL ENTRY?            
         BNE   TYPC0050            NO  - BUMP TO NEXT ENTRY                     
         OC    ETNCNTYP-EQUITABL(3,R4),ETNCNTYP-EQUITABL(R4)                    
*                                  YES - NEW CODE FOR TABLE ENTRY?              
         BZ    TYPC0050            NO  - CAN'T USE IT                           
         CR    R4,R2               YES - TABLE ENTRY ALREADY PROCESSED?         
         BNL   TYPC0720            NO  - CONSIDER ITEM NOT ENTERED              
         TM    ETFLAG,X'80'        YES - TABLE ENTRY SELECTED?                  
         BNE   TYPC0060            NO  -                                        
         MVC   ETNCNTYP,ETNCNTYP-EQUITABL(R4)                                   
*                                  YES - ALREADY VALIDATED -                    
*                                     USE ITS CODE FOR THIS LINE                
         MVC   8(1,R3),ETNCNTYP    INSERT INTO SCREEN OUTPUT                    
         FOUT  (R3)                SET TO TRANSMIT                              
         OI    4(R3),X'20'         SET FIELD PREV VALID                         
         B     TYPC0800            EXIT CC ZERO                                 
TYPC0050 EQU   *                                                                
         LA    R4,LEQUITBL(R4)     BUMP TO NEXT TABLE ENTRY                     
         OC    0(12,R4),0(R4)      ANY ENTRY?                                   
         BZ    TYPC0720            NO  - TREAT AS 'NO FIND'                     
         B     TYPC0040            YES - GO BACK AND CHECK IT                   
TYPC0060 EQU   *                                                                
*                                  TABLE ENTRY NOT SELECTED:                    
*                                     CODE MUST HAVE BEEN ON FILE -             
*    NOTE:  SCREEN FIELD MAY HAVE BEEN CHANGED ON UNSELECTED LINE               
*        HOWEVER, ON-FILE VALUE OF CODE IS IN TABLE, AND                        
*        WILL BE USED.                                                          
*                                                                               
         TM    ETCDEFLG,ADDSP      CONTYP CODE ON FILE?                         
         BO    TYPC0720            NO  - EXIT CC NOT ZERO                       
         MVC   ETNCNTYP,ETNCNTYP-EQUITABL(R4)                                   
*                                  YES - ON-FILE CODE FOR CONTYP -              
*                                     USE ITS CODE FOR THIS LINE                
         MVC   8(1,R3),ETNCNTYP    INSERT INTO SCREEN OUTPUT                    
         FOUT  (R3)                SET TO TRANSMIT                              
         OI    4(R3),X'20'         SET FIELD PREV VALID                         
         B     TYPC0800            EXIT CC ZERO                                 
TYPC0200 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'32'           SET KEY FOR CONTRACT TYPE RECORD             
         MVC   KEY+24(2),TWAAGY    INSERT TARGET REP CODE                       
         MVC   KEY+26(1),8(R3)     INSERT CONTRACT TYPE                         
TYPC0220 EQU   *                                                                
         GOTO1 HIGH                READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   TYPC0740            NO  - KEY INVALID AS ENTERED                 
         MVC   ETNCNTYP,KEY+26     INSERT CODE INTO TABLE                       
         TM    ETCDEFLG,CTONFILE   CODE ALREADY ON FILE?                        
         BO    TYPC0800            YES - DON'T SET 'ADD IT'                     
         OI    ETCDEFLG,ADDCT      NO  - SET 'ADD IT'                           
         B     TYPC0800                                                         
TYPC0720 EQU   *                                                                
         LA    RF,CTOLAST          SET A(EQUIVALENCY TABLE)                     
         A     RF,=F'11000'        DISPLACE TO CONTROL INFO IN TWA              
*                                     DETAILING WHERE TO PLACE RETURN           
         USING MISFLGS,RF                                                       
         TM    CNTGPBIT+CNTVTYPB,CNTVTYPA                                       
*                                  CONTRACT TYPE REQUIRED?                      
         BZ    TYPC0800            NO                                           
*                                  YES - SEND OUT ERROR MESSAGE                 
         DROP  RF                                                               
         MVC   RERROR,=AL2(571)    CONTRACT TYPE REQUIRED                       
         B     TYPC0780                                                         
TYPC0740 EQU   *                                                                
         MVC   RERROR,=AL2(002)    FIELD INVALID AS ENTERED                     
         B     TYPC0780                                                         
TYPC0780 EQU   *                                                                
         ST    R3,DUB              SET A(ERROR FIELD)                           
         LTR   RB,RB               SET CC NOT ZERO                              
         B     TYPC0820            EXIT WITH ERROR                              
TYPC0800 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
TYPC0820 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
         DROP  R2                                                               
*                                                                               
*   SETFIL:    SWITCHES TO SOURCE REP FILE, IF NECESSARY.                       
*                                                                               
SETFIL   NMOD1 0,*SFIL*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLC   SRCEUTL,ORIGUTL     SOURCE/TARGET ON SAME FILE?                  
         BE    SETF0040            YES - DON'T SWITCH                           
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CSWITCH                                                       
         DROP  RE                                                               
         GOTO1 (RF),DMCB,(SRCEUTL,X'FFFFFFFF'),0                                
         CLI   4(R1),0             SWITCHED OKAY?                               
         BE    SETF0040            YES - CONTINUE TO PROCESS                    
         CLI   4(R1),2             NO  - SYSTEM NOT OPENED?                     
         BE    *+6                 YES                                          
         DC    H'0'                NO  - OTHER REASON                           
*                                                                               
*                                  ERROR: SWITCH BACK TO TARGET REP             
*        GOTO1 (RF),DMCB,(X'08',X'FFFFFFFF'),0                                  
         GOTO1 (RF),DMCB,=C'REP ',0                                             
         CLI   4(R1),0             SWITCHED BACK OKAY?                          
*                                                                               
*   MUST EXIT WITH ERROR:  HOW TO INTERFACE?                                    
*                                                                               
****>>>  BE    SRCCLOSD            YES - EXIT WITH MESSAGE                      
         DC    H'0'                NO  - ABORT                                  
*                                                                               
SETF0040 EQU   *                                                                
         XIT1                      SWITCH FINISHED                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   SETEQUIV:  SCAN THE EQUIVALENCY TABLE.  FOR EACH ENTRY NOT FLAGGED          
*        AS 'ALREADY ON FILE,' GENERATE A 1F RECORD.                            
*                                                                               
SETEQUIV NMOD1 0,*SEQV*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         MVC   AIO,AIO2                                                         
         L     R3,AIO2             USE SECOND IO AREA FOR TKO                   
         USING TKO,R3                                                           
         XC    RTKOREC(200),RTKOREC                                             
         MVC   RTKOLEN,=X'003C'    SET RECORD LENGTH = 60 CHARS                 
*                                                                               
         L     R6,AEQUITBL         SET A(EQUIVALENCY TABLE)                     
         USING EQUITABL,R6                                                      
SEQQ0020 EQU   *                                                                
         OC    ETOAGYOF,ETOAGYOF   ANY ENTRY ON LINE?                           
         BZ    SEQQ0800            NO  - FINISHED                               
         TM    ETFLAG,X'80'        LINE SELECTED?                               
         BNO   SEQQ0400            NO  - SKIP IT                                
         TM    ETFLAG,X'20'        YES - LINE ALREADY SCANNED?                  
         BO    SEQQ0400            YES - SKIP IT                                
         XC    RTKOKEY,RTKOKEY     CLEAR ENTIRE KEY                             
*                                  SET UP GENERIC INFO IN RECORD                
         MVI   RTKOKTYP,X'1F'      INSERT RECORD TYPE                           
         MVC   RTKOKREP,TARGREP    INSERT TARGET REP CODE                       
         MVC   RTKOKORP,SRCEREP    INSERT SOURCE REP CODE                       
         MVC   RTKOELEM(2),=X'011A'                                             
*                                  SET UP 01 CODE/LEN=26 CHARS                  
         GOTO1 DATCON,DMCB,(5,WORK),(3,RTKODATE)                                
*                                  INSERT DATE OF ADDITION                      
         MVC   RTKOLUID,SAVELUID   INSERT LUID MAKING REQUEST                   
SEQQ0040 EQU   *                                                                
         TM    ETCDEFLG,AGONFILE   AGENCY CODE ON FILE?                         
         BO    SEQQ0060            YES - DON'T ADD AGAIN                        
         TM    ETCDEFLG,ADDAGY     ADD-CODE FLAG SET?                           
         BNO   SEQQ0060            NO  - DON'T ADD IT                           
         OI    ETCDEFLG,AGONFILE   YES - SET 'AGENCY CODE ON FILE'              
         MVI   RTKOKRTP,1          SET RECORD TYPE                              
         MVC   RTKOKCOD(6),ETOAGYOF                                             
*                                  INSERT SOURCE AGENCY/OFF CODES               
         MVC   RTKOEQIV(6),ETNAGYOF                                             
*                                  INSERT TARGET AGENCY/OFF CODES               
         XC    KEY,KEY                                                          
         MVC   KEY(27),RTKOKEY     SET KEY FOR ADDREC                           
*                                                                               
*   RECORD MAY NOT HAVE BEEN ON FILE AT START OF SCREEN, BUT                    
*        FIRST SCREEN USE MAY HAVE ADDED IT.  A SUBSEQUENT                      
*        ENTRY MAY TRY TO ADD IT AGAIN, IN WHICH CASE THE                       
*        PROGRAM ABORTS WITH DUPLICATE KEY CHECK.                               
*        PRE-READING THE KEY WILL ELIMINATE THE OCCURRENCE.                     
*                                                                               
         GOTO1 HIGH                SEE IF KEY ALREADY ON FILE                   
         CLC   KEY(27),KEYSAVE     KEY ALREADY ADDED?                           
         BE    SEQQ0050            YES                                          
         MVC   KEY(27),RTKOKEY     RESET KEY FOR ADDREC                         
         GOTO1 ADDREC                                                           
****     GOTO1 ADDREC,DMCB,RTKOREC                                              
SEQQ0050 EQU   *                                                                
         XC    RTKOKCOD(6),RTKOKCOD                                             
*                                  CLEAR SOURCE CODE                            
         XC    RTKOEQIV,RTKOEQIV   CLEAR TARGET CODE                            
SEQQ0060 EQU   *                                                                
         TM    ETCDEFLG,ADONFILE   ADVERT CODE ON FILE?                         
         BO    SEQQ0080            YES - DON'T ADD AGAIN                        
         TM    ETCDEFLG,ADDADV     ADD-CODE FLAG SET?                           
         BNO   SEQQ0080            NO  - DON'T ADD IT                           
         OI    ETCDEFLG,ADONFILE   YES - SET 'ADVERT CODE ON FILE'              
         MVI   RTKOKRTP,2          SET RECORD TYPE                              
         MVC   RTKOKCOD(4),ETOADVRT                                             
*                                  INSERT SOURCE ADVERTISER CODE                
         MVC   RTKOEQIV(4),ETNADVRT                                             
*                                  INSERT TARGET ADVERTISER CODE                
         XC    KEY,KEY                                                          
         MVC   KEY(27),RTKOKEY     SET KEY FOR ADDREC                           
         GOTO1 HIGH                SEE IF KEY ALREADY ON FILE                   
         CLC   KEY(27),KEYSAVE     KEY ALREADY ADDED?                           
         BE    SEQQ0070            YES                                          
         MVC   KEY(27),RTKOKEY     RESET KEY FOR ADDREC                         
         GOTO1 ADDREC,DMCB,RTKOREC                                              
SEQQ0070 EQU   *                                                                
         XC    RTKOKCOD(6),RTKOKCOD                                             
*                                  CLEAR SOURCE CODE                            
         XC    RTKOEQIV,RTKOEQIV   CLEAR TARGET CODE                            
SEQQ0080 EQU   *                                                                
         TM    ETCDEFLG,SPONFILE   S/P    CODE ON FILE?                         
         BO    SEQQ0100            YES - DON'T ADD AGAIN                        
         TM    ETCDEFLG,ADDSP      ADD-CODE FLAG SET?                           
         BNO   SEQQ0100            NO  - DON'T ADD IT                           
         OI    ETCDEFLG,SPONFILE   YES - SET 'S/P    CODE ON FILE'              
         MVI   RTKOKRTP,3          SET RECORD TYPE                              
         MVC   RTKOKCOD(3),ETOSALEP                                             
*                                  INSERT SOURCE SALESPERSON CODE               
         MVC   RTKOEQIV(3),ETNSALEP                                             
*                                  INSERT TARGET SALESPERSON CODE               
         MVC   RTKOEQIV+3(2),ETNSPOFF                                           
*                                  INSERT TARGET S/P OFFICE CODE                
         MVC   RTKOSPTM,ETNSPTEM                                                
*                                  INSERT TARGET S/P TEAM   CODE                
         XC    KEY,KEY                                                          
         MVC   KEY(27),RTKOKEY     SET KEY FOR ADDREC                           
         GOTO1 HIGH                SEE IF KEY ALREADY ON FILE                   
         CLC   KEY(27),KEYSAVE     KEY ALREADY ADDED?                           
         BE    SEQQ0090            YES                                          
         MVC   KEY(27),RTKOKEY     RESET KEY FOR ADDREC                         
         GOTO1 ADDREC,DMCB,RTKOREC                                              
SEQQ0090 EQU   *                                                                
         XC    RTKOKCOD(6),RTKOKCOD                                             
*                                  CLEAR SOURCE CODE                            
         XC    RTKOEQIV,RTKOEQIV   CLEAR TARGET CODE                            
SEQQ0100 EQU   *                                                                
         TM    ETCDEFLG,CTONFILE   CONTYP CODE ON FILE?                         
         BO    SEQQ0120            YES - DON'T ADD AGAIN                        
         TM    ETCDEFLG,ADDCT      ADD-CODE FLAG SET?                           
         BNO   SEQQ0120            NO  - DON'T ADD IT                           
         OI    ETCDEFLG,CTONFILE   YES - SET 'CONTYP CODE ON FILE'              
         MVI   RTKOKRTP,4          SET RECORD TYPE                              
         MVC   RTKOKCOD(1),ETOCNTYP                                             
*                                  INSERT SOURCE CONTYPE     CODE               
         MVC   RTKOEQIV(1),ETNCNTYP                                             
*                                  INSERT TARGET CONTYPE     CODE               
         XC    KEY,KEY                                                          
         MVC   KEY(27),RTKOKEY     SET KEY FOR ADDREC                           
         GOTO1 HIGH                SEE IF KEY ALREADY ON FILE                   
         CLC   KEY(27),KEYSAVE     KEY ALREADY ADDED?                           
         BE    SEQQ0110            YES                                          
         MVC   KEY(27),RTKOKEY     RESET KEY FOR ADDREC                         
         GOTO1 ADDREC,DMCB,RTKOREC                                              
SEQQ0110 EQU   *                                                                
         XC    RTKOKCOD(6),RTKOKCOD                                             
*                                  CLEAR SOURCE CODE                            
         XC    RTKOEQIV,RTKOEQIV   CLEAR TARGET CODE                            
SEQQ0120 EQU   *                                                                
*                                                                               
SEQQ0400 EQU   *                                                                
         LA    R6,LEQUITBL(R6)     BUMP TO NEXT TABLE ENTRY                     
         B     SEQQ0020            GO BACK FOR NEXT ENTRY                       
SEQQ0800 EQU   *                                                                
         MVC   AIO,AIO1            RESET A(PRIMARY IO AREA)                     
         XIT1                      FINISHED                                     
*                                                                               
         DROP  R3,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   RESETFIL:  SWITCHES TO TARGET REP FILE, IF NECESSARY.                       
*                                                                               
RESETFIL NMOD1 0,*RSFL*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLC   SRCEUTL,ORIGUTL     SOURCE/TARGET ON SAME FILE?                  
         BE    RSFL0080            YES - NO NEED TO SWITCH BACK                 
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CSWITCH                                                       
         DROP  RE                                                               
*        GOTO1 (RF),DMCB,(X'08',X'FFFFFFFF'),0                                  
         GOTO1 (RF),DMCB,=C'REP ',0                                             
         CLI   4(R1),0             SWITCHED BACK OKAY?                          
         BE    *+6                                                              
         DC    H'0'                NO  - ABORT                                  
RSFL0080 EQU   *                                                                
         XIT1                      SWITCH FINISHED                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE TO BUILD POINTER LIST IN P1                                           
*  P2 - 0=NEW POINTER                                                           
*       1=OLD POINTER                                                           
***********************************************************************         
PTRS     NMOD1 0,**PTRS**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R5,AIO              SET A(IO AREA FOR CONTRACT)                  
         USING RCONREC,R5                                                       
*                                                                               
         L     R2,4(R1)                                                         
         LR    RE,R2                                                            
         L     R3,8(R1)                                                         
         XCEF  (RE),500                                                         
* BUILD ACTIVE PTR                                                              
         MVI   0(R2),X'0C'                                                      
         MVC   02(02,R2),TWAAGY                                                 
         MVC   04(02,R2),RCONKGRP                                               
         MVC   06(05,R2),RCONKSTA                                               
         MVC   11(02,R2),RCONKOFF                                               
         MVC   13(04,R2),RCONKAGY                                               
         MVC   17(02,R2),RCONKAOF                                               
         MVC   19(04,R2),RCONKADV                                               
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
* CREATE PTR 1                                                                  
         MVI   0(R2),X'8C'                                                      
         MVC   21(02,R2),TWAAGY                                                 
         ZAP   DUB(5),=P'99999999'                                              
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
         SP    DUB(5),WORK(5)                                                   
         MVO   WORK(5),DUB(5)                                                   
         MVC   23(4,R2),WORK                                                    
         LA    R2,32(R2)                                                        
* CREATE PTR 2                                                                  
         MVI   0(R2),X'9C'                                                      
         MVC   02(02,R2),TWAAGY                                                 
         MVC   04(02,R2),RCONKOFF                                               
         MVC   06(02,R2),RCONKGRP                                               
         MVC   08(05,R2),RCONKSTA                                               
         MVC   13(04,R2),RCONKADV                                               
         MVC   17(04,R2),RCONKAGY                                               
         MVC   21(02,R2),RCONKAOF                                               
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
* CREATE PTR 3                                                                  
         MVI   0(R2),X'AC'                                                      
         MVC   01(2,R2),TWAAGY                                                  
         MVC   03(2,R2),RCONKOFF                                                
         MVC   05(2,R2),RCONTEM                                                 
* INVERT SALESMAN CODE FOR LAST NAME HIGH                                       
         LA    RE,RCONSAL+2        LAST INITIAL                                 
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,R0                                                            
         MVC   07(1,R2),0(RE)                                                   
         MVC   08(2,R2),RCONSAL                                                 
         CLI   RCONSAL+2,C' '      ONLY 2 INITIALS?                             
         BNE   *+8                                                              
         MVI   9(R2),C' '                                                       
         MVC   10(5,R2),RCONKSTA                                                
         MVC   15(4,R2),RCONKAGY                                                
         MVC   19(4,R2),RCONKADV                                                
         MVC   23(4,R2),RCONKCON                                                
         LA    R2,32(R2)                                                        
* CREATE PTR 4                                                                  
         MVI   0(R2),X'BC'                                                      
         MVC   02(02,R2),TWAAGY                                                 
         MVC   04(02,R2),RCONCTGY                                               
         MVC   06(02,R2),RCONKOFF                                               
         MVC   08(05,R2),RCONKSTA                                               
         MVC   13(04,R2),RCONKAGY                                               
         MVC   17(02,R2),RCONKAOF                                               
         MVC   19(04,R2),RCONKADV                                               
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
* CREATE PTR BF                                                                 
BFPTR    MVI   0(R2),X'BF'                                                      
         MVC   02(02,R2),TWAAGY                                                 
         MVC   04(06,R2),RCONKAGY                                               
         MVC   10(04,R2),RCONKADV                                               
         MVC   14(05,R2),RCONKSTA                                               
         MVC   19(02,R2),RCONKOFF                                               
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(2,21(R2))                            
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
* CREATE PTR 5                                                                  
         MVI   0(R2),X'CC'                                                      
         MVC   01(02,R2),TWAAGY                                                 
         MVC   03(05,R2),RCONKSTA                                               
         MVC   08(02,R2),RCONKOFF                                               
         MVC   10(02,R2),RCONTEM                                                
* INVERT SALESMAN                                                               
         LA    RE,RCONSAL+2                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,R0                                                            
         MVC   12(1,R2),0(RE)                                                   
         MVC   13(2,R2),RCONSAL                                                 
         CLI   RCONSAL+2,C' '                                                   
         BNE   *+8                                                              
         MVI   14(R2),C' '                                                      
*                                                                               
         MVC   15(04,R2),RCONKADV                                               
         MVC   19(04,R2),RCONKAGY                                               
         MVC   23(04,R2),RCONKCON                                               
         LA    R2,32(R2)                                                        
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   PT8D                NO BOP                                       
         SPACE 1                                                                
         USING RCONBPEL,R6                                                      
DCPRTR   MVI   0(R2),X'DC'         CREATE BOP POINTER FOR CHANGE                
         MVC   5(2,R2),TWAAGY                                                   
         MVC   7(4,R2),RCONKADV                                                 
         GOTO1 DATCON,DMCB,(5,WORK),(3,TODAY)                                   
*                                  INSERT DATE                                  
         MVC   11(3,R2),TODAY      NEW POINTER GETS TODAYS DATE                 
         B     DC20                ALWAYS AN 'ADD'                              
*                                                                               
*        CLC   CONACT(3),=C'ADD'                                                
*        BE    DC20                                                             
*        LTR   R3,R3               0=NEW POINTER                                
*        BZ    DC20                                                             
*        MVC   11(3,R2),RCONBPDT   OLD PTR NEEDS OLD BOP CHANGE DATE            
*    (IN ADPTRS, ONLY ADD NEW POINTER IF CHANGE IS OTHER THAN DATE)             
*                                                                               
DC20     MVC   14(4,R2),RCONBPRF                                                
         MVC   18(5,R2),RCONKSTA                                                
         MVC   23(4,R2),RCONKCON                                                
         LA    R2,32(R2)                                                        
         DROP  R6                                                               
*                                                                               
*   CREATE X'8D' POINTERS                                                       
*                                                                               
PT8D     EQU   *                                                                
                                                                                
* - GET FLIGHT START/END DATES AND SAVE IN WORK+40                              
         GOTO1 DATCON,DMCB,(3,RCONDATE),(2,WORK+40)    START DATE               
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(2,WORK+42)  END DATE                 
                                                                                
* - GET DEMO FROM BOP OR SAR ELEMENT AND SAVE IN WORK+45                        
* - LOOK FOR DEMO MARKED AS PRIMARY (X'40' IN 1ST BYTE)                         
* - IF NO DEMO MARKED AS PRIMARY, USE 1ST DEMO AS DEFAULT                       
*                                                                               
         XC    WORK+45(3),WORK+45                                               
         LA    R4,RCONELEM                                                      
PPC8DLP  CLI   0(R4),0                                                          
         BE    PPC8D00                                                          
         CLI   0(R4),X'12'         SAR ELEMENT                                  
         BE    PPC8DDD                                                          
         CLI   0(R4),X'10'         BOP ELEMENT                                  
         BE    PPC8DEE                                                          
         ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    PPC8D00                                                          
         AR    R4,R1                                                            
         B     PPC8DLP             GO TO LOOP                                   
         USING RSARCO,R4                                                        
PPC8DDD  LA    RE,RSARDEM            DEMO                                       
         LA    RF,8                                                             
         MVC   WORK+45(3),RSARDEM    DEMO                                       
PPC8DDE  TM    0(RE),X'40'         IS IT MARKED AS PRIMARY ?                    
         BO    PPC8DDF               YES                                        
         LA    RE,3(RE)                                                         
         MVC   WORK+45(3),0(RE)                                                 
         BCT   RF,PPC8DDE                                                       
         MVC   WORK+45(3),RSARDEM    NO/USE 1ST AS DEFAULT                      
PPC8DDF  NI    WORK+45,X'FF'-X'40'          CLEAR MAIN DEMO INDICATOR           
         B     PPC8D00                                                          
                                                                                
         USING RCONBPEL,R4                                                      
PPC8DEE  LA    RE,RCONBPDM+1                                                    
         LA    RF,6                                                             
         MVC   WORK+45(3),RCONBPDM+1                                            
PPC8DEF  TM    0(RE),X'40'              IS IT MARKED AS PRIMARY DEMO?           
         BO    PPC8DEG                  YES                                     
         LA    RE,3(RE)                 NO/BUMP TO NEXT DEMO                    
         MVC   WORK+45(3),0(RE)                                                 
         BCT   RF,PPC8DEF                                                       
         MVC   WORK+45(3),RCONBPDM+1     NO PRIMARY/USE 1ST AS DEFAULT          
PPC8DEG  NI    WORK+45,X'FF'-X'40'           CLEAR MAIN DEMO INDICATOR          
         B     PPC8D00                                                          
         DROP R4                                                                
                                                                                
* BUILD BASIC KEY IN WORK                                                       
PPC8D00  XC    WORK(32),WORK                                                    
         LA    R4,WORK                                                          
         MVI   0(R4),X'8D'                                                      
         MVC   1(2,R4),RCONKREP                                                 
         MVC   8(2,R4),WORK+40    START DATE                                    
         MVC   10(2,R4),WORK+42    END DATE                                     
         MVC   12(4,R4),RCONKCON   CONTRACT NUMBER                              
                                                                                
* ID = 1 = AGENCY / ADVERTISER                                                  
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),1                                                         
         MVC   17(6,R2),RCONKAGY       AGENCY(4) AND OFFICE(2)                  
         MVC   23(4,R2),RCONKADV       ADVERTISER                               
         LA    R2,32(R2)                                                        
                                                                                
* ID = 2 = SALESPERSON/CONTRACT TYPE/GROUP-SUBG/CATEGORY/TEAM                   
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),2                                                         
         MVC   17(3,R2),RCONSAL                                                 
         MVC   20(1,R2),RCONTYPE                                                
         MVC   21(2,R2),RCONKGRP                                                
         MVC   23(2,R2),RCONCTGY                                                
         MVC   25(2,R2),RCONTEM                                                 
         LA    R2,32(R2)                                                        
                                                                                
* ID = 3 = OFFICE(SALESPERSON)/DEMO/CREATION DATE                               
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),3                                                         
         MVC   17(2,R2),RCONKOFF      SALESPERSON OFFICE                        
* CREATION DATE YMD -> BINARY                                                   
         GOTO1 DATCON,DMCB,(3,RCONHDRD),(2,22(R2))                              
         MVC   19(3,R2),WORK+45       DEMO                                      
PPC8DX   LA    R2,32(R2)                                                        
*                                                                               
* END X'8D' PASSIVE POINTERS                                                    
*                                                                               
*   CREATE X'8E' POINTERS                                                       
*   SIMILAR TO X'8D' POINTERS BUT HAVE STATION IN KEY INSTEAD OF 0'S            
*                                                                               
* WORK HAS BASIC KEY- REPLACE ZEROS OF X'8D' WITH STATION                       
PPCON8E  MVI   WORK,X'8E'                                                       
         MVC   WORK+3(5),RCONKSTA                                               
                                                                                
* ID = 1 = AGENCY / ADVERTISER                                                  
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),1                                                         
         MVC   17(6,R2),RCONKAGY       AGENCY(4) AND OFFICE(2)                  
         MVC   23(4,R2),RCONKADV       ADVERTISER                               
         LA    R2,32(R2)                                                        
                                                                                
* ID = 2 = SALESPERSON/CONTRACT TYPE/GROUP-SUBG/CATEGORY/TEAM                   
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),2                                                         
         MVC   17(3,R2),RCONSAL                                                 
         MVC   20(1,R2),RCONTYPE                                                
         MVC   21(2,R2),RCONKGRP                                                
         MVC   23(2,R2),RCONCTGY                                                
         MVC   25(2,R2),RCONTEM                                                 
         LA    R2,32(R2)                                                        
                                                                                
* ID = 3 = OFFICE(SALESPERSON)/DEMO/CREATION DATE                               
         MVC   0(32,R2),WORK                                                    
         MVI   16(R2),3                                                         
         MVC   17(2,R2),RCONKOFF      SALESPERSON OFFICE                        
* CREATION DATE YMD -> BINARY                                                   
         GOTO1 DATCON,DMCB,(3,RCONHDRD),(2,22(R2))                              
         MVC   19(3,R2),WORK+45    DEMO                                         
PPC8EX   LA    R2,32(R2)                                                        
                                                                                
* END X'8E' PASSIVE POINTERS                                                    
*                                                                               
         EJECT                                                                  
* ADD X'9D' PASSIVE POINTER (RIS/PRODUCT)                                       
*                                                                               
         MVI   0(R2),X'9D'                                                      
         MVC   1(2,R2),RCONKREP                                                 
         MVC   3(5,R2),RCONKSTA                                                 
         MVC   8(4,R2),RCONKADV                                                 
         MVC   23(4,R2),RCONKCON                                                
* NOW SET PRODUCT NAME OR CODE INTO KEY                                         
         MVI   12(R2),X'FF'              SET DEFAULT TO LAST                    
         MVC   13(3,R2),RCONPRD                                                 
         LA    R6,RCONREC          NOW LOOK FOR PRODUCT NAME                    
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   PP9DX                                                            
         USING RCONEXEL,R6                                                      
         MVC   12(9,R2),RCONEXPR   SET PRODUCT NAME                             
         DROP  R6                                                               
PP9DX    LA    R2,32(R2)           9D POINTER CREATED, BUMP                     
*                                                                               
* END X'9D' PASSIVE POINTER                                                     
*                                                                               
* ADD X'AB01' PASSIVE POINTER (PRODUCT CODE) IF PRODUCT CODE PRESENT            
*                                                                               
         CLC   RCONPRD,=C'   '     ANY PRODUCT CODE?                            
         BNH   PRAB01X             NO  - DON'T ADD THIS KEY                     
         MVC   00(2,R2),=X'AB01'                                                
         MVC   10(2,R2),RCONKREP                                                
         MVC   12(4,R2),RCONKAGY                                                
         MVC   16(4,R2),RCONKADV                                                
         MVC   20(3,R2),RCONPRD                                                 
         MVC   23(4,R2),RCONKCON                                                
         LA    R2,32(R2)           9D POINTER CREATED, BUMP                     
PRAB01X  EQU   *                                                                
*                                                                               
* END X'9D' PASSIVE POINTER                                                     
*                                                                               
PTREC    EQU   *                                                                
         CLC   CONACT(3),=C'ADD'                                                
         BNE   PTRX                                                             
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   PTRX                NO SAR                                       
         SPACE 1                                                                
         USING RSAREL,R6                                                        
         MVI   0(R2),X'EC'                                                      
         MVC   21(2,R2),TWAAGY                                                  
         MVC   23(4,R2),TRGTCON                                                 
PTRX     DS    0H                                                               
         XMOD1                                                                  
         DROP  R5,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD POINTERS TO FILE                                               
*              P1=A(OLD PTR LIST)                                               
*              P2=A(NEW PTR LIST)                                               
*              P3=A(DISK ADDR)                                                  
***********************************************************************         
ADDPTRS  NMOD1 0,*ADDPTR*                                                       
         L     RC,0(R1)                                                         
         LM    R2,R4,4(R1)                                                      
*                                                                               
         MVI   DMOUTBTS,0          FOR PROPER RECOVERY                          
         OI    DMINBTS,X'08'       PASS DELETES                                 
AP25     CLC   0(27,R2),0(R3)      SAME?                                        
         BE    AP100                                                            
* DIFFERENT                                                                     
* DELETE OLD PTR                                                                
         CLI   0(R2),0             ADD?                                         
         BNE   AP30                                                             
* ADD                                                                           
         MVC   KEY,0(R3)           NEW KEY                                      
         B     AP50                                                             
* CHANGE                                                                        
AP30     MVC   KEY,0(R2)                                                        
         CLI   KEY,X'DC'           BOP POINTER ONLY CHANGED IF                  
         BNE   AP33                                                             
         CLC   0(11,R3),KEY        ADVERTISER OR                                
         BNE   AP33                                                             
         CLC   14(13,R3),KEY+14    REF #, STATION OR CON# CHANGES               
         BE    AP100                                                            
AP33     OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         BAS   RE,APCHECK                                                       
         CLC   KEY(27),KEYSAVE                                                  
         BNE   AP40                                                             
         MVI   KEY+27,X'FF'                                                     
         GOTO1 WRITE                                                            
         BAS   RE,APCHECK                                                       
* ADD NEW PTR                                                                   
AP40     MVC   KEY,0(R3)                                                        
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         BAS   RE,APCHECK                                                       
         CLC   KEY(27),KEYSAVE     KEY ALREADY THERE?                           
         BE    *+14                                                             
         MVC   KEY,KEYSAVE                                                      
         B     AP50                                                             
* UNDELETE OLD PTR                                                              
         MVI   KEY+27,0                                                         
         GOTO1 WRITE                                                            
         BAS   RE,APCHECK                                                       
         B     AP100                                                            
* ADD PTR                                                                       
AP50     MVI   KEY+27,0                                                         
         MVC   KEY+28(4),0(R4)     DISK ADDR                                    
         GOTO1 ADD                                                              
         BAS   RE,APCHECK                                                       
*                                                                               
* NEXT POINTER                                                                  
AP100    LA    R2,32(R2)                                                        
         LA    R3,32(R3)                                                        
         CLI   0(R3),0             LAST?                                        
         BNE   AP25                                                             
         MVI   DMOUTBTS,X'FD'                                                   
         B     EXXIT                                                            
*                                                                               
APCHECK  TM    DMCB+8,X'FD'        ALL BUT DELETE                               
         BCR   8,RE                                                             
         DC    H'0'                                                             
*                                                                               
EXXIT    XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
***>>>                                                                          
SUBROUT  DS    0H                                                               
         NMOD1 0,**SUBR**                                                       
         L     RC,0(R1)            SET A(WORKSPACE)                             
         L     R2,8(R1)            SET A(EQUIV TABLE ENTRY IN PROCESS)          
         L     R8,ASPOOLD                                                       
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         LA    R5,SYSSPARE                                                      
                                                                                
QGOCON   EQU   4                   GO AND SWAP TO CONTRACT                      
QCKGLOB  EQU   6                   CHECK IF GLOBBER CALLS                       
                                                                                
         CLI   4(R1),QGOCON                                                     
         BE    GOCONPGM                                                         
         CLI   4(R1),QCKGLOB                                                    
         BE    CKGLOB                                                           
         DC    H'0'                                                             
*                                                                               
SUBYES   SR    R1,R1                                                            
         B     *+8                                                              
SUBNO    LA    R1,1                SET CONDITION CODE AT EXIT                   
         LTR   R1,R1                                                            
SUBRX    XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* GO AND SWAP TO CONTRACT                                                       
***********************************************************************         
GOCONPGM DS    0H                                                               
         XC    BLOCK(256),BLOCK                                                 
         LA    R3,BLOCK                                                         
         USING GLVXFRSY,R3                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'SFM'    DARE PROGRAM                                 
         MVC   GLVXTOSY,=C'REP'    TO THE REP SYSTEM                            
         MVC   GLVXTOPR,=C'CON'    CONTRACT PROGRAM                             
****     OI    GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER               
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
         DROP  R3                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',BLOCK,24,GLVXCTL                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    BLOCK(256),BLOCK                                                 
         LA    R3,BLOCK                                                         
         USING GLCONNUM,R3                                                      
*                                                                               
         USING EQUITABL,R2                                                      
         GOTO1 HEXOUT,DMCB,ETNCONUM,GLCONNUM,4,=C'TOG'                          
         MVC   GLCONCA(3),=C'CF*'  PAPERWORK IF 'CONFIRMED ORDER'               
***>>>   MVC   GLCONCA(3),=C'PRI'  PRINT IF 'CONFIRMED ORDER'                   
         TM    ETFLAG,X'01'        'VERSION' IN PROCESS?                        
         BNO   GOCO0020            NO  - 'OFF' = CONFIRMED                      
         MVC   GLCONCA(4),=C'LAST' WKST IF 'VERSION ORDER'                      
GOCO0020 EQU   *                                                                
         MVI   GLCONFLG,X'80'      SET 'RETURN FROM CONTRACT'                   
*                                                                               
         DROP  R2,R3                                                            
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',BLOCK,GLCONLNQ,GLRKACT                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*              TRANSFER TO CONTRACT FROM HERE!!                                 
         XMOD1 2                   EXIT ALL THE WAY OUT                         
***********************************************************************         
* CHECK IF ANYTHING IN GLOBBER                                                  
***********************************************************************         
CKGLOB   DS    0H                                                               
         XC    BLOCK(256),BLOCK                                                 
*                                                                               
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'GETD',BLOCK,24,GLVXCTL                          
         TM    DMCB+8,X'10'        NO VARIABLES FOUND, SKIP                     
         BO    SUBNO                                                            
         GOTO1 (RF),DMCB,=C'DELE',,,GLVXCTL                                     
*                                                                               
         B     SUBYES                                                           
         DROP  R4                                                               
         EJECT                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'121RESFM42S  01/03/03'                                      
         END                                                                    
