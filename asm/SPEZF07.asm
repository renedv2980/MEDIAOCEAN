*          DATA SET SPEZF07    AT LEVEL 156 AS OF 11/08/17                      
*PHASE T23007A                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE: T23007 - EASI INVOICE LIST AND MAINT                        *         
*  COMMENTS: THIS PROGRAM LISTS INVOICES AND CAN OVERIDE CLIENT       *         
*            AND/OR PRODUCT FOR EACH INVOICE.                         *         
*                                                                     *         
*  OUTPUTS: UPDATED INVOICE BATCHES                                   *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - WORK REG                                              *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - WORK REG & POINTER TO INVOICE RECORD                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER, EZBLOCK         *         
*          R7 - SECOND BASE                                           *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPEZF00-T23000)         *         
*                  - IN AIO FROM HYPER CONTROLLER (T00A30)            *         
*                      - WORKER RECORD                                *         
*             AIO2/3   - EZBLOCK                                      *         
*             WRKFBUFR - WORKER BUFFER                                *         
*                      - INDEX RECORD                                 *         
*                                                                     *         
***********************************************************************         
*                                                                               
***********************************************************************         
*                                                                     *         
*  LEV 20-21 MAR28/90 DON'T PUT CONVERT DATE OUT, FIX DRH KEY EDITS   *         
*  LEV 22    APR25/90 DELETE INVOICES AND CK FOR DELETED INVOICES     *         
*  LEV 23    MAY03/90 DELETE INVOICES AND CK FOR DELETED INVOICES     *         
*  LEV 24    JUN06/90 FIX DISPLAY PRD-EST IN DRHOOK                   *         
*  LEV 25    JUN14/90 FIX R2 INIT FOR MISSING ERR                     *         
*  LEV 26    AUG09/90 ADD RESTORE FUNCTION                            *         
*  LEV 27    SEP11/90 FIX R2 FOR INVALID ACTION ERR MSG               *         
*  LEV 28    OCT01/90 DON'T ALLOW DELETE FOR CONVERTED INVOICES       *         
*  LEV 29    OCT11/90 ALLOW DELETE FOR NON-DD TERMINALS               *         
*  LEV 30-31 NOV05/90 FORCE REVALIDATE KEY AT END OF PRINTREP LIST    *         
*                     BYPASS FILES STATUS = KEEP UNLESS FILTER KEEP   *         
*  LEV 32    NOV19/90 FORCE BOMB IF CONVERT DATE ON WITHOUT STATUS BIT*         
*  LEV 33    DEC06/90 FORCE UNKEEP IF RECOVERT OR RESTORE REQUEST FIX *         
*  LEV 34    JAN18/91 CHANGE KEEP TO DONE                             *         
*  LEV 35    JAN29/91 MAKE DONE BOTH DEL AND CONV FOR KEEP AND NON-KEP*         
*  LEV 36    JAN29/91 SHOW D FOR DELETED INVOICE                      *         
*  LEV 37    JAN30/91 PUT IN BATCH DATE FILTER                        *         
*  LEV 38    FEB12/91 FIX MEDIA BUG FOR RADIO                         *         
*  LEV 39    FEB21/91 ADD MEDIA FILTER                                *         
*  LEV 40    MAR29/91 ADD MOS FILTER AND SPECIAL RATES                *         
*  LEV 41    APR12/91 ADD CK EST FOR SPECIAL RATE DEFAULT             *         
*  LEV 42    APR19/91 FIX BUG PRINTING O INVOICES HEADING/TOTALS      *         
*  LEV 43    MAY30/91 NO OP SPEC RATES FOR NOW                        *         
*  LEV 44    JUN03/91 FIX SPEC RATE BUG                               *         
*  LEV 45    JUN10/91 SHOW EST EVEN IF NO PRODUCT                     *         
*  LEV 46    JUN25/91 FORCE BATCHES WITH ALL DEL/CONV INV TO STAT CONV*         
*  LEV 47    AUG14/91 ADD NETWORK TO SYSTEM                           *         
*  LEV 48    SEP25/91 ADD DEL PREV INVOICE                            *         
*  LEV 49    OCT04/91 FIX FILTER DEL/CONV/DONE                        *         
*  LEV 50    NOV06/91 ADD EQUIV STA AND HAVE DEFUALT LIST UNCONV      *         
*  LEV 51    JAN24/92 FIX CONVERTED BATCH TESTING                     *         
*  LEV 52    JAN28/92 REMOVE UNCONV FROM FILTER HELP MSG, STOP        *         
*                     CHANGES TO CONVERTED INV, BUG UNDEL,RECONVERT   *         
*  LEV 53-54 FEB13/92 SET UP FOR MEDIAS C/S FIX KEEPSW                *         
*  LEV 55    FEB19/92 FIX MEDIA BUG IN LS124                          *         
*  LEV 56-57 FEB25/92 FIX MEDIA BUG IN INVSTA AND EDIT IN GMD1        *         
*  LEV 58    MAR03/92 DON'T CK SPECIAL RATE FOR VALIDATED             *         
*  LEV 59    MAR11/92 ADD FILTER UNCONV                               *         
*  LEV 60    APR06/92 FIX SET BATCH TO KEEP FOR MORE THAN 1 DELETE    *         
*  LEV 61    APR20/92 USE CATCHIOS TO SEE IF APPROACHING MAX          *         
*  LEV 62    MAY04/92 DON'T PRINT INV SEQ WITH COMMAS                 *         
*  LEV 63    AUG31/92 SHOW TV & NETWORK BATCHES FOR CANADIAN AGENCIES *         
*  LEV 64    OCT20/92 FIX NO DATA RECEIVED BUG                        *         
*  LEV 65    NOV17/92 SHOW EST ON LIST FUNCTION                       *         
*  LEV 66    APR04/93 ADD CABLE HEAD (EZIHNET)                        *         
*  LEV 67    APR19/93 CHGE FROM FACWK TO EASIWK                       *         
*  LEV 68    SEP23/93 FIX REPORT TO GET RID OF .O FOR CONVERTED       *         
*  LEV 69    OCT01/93 FIX NO PRODUCT FOUND TO UNKNOWN, NOT DUMP       *         
*  LEV 70    OCT07/93 ADD LC - LOCAL CABLE                            *         
*  LEV 71    APR05/94 EZIHCCMB CHECK FOR COMBINED INVOICE-RECONVERT   *         
*                     FIX VFTR USER=, FIX MAXIOS ERR MSG DUMP         *         
*  LEV 72    APR15/94 SHOW GROSS $, STOP BLK MOS FROM CONVERSION      *         
*  LEV 73    APR29/94 FIX DUMP WITH USER = ALL                        *         
*  LEV 74    JUN07/95 CONVERT TO WRKF FILES, NO MORE USER=ALL         *         
*  LEV 75    MAY22/96 FIX CK TWA FOR * FOR USER = OFFLINE             *         
*                     MAKE REC CT EDITS LARGER/SHOW ERR IF BAT NOT FND*         
*                     CK STATION TO BE ON MASTER FILE                 *         
*                     ALLOW ALL FOR INVOICE FILTER, SHOW TOTAL AT END *         
*  LEV 76    JUN11/96 FIX - READ RECORD INTO WRKFREC NOT AIO1         *         
*  LEV 77    AUG01/96 CHANGE EZBLOCK & PHASE NAME CK PERIOD           *         
*  LEV 78    AUG12/96 ADD OPTION ALL, IF INV= REQ SHOW REGARDLESS     *         
*                     DIS-ALLOW USER=ALL                              *         
*                     SHOW 5 DIGITS OF BATCH NUMBER                   *         
*  LEV 79    JUN16/97 FIX HEADINGS ON NOW REPORT                      *         
*  LEV 80    JUN24/97 STOP ACTION ADD                                 *         
*  LEV 81    AUG07/97 STOP SHOWING EST 0 INSTEAD OF CLT NAME          *         
*  LEV 82    OCT01/97 ALLOW LOW POWER TV STATIONS                     *         
*  LEV 83    SEP11/98 SPEED UP PROCESSING                             *         
*  LEV 84    OCT20/98 LARGER EDIT FOR TOTAL SPOTS                     *         
*                     COPIED STATUS BITS DEFINITION                   *         
*  LEV 85    NOV03/98 CHECK FOR MOS OF FFFF                           *         
*  LEV 86    NOV16/98 CHECK CATCHIO AFTER INDEX                       *         
*  LEV 87    FEB08/99 FIX BAD MOS IN KEY                              *         
*  LEV 88    OCT04/99 FIX EZPRINT WITH VPRINT                         *         
*  LEV 89    NOV02/99 ADD FILTER FOR MID FLIGHT CL, SOON FILTERS OKAY *         
*  LEV 90    NOV02/99 ADD FILTER FOR MID FLIGHT CL, SOON FILTERS OKAY *         
*  LEV 91    JAN27/00 FIX FTRBMOS                                     *         
*  LEV 92    JUN29/00 ALLOW DELETE FOR BAD MOS                        *         
*  LEV 93 SMUR AUG14/00 IF SPOT SET BAGYMD TO 01                      *         
*  LEV 94 BGRI SEP11/00 ERR MSG IF BAD MEDIA                          *         
*                                                                     *         
***********************************************************************         
         TITLE 'T23007 - INVOICE LIST'                                          
T23007   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**3007**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    RC,SVRC                                                          
         ST    R2,RELO                                                          
         MVC   AIO,AIO1                                                         
         MVI   QMED,C'T'                                                        
         MVI   IOOPT,C'Y'          USER DOING ALL I/O                           
         CLC   =A(WRKFEND-SYSD),LSYSD                                           
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,SETUP                                                         
         BNE   NOINV                                                            
*                                                                               
         L     R6,AIO2             SET EZBLOCK                                  
         USING EZBLOCKD,R6                                                      
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    XIT                                                              
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LIST                                                             
         CLI   MODE,RECADD         ADD RECORD                                   
         BE    INVAL                                                            
         CLI   MODE,RECDEL         DEL RECORD                                   
         BE    INVAL                                                            
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     XIT                                                              
NEQXIT   LTR   RB,RB                                                            
XIT      XIT1                                                                   
*                                                                               
EQXITRE  CR    RB,RB                                                            
         BR    RE                                                               
NEQXITRE LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*   VKEY - VALIDATE KEY                                               *         
***********************************************************************         
*                                                                               
VKEY     DS    0H                                                               
         CLI   ACTNUM,ACTSEL                                                    
         BNE   VK000                                                            
*                                                                               
         NI    DIVREPTH+1,X'FF'-X'0C'                                           
         OI    DIVREPTH+6,X'80'                                                 
         NI    DIVREPH+1,X'FF'-X'2C'                                            
         OI    DIVREPH+6,X'80'                                                  
*                                                                               
VK000    DS    0H                                                               
         CLI   RECNUM,X'12'        CONSUM                                       
         BE    *+12                                                             
         CLI   RECNUM,X'0F'        CONLIST                                      
         BNE   *+8                                                              
         MVI   USEIO,C'Y'                                                       
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKA                                                              
*                                                                               
         CLI   RECNUM,X'12'        CONSUM                                       
         BE    VK001                                                            
         CLI   RECNUM,X'0F'        CONLIST                                      
         BE    VK001                                                            
*                                                                               
         CLI   ACTNUM,ACTSEL                                                    
         BNE   INVAL                                                            
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    INVAL                                                            
         B     VKA                                                              
*                                                                               
VK001    DS    0H                                                               
         CLI   CALLSTCK,X'E1'                                                   
         BE    VKXIT                                                            
         CLI   CALLSTCK,X'DE'                                                   
         BE    VKXIT                                                            
         B     INVAL                                                            
*                                                                               
* IF NO FIELDS ARE CHANGED, JUST KEEP ON WITH DISPLAY *                         
*                                                                               
VKA      DS    0H                                                               
         TM    LINSTAH+4,X'20'     BATCH STATION                                
         BZ    VK010                                                            
         TM    LINBDTH+4,X'20'     BATCH DATE                                   
         BZ    VK010                                                            
         TM    LINBSQH+4,X'20'     BATCH SEQ                                    
         BZ    VK010                                                            
         TM    LINFTRH+4,X'20'     FILTERS                                      
         BO    VKXIT                                                            
*                                                                               
VK010    MVI   NEWDISP,C'N'                                                     
*                                                                               
         GOTO1 VALIMED             VALIMED SETS TO SPOT OR NET                  
*                                                                               
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         LA    R2,LINSTAH          STATION                                      
         XC    RQSTA,RQSTA                                                      
         CLI   5(R2),0                                                          
         BE    VK100                                                            
*                                                                               
         GOTO1 VALISTA                                                          
         CLC   RQSTA,QSTA                                                       
         BE    *+8                                                              
         MVI   NEWDISP,C'Y'                                                     
         MVC   RQSTA,QSTA                                                       
*                                                                               
VK100    OI    4(R2),X'20'                                                      
         LA    R2,LINBDTH          BATCH DATE                                   
         MVI   DTPLUS,C'N'                                                      
         CLI   5(R2),0             IF NO DATE                                   
         BNE   VK120                                                            
         MVI   NEWDISP,C'Y'        THEN NEW BATCH                               
         XC    RQDTE,RQDTE                                                      
         B     VK200                                                            
*                                                                               
VK120    LLC   RF,5(R2)               LOOK FOR FINAL +                          
         LA    RE,FHDRLEN-1(R2,RF)    POINT TO LAST CHAR                        
         CLI   0(RE),C'+'                                                       
         BNE   VK130                                                            
         MVI   0(RE),C' '                                                       
         BCTR  RF,0                DECREMENT LENGTH                             
         STC   RF,5(R2)                                                         
         MVI   DTPLUS,C'Y'         LOOK FOR THIS DATE OR LATER                  
*                                                                               
VK130    GOTO1 DATVAL,DMCB,(0,FHDRLEN(R2)),WORK                                 
         OC    DMCB,DMCB                                                        
         BZ    BADATE                                                           
         GOTO1 DATCON,(R1),(0,WORK),(2,DUB)                                     
         CLC   RQDTE,DUB           TEST SAME AS LAST                            
         BE    *+8                                                              
         MVI   NEWDISP,C'Y'                                                     
         MVC   RQDTE,DUB                                                        
*                                                                               
VK200    OI    4(R2),X'20'                                                      
         LA    R2,LINBSQH          SEQ                                          
         CLI   5(R2),0             ANY INPUT                                    
         BNE   VK210                                                            
         MVI   NEWDISP,C'Y'        NO, NEW BATCH                                
         XC    RQBSEQ,RQBSEQ                                                    
         B     VK300                                                            
*                                                                               
VK210    MVC   WORK(8),=8C'0'                                                   
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,VKMVN                                                         
         EX    R1,VKCLC                                                         
         BNE   NUMERR                                                           
         EX    R1,VKPK                                                          
         OI    DUB+7,X'0F'                                                      
         CVB   R0,DUB                                                           
*                                                                               
         CLM   R0,15,RQBSEQ         TEST SAME AS BEFORE                         
         BE    *+8                                                              
         MVI   NEWDISP,C'Y'                                                     
         STCM  R0,15,RQBSEQ                                                     
         B     VK300                                                            
*                                                                               
VKMVN    MVN   WORK(0),8(R2)                                                    
VKCLC    CLC   WORK(0),8(R2)                                                    
VKPK     PACK  DUB,8(0,R2)                                                      
*                                                                               
*                                  FILTERS LINE                                 
VK300    OI    4(R2),X'20'                                                      
         LA    R2,LINFTRH          FILTERS                                      
         GOTO1 =A(VFTR),RR=RELO                                                 
         OI    4(R2),X'20'                                                      
*                                                                               
         XC    ATOT,ATOT           ZERO ALL TOTALS                              
         XC    SVLSTBAT,SVLSTBAT   ZERO LAST BATCH DATA                         
*                                                                               
VKXIT    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'           AGENCY RECORD                                
         MVC   KEY+1(2),AGENCY                                                  
         B     XIT                                                              
*                                                                               
***********************************************************************         
*   LIST - LIST RECORDS                                               *         
***********************************************************************         
*                                                                               
LIST     DS    0H                                                               
         GOTO1 CALLOV,DMCB,(X'10',0),ATWA                                       
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                WHERE IS T23010                              
         L     RF,DMCB                                                          
         ST    RF,VEZMOD                                                        
*                                                                               
         BRAS  RE,INITEZB                                                       
*                                                                               
         MVI   USEIO,C'Y'          SET USER WILL DO ALL I/O                     
         MVI   EOJSW,C'N'                                                       
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LS050                                                            
*                                                                               
         LM    R0,R1,=A(HEADING,HDHK)                                           
         A     R0,RELO                                                          
         ST    R0,SPECS                                                         
         A     R1,RELO                                                          
         ST    R1,HEADHOOK                                                      
         B     *+8                                                              
*                                                                               
LS050    MVI   NLISTS,NUMLINS                                                   
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LS060                                                            
*                                                                               
         XCEFL INVLIST,L'INVLIST     CLEAR INVOICE NUMBER LIST                  
*                                                                               
LS060    DS    0H                                                               
         LA    R5,INVLIST          R5 TO START OF LIST                          
         ST    R5,INVLPTR                                                       
         XC    WKRCTS,WKRCTS                                                    
*                                                                               
         BRAS  RE,INITWKB                                                       
*                                                                               
         LAY   R4,EZWRKIOB                                                      
         USING WRKIOD,R4                                                        
*                                                                               
         XC    WRKEZKEY,WRKEZKEY                                                
*                                                                               
* SEE IF THIS IS A CONTINUATION OF LIST *                                       
*                                                                               
         OC    SVLSTBAT,SVLSTBAT   ANY LAST BATCH DATA                          
         BNZ   LS080                YES                                         
*                                                                               
         MVC   WRKEZUID,TWAORIG                                                 
         OC    FUIDNUM,FUIDNUM                                                  
         BZ    *+10                                                             
         MVC   WRKEZUID,FUIDNUM                                                 
         XC    BINV,BINV           ZERO INVOICE DISPLAYED COUNT                 
         B     LS100                                                            
*                                                                               
LS080    DS    0H                                                               
         MVC   WRKEZUID,SVBUID                                                  
         MVC   WRKEZSQN,SVBWKFLN   BATCH SEQ NUMBER                             
         OI    WRKINDS,WRKISEQQ                                                 
*                                                                               
LS100    DS    0H                                                               
         LAY   R4,EZWRKIOB                                                      
*                                                                               
         TM    FTRFLAG2,FTREXP                                                  
         BO    *+8                                                              
         OI    WRKINDS,WRKINOXQ                                                 
*                                                                               
         MVI   WRKIACTN,WRKIANDX                                                
*                                                                               
         GOTO1 EZWRKIO,(R4)                                                     
         CLI   WRKIERRS,WRKIEEOF                                                
         BE    LS200                                                            
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                  WRKIOB,R5                                    
*                                                                               
         CLI   OFFLINE,C'Y'        NO TEST IF OFFLINE                           
         BE    LS103                                                            
*                                                                               
         MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   SET ON RETURN                                
         GOTO1 CATCHIOS            SEE IF 90% OF MAX                            
         CLI   ERROR,0             IF ERROR, OVER MAX                           
         BNE   MAXIOSER                                                         
*                                                                               
* LOOP THRU ALL BATCHES - DISLIN DISPLAYS ALL INVOICES IN A BATCH *             
*                                                                               
LS103    DS    0H                                                               
         L     R1,IDXRDCT                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,IDXRDCT                                                       
*                                                                               
         BRAS  RE,FAKEIND                                                       
*                                                                               
         LA    R4,SVWEZIND                                                      
         USING UKRECD,R4                                                        
*                                                                               
         CLI   UKDAY,X'99'         MUST BE DAY 99                               
         BNE   LS100                                                            
*                                                                               
         OC    FTRINVNO,FTRINVNO   FILTERING ON INVOICE                         
         BNZ   LS104                                                            
*                                                                               
         TM    UKSTAT,X'08'        THIS FILE ON KEEP                            
         BZ    LS104                NO                                          
*                                                                               
* ALLOW ALL IF REQUESTING DONE, DEL, CONV                                       
*                                                                               
         TM    FTRFLAG,FTRCVQ+FTRDEL+FTRDONE+FTRALL                             
         BZ    LS100                              BYPASS                        
*                                                                               
LS104    DS    0H                                                               
         OC    FTRMOS,FTRMOS       FILTERING ON MOS                             
         BZ    LS105                                                            
*                                                                               
         LAY   R2,EZWRKIOB                                                      
         USING WRKIOD,R2                                                        
*                                                                               
         OC    WRKEZMOS,WRKEZMOS                                                
         BZ    *+14            NOT THERE - CHECK OLD, 1-BYTE MOS FIELD          
         MVC   HALF,WRKEZMOS                                                    
         B     LS104A                                                           
*                                                                               
         CLI   WRKEZUDT,X'00'                                                   
         JE    *+2                 NO MOS IN BATCH INDEX                        
*                                                                               
         MVC   BYTE,WRKEZUDT                                                    
         DROP  R2                                                               
*                                                                               
         BRAS  RE,NEW2OLD                                                       
*                                                                               
LS104A   DS    0H                                                               
         CLC   HALF,FTRBMOS                                                     
         BNE   LS100                                                            
*                                                                               
         USING EZWKRIXD,R4                                                      
*                                                                               
LS105    DS    0H                                                               
         MVC   SRCESTA(4),EZWISTN  STATION                                      
         MVC   SRCESTA+4(1),EZWIMED                                             
         MVC   ORIGSTA,SRCESTA                                                  
         DROP  R4                                                               
*                                                                               
         CLI   SRCESTA+3,C'.'                                                   
         BNE   *+8                                                              
         MVI   SRCESTA+3,C' '                                                   
*                                                                               
         MVC   EQUISTA,SRCESTA                                                  
*                                                                               
         OC    FUIDNUM,FUIDNUM     THIS A USER =                                
         BNZ   LS116                YES, ALSO SHOW ALL MEDIA'S                  
*                                                                               
         GOTO1 VEQVSTA             GO GET EQUIVALENT STATION                    
*                                                                               
         GOTO1 VFILTSYS            GO GET EQUIVALENT STATION                    
         BNE   LS100                                                            
*                                                                               
LS116    DS    0H                                                               
         USING W_RECD,R4                                                        
*                                                                               
         OC    RQSTA,RQSTA         STATION FILTER                               
         BZ    *+14                                                             
         CLC   EQUISTA,RQSTA                                                    
         BNE   LS100                                                            
*                                                                               
         OC    RQBSEQ,RQBSEQ       BATCH SEQ FILTER                             
         BZ    *+14                                                             
         CLC   RQBSEQ,W_FILENO                                                  
         BNE   LS100                                                            
         DROP  R4                                                               
*                                                                               
         USING EZWKRIXD,R4                                                      
         MVC   SVSTA(4),EZWISTN  STATION                                        
         MVC   SVSTA+4(1),EZWIMED                                               
         DROP  R4                                                               
*                                                                               
         CLI   FTRMEDIA,0                                                       
         BE    LS128                                                            
*                                                                               
         CLC   FTRMEDIA,EQVMED                                                  
         BNE   LS100                                                            
*                                                                               
LS128    DS    0H                                                               
         USING UKRECD,R4                                                        
*                                                                               
         OC    RQDTE,RQDTE         DATE FILTER                                  
         BZ    LS140                                                            
         CLC   UKAGELD,RQDTE                                                    
         BL    LS100               LOW, SKIP                                    
         BE    LS140               EQUAL, OK                                    
         CLI   DTPLUS,C'Y'         ELSE, CHECK NEED EXACT DATE                  
         BNE   LS100                                                            
*                                                                               
LS140    OC    FTRBSDT,FTRBSDT     FILTERING ON BATCH DATE                      
         BZ    LS142                NO                                          
         CLC   UKAGELD,FTRBSDT                                                  
         BL    LS100               LOW, SKIP                                    
         CLC   UKAGELD,FTRBEDT                                                  
         BH    LS100               HIGH, SKIP                                   
*                                                                               
LS142    DS    0H                                                               
         L     R1,RECRDCT                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,RECRDCT                                                       
*                                                                               
         LAY   R4,EZWRKIOB                                                      
         USING WRKIOD,R4                                                        
         MVI   WRKIACTN,WRKIAGET                                                
         GOTO1 EZWRKIO,(R4)                                                     
         CLI   WRKIERRS,WRKIEEOF                                                
         BE    LS100                                                            
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVWCMNT,WRKEZDSC                                                 
         DROP  R4                  WRKIOB,R5                                    
*                                                                               
         LA    R3,SVWEZIND                                                      
         USING W_RECD,R3                                                        
         MVC   SVWKDTEC,W_AGELD       SAVE DATE AND BATCH SEQ                   
         MVC   SVWKSTAT,W_STAT                                                  
*                                                                               
         LA    R2,SVWCMNT                                                       
         USING EZWKRCMD,R2                                                      
*                                                                               
         OC    FTRSRCE,FTRSRCE     FILTERING ON SOURCE                          
         BZ    LS144                                                            
         CLC   FTRSRCE,EZWCSRCE                                                 
         BNE   LS100                                                            
*                                                                               
* FOR RECONVERT, CONVERTED, DELETED, DON'T CHECK BATCH CONVERT FLAG             
*                                                                               
* N O T E - UNTIL THE WORKER COMMNENT BUG IS FIXED, DON'T TEST IT *             
*                                                                               
LS144    TM    FTRFLAG,FTRCVQ+FTRDEL+FTRDONE                                    
         BNZ   LS145                                                            
         TM    EZWCSTAT,X'40'      FULLY CONVERTED                              
         NOP   LS100                YES, BYPASS                                 
         B     LS146                                                            
*                                                                               
LS145    TM    EZWCSTAT,X'40'      FULLY CONVERTED                              
         NOP   LS100                NO, BYPASS                                  
         DROP  R2,R3                                                            
*                                                                               
LS146    DS    0H                                                               
         L     RE,=A(DISLIN)                                                    
         A     RE,RELO                                                          
         ST    RE,EZHOOK                                                        
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         GOTO1 VEZMOD,DMCB,(R6)                                                 
*                                                                               
         XC    SVLSTBAT,SVLSTBAT                                                
*                                                                               
* RETURN HERE ONLY AT END OF BATCH *                                            
*                                                                               
         CLI   MODE,PRINTREP       IF PRINTING REPORT                           
         BE    LS150                YES, NEXT BATCH                             
*                                                                               
         OC    RQBSEQ,RQBSEQ       BATCH SEQ FILTER                             
         BNZ   XIT                  YES, ALL DONE                               
*                                                                               
         B     LS100                NO, NEXT BATCH                              
*                                                                               
LS150    LM    RE,R1,JTOT          ADD BATCH TO JOB TOTALS                      
         LA    RE,1(,RE)                                                        
         A     RF,BINV                                                          
         A     R0,BSPTS                                                         
         A     R1,BDOLS                                                         
         STM   RE,R1,JTOT                                                       
*                                                                               
* PRINT BATCH TOTALS *                                                          
*                                                                               
         OC    BINV,BINV          IF ZERO, BYPASS                               
         BZ    LS180                                                            
*                                                                               
         LA    R0,1                                                             
         C     R0,BINV             UNLESS ONLY 1                                
         BE    LS180                                                            
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         EDIT  (B4,BINV),(5,PINV-6),COMMAS=YES,ZERO=NOBLANK                     
         MVC   PINV(8),=C'INVOICES'                                             
*                                                                               
         EDIT  (B4,BSPTS),(10,PSPTS-5),COMMAS=YES,ZERO=NOBLANK                  
         EDIT (B4,BDOLS),(13,PDOLS),2,COMMAS=YES,ZERO=NOBLANK,MINUS=YES         
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LS180    XC    BTOT,BTOT           CLEAR BATCH TOTALS                           
*                                                                               
         OC    RQBSEQ,RQBSEQ       BATCH SEQ FILTER                             
         BNZ   LS200                YES, ALL DONE                               
*                                                                               
         B     LS100                                                            
*                                                                               
LS200    XC    SVLSTBAT,SVLSTBAT   ZERO LAST BATCH DATA                         
*                                                                               
         EDIT  (B4,BINV),(6,LINTL+30),COMMAS=YES                                
         MVC   LINTL+36(9),=C'=INVOICES'                                        
*                                                                               
         OI    LINTLH+6,X'80'                                                   
*                                                                               
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   LS210                                                            
         EDIT  (B4,IDXRDCT),(6,LINTL),COMMAS=YES                                
         MVC   LINTL+6(6),=C'=INDEX'                                            
         EDIT  (B4,RECRDCT),(6,LINTL+13),COMMAS=YES                             
         MVC   LINTL+19(5),=C'=GETS'                                            
         OI    LINTLH+6,X'80'                                                   
*                                                                               
LS210    CLI   MODE,PRINTREP       UNLESS PRINTING REPORT                       
         BNE   XIT                  ALL DONE                                    
*                                                                               
* PRINT JOB TOTALS *                                                            
*                                                                               
         MVC   SVWCSRCE,SPACES     BLANK OUT HEADINGS                           
*                                                                               
         MVC   PRTSTA7C,SPACES                                                  
*                                                                               
*        XC    SVWKFILN,SVWKFILN                                                
         XC    SVWKSTAT,SVWKSTAT                                                
         MVI   SVWCSTAT,0                                                       
         MVI   EOJSW,C'Y'          SET TO Y AT EOJ, SUPPRESS HDHK               
*                                                                               
         OC    JBAT,JBAT           DON'T PRINT NONSENSE TOTALS                  
         BZ    LS220                FOR NONE                                    
         LA    R0,1                                                             
         C     R0,JBAT             DON'T PRINT NONSENSE TOTALS                  
         BE    LS220                OR 1                                        
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         EDIT  (B4,JBAT),(5,PPRDCD-6),COMMAS=YES,ZERO=NOBLANK                   
         MVC   PPRDCD(7),=C'BATCHES'                                            
*                                                                               
         EDIT  (B4,JINV),(5,PINV-6),COMMAS=YES,ZERO=NOBLANK                     
         MVC   PINV(7),=C'INVOICE'                                              
         LA    R0,1                                                             
         C     R0,JINV                                                          
         BE    *+8                                                              
         MVI   PINV+7,C'S'                                                      
*                                                                               
         EDIT  (B4,JSPTS),(10,PSPTS-5),COMMAS=YES,ZERO=NOBLANK                  
         EDIT (B4,JDOLS),(13,PDOLS),2,COMMAS=YES,ZERO=NOBLANK,MINUS=YES         
*                                                                               
         MVI   ALLOWLIN,2                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
LS220    NI    LINSTAH+4,X'FF'-X'20' FORCE START AT TOP OF LIST                 
         CLI   MODE,PRINTREP                                                    
         BE    XIT                                                              
*                                                                               
         XC    SVLSTBAT,SVLSTBAT     SET NO LAST BATCH DATA                     
         XCEFL INVLIST,L'INVLIST       CLEAR INVOICE NUMBER LIST                
         XC    WKRCTS,WKRCTS                                                    
         B     XIT                                                              
*                                                                               
***********************************************************************         
*   DREC - DISPLAY RECORD                                             *         
***********************************************************************         
DREC     DS    0H                                                               
         CLI   RECNUM,X'12'        CONSUM                                       
         BE    *+12                                                             
         CLI   RECNUM,X'0F'        CONLIST?                                     
         BNE   DR10                IF NO - SKIP, OTHERWISE                      
         CLI   SELLISTN,X'00'                                                   
         BE    *+10                                                             
         MVC   SVLINNUM,SELLISTN   SAVE SELECT LINE NUMBER                      
*                                                                               
DR10     DS    0H                                                               
         GOTO1 =A(PREZ),RR=RELO    FIND RECORD, CALL EZMOD, ETC.                
*                                                                               
         OI    DIVFLGH+1,X'01'     SET MODIFIED BIT ON                          
         OI    DIVFLGH+6,X'80'     FORCE TRANSMIT                               
*                                                                               
         B     XIT                                                              
*                                                                               
***********************************************************************         
*   VREC - VALIDATE RECORD                                            *         
***********************************************************************         
*                                                                               
VREC     DS    0H                                                               
         XC    KPFLAGS,KPFLAGS                                                  
         MVI   RESTORSW,C'N'                                                    
         MVI   DELETESW,C'N'                                                    
         MVI   RECONVSW,C'N'                                                    
         MVI   CONVRTSW,C'N'                                                    
*                                                                               
         XC    IMSWITCH,IMSWITCH                                                
         MVC   SVSNMED,SVMEDTWA                                                 
*                                                                               
         OC    FUIDNUM,FUIDNUM     HAS ID BEEN SET AS OPTION                    
         BNZ   UIDVRERR                                                         
*                                                                               
         TM    DIVCCDH+4,X'20'     TEST NEED TO VALIDATE CLIENT                 
         BZ    VR006                                                            
         TM    DIVPCDH+4,X'20'     OR PRODUCT                                   
         BZ    VR006                                                            
         TM    DIVCSTH+4,X'20'     OR RECONVERT/CANCEL/DELETE                   
         BZ    VR010                                                            
*                                                                               
         TM    DIVREPH+4,X'20'     OR REP ID                                    
         BZ    VR010                                                            
         TM    DIVREPH+4,X'80'     INPUT THIS TIME?                             
         BO    VR010                                                            
*                                                                               
         CLI   SPOTNETS,C'N'       NET MEDIA?                                   
         BNE   VR003                                                            
*                                                                               
         LA    R2,DIVPKGH                                                       
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BO    VR003               YES - EXIT                                   
         CLI   5(R2),X'00'         INPUT?                                       
         BE    VR006            IF NO - VALIDATE (COULD BE SPACED OUT)          
         CLI   DIVCCDH+5,X'00'  IF INPUT, CHECK WHETHER CLIENT ENTERED          
         BE    NOPKG               NO CLIENT - NO PACKAGE                       
         CLI   DIVPCDH+5,X'00'     PRODUCT ENTERED?                             
         BE    NOPKG                                                            
         B     VR006               CLT,PRD ENTERED - VALIDATE                   
                                                                                
VR003    DS    0H                                                               
         NI    GENSTAT2,X'FF'-RETEQSEL   SET NO RETURN THIS SELECTION           
*                                                                               
         OI    DIVFLGH+1,X'01'     SET MODIFIED BIT ON                          
         OI    DIVFLGH+6,X'80'     FORCE TRANSMIT                               
*                                                                               
         B     XIT                                                              
*                                                                               
VR006    DS    0H                                                               
         BAS   RE,TSTRO                                                         
*                                                                               
         TM    SVIHCVST,EZIHCVQ    THIS INVOICE CONVERTED                       
         BZ    VR020                NO, CHANGES ALLOWED                         
*                                                                               
         LA    R2,DIVCSTH          RECONVERT/CANCEL/DELETE FIELD                
         CLI   5(R2),0     ANY REQUEST ENTERED                                  
         BE    CVTCHGER             NO, NO CHANGES ALLOWED                      
         BE    VR266                                                            
         LLC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
*                                                                               
         EX    RF,VR2CLC           RECONVERT                                    
         BNE   CVTCHGER             NO, NO CHANGES ALLOWED                      
*                                                                               
VR010    DS    0H                                                               
         BAS   RE,TSTRO                                                         
*                                                                               
         CLI   DIVCCDH+5,0         CLIENT ENTERED                               
         BNE   VR020                YES                                         
         CLI   DIVPCDH+5,0         PRODUCT ENTERED                              
         BNE   VR020                YES                                         
*                                                                               
         OC    SVIHCVAD,SVIHCVAD   WAS THERE AN OVERRRIDE CLT                   
         BNZ   VR022                THEN MUST CONTINUE TO BE                    
*                                                                               
         OI    DIVCCDH+4,X'20'     SET CLIENT VALIDATED                         
         XC    DIVCNMD,DIVCNMD                                                  
         OI    DIVCNMH+6,X'80'                                                  
         OI    DIVPCDH+4,X'20'     SET PRODUCT VALIDATED                        
         XC    DIVPCD,DIVPCD                                                    
         XC    DIVPNMD,DIVPNMD                                                  
         OI    DIVPNMDH+6,X'80'                                                 
         XC    DIVPN2D,DIVPN2D                                                  
         OI    DIVPN2DH+6,X'80'                                                 
         MVI   DIVOVRT,0                                                        
         OI    DIVOVRTH+4,X'20'     SET VALIDATED                               
         XC    SVIHCVAD,SVIHCVAD                                                
         MVI   SVIHCVPR,0                                                       
         MVI   SVIHCVP2,0                                                       
         XC    SVIHCPRD,SVIHCPRD                                                
         XC    SVIHCPR2,SVIHCPR2                                                
         MVI   SVIHCVES,0                                                       
         MVI   SVIHCPKG,0                                                       
         B     VR200                                                            
*                                                                               
VR020    LA    R2,DIVCCDH                                                       
         CLI   DIVCCDH+5,0         CLIENT NOT ENTERED                           
         BE    MISSERR               TEMP UNTIL WE USE CLINAME RECS             
*                                                                               
VR022    LA    R2,DIVCCDH                                                       
         CLI   DIVCCDH+5,0         CLIENT NOT ENTERED                           
         BE    MISSERR                                                          
*                                                                               
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         GOTO1 VALICLT                                                          
*                                                                               
         TM    SVSFLAG1,STPG                                                    
         BZ    VR028                                                            
         TM    SVCOPT4,COP4PG                                                   
         BZ    PGERR                                                            
*                                                                               
VR028    MVC   SVIHCVAD,BCLT       2 BYTE PACKED OVERRIDE CLT                   
         MVC   SVCLTTWA,QCLT       3 CHAR OVERRIDE CLT                          
         MVC   DIVCNMD(20),CLTNM                                                
         OI    DIVCNMDH+6,X'80'                                                 
         OI    DIVCCDH+4,X'20'     SET VALIDATED                                
         MVI   CHANGESW,C'Y'       SET INVOICE CHANGED                          
         B     VR100                                                            
*                                                                               
VR030    DS    0H                  FIND CLIENT FROM AGENCY ADV CODE             
*                                       OR FROM 25 CHAR CODE                    
         OC    SVIHAAID,SVIHAAID                                                
         BNZ   VR060                                                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING EZCNMD,R3                                                        
*                                                                               
         MVC   EZCKTYP,=C'ZC'                                                   
         MVC   EZCKAGY,AGENCY                                                   
         MVC   EZCKNAM,SVIHADVN                                                 
*                                                                               
         MVI   CURSYST,C'P'                                                     
         GOTO1 VALIFAS             SWITCH TO MPL SYSTEM                         
*                                                                               
         GOTO1 HIGH                                                             
         OC    EZCKNAM,SPACES                                                   
         OC    KEYSAVE+EZCKNAM-EZCNMD(25),SPACES                                
         CLC   KEY(32),KEYSAVE                                                  
         BE    VR034                                                            
*                                  NOT ON FILE ERROR                            
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
         B     NOCLTFND                                                         
*                                                                               
VR034    DS    0H                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         L     R3,AIO1                                                          
         LA    R3,EZCELS                                                        
         MVI   DUB,0               CLEAR CONTROL                                
*                                                                               
VR040    DS    0H                                                               
         CLI   0(R3),0                                                          
         BE    VR050                                                            
         CLI   0(R3),X'02'                                                      
         BE    VR044                                                            
*                                                                               
         LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     VR040                                                            
*                                                                               
VR044    DS    0H                                                               
         USING EZCIDEL,R3                                                       
         MVI   BYTE,0                                                           
         CLC   =C'ALL',EZCSTA      UNLESS ALL STATION ELEM                      
         BE    VR046                                                            
         CLI   EZCSTA,C' '         OR BLANK                                     
         BNH   VR046                                                            
         CLC   EZCCALL,QSTA        SPECIFIC STATION                             
         BNE   VR040               MUST AGREE WITH WKR FILE KEY                 
         OI    BYTE,X'80'          SET HAVE SPECIFIC STATION                    
*                                                                               
VR046    DS    0H                                                               
         CLI   EZCMED,C' '         TEST HAVE SPECIFIC MEDIA                     
         BNH   VR048                                                            
         CLC   EZCMED,QSTA+4       MUST AGREE WITH WKR FILE KEY                 
         BNE   VR040                                                            
         OI    BYTE,X'40'          YES                                          
*                                                                               
VR048    DS    0H                                                               
         CLC   BYTE,DUB            TEST NEW AS SPECIFIC AS OLD                  
         BL    VR040               NO - SKIP IT                                 
*                                                                               
VR050    DS    0H                                                               
         CLC   EZCCOD,=C'***'      TEST 'UNKNOWN'                               
         BE    NOCLTFND                                                         
         MVC   QCLT,EZCCOD         SET CLIENT CODE                              
         OC    QCLT,SPACES                                                      
         TM    BYTE,X'C0'          BOTH STAT AND MED SPECIFIC                   
         BO    VR054               YES - QUIT LOOKING                           
         MVC   DUB(1),BYTE         SAVE LAST STATUS                             
         B     VR040                                                            
         DROP  R3                                                               
*                                                                               
VR054    DS    0H                                                               
         OC    QCLT,QCLT           TEST FOUND CLIENT                            
         BZ    NOCLTFND                                                         
         GOTO1 CLPACK,DMCB,QCLT,SVIHCVAD                                        
         B     VR070                                                            
*                                                                               
VR060    DS    0H                                                               
         MVC   QCLT,SVIHAAID                                                    
         GOTO1 CLPACK,DMCB,SVIHAAID,SVIHCVAD                                    
*                                                                               
* NOW DOUBLE CHECK IF CLIENT IS ON FILE *                                       
*                                                                               
VR070    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),SVIHCVAD                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NOSPTCLT                                                         
         MVC   BCLT,SVIHCVAD                                                    
         MVI   SVCLTSPR,0                                                       
*                                                                               
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         MVC   FILENAME+3(3),=C'FIL'                                            
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLI   CPROF+14-CLTHDR(R4),C'0'   SPECIAL RATE CODE                     
         BE    VR076                       NO                                   
         CLI   CPROF+14-CLTHDR(R4),0      SPECIAL RATE CODE                     
         BE    VR076                       NO                                   
*                                                                               
         B     VR076        ** TEMP **                                          
*                                                                               
         LA    R0,7                                                             
         LA    RE,=C'SFNQVXP'                                                   
         LA    RF,=C'1234567'                                                   
*                                                                               
VR072    CLC   CPROF+14-CLTHDR(1,R4),0(RF)  SPECIAL RATE CODE                   
         BE    VR074                                                            
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,VR072                                                         
         MVI   DIVSPRT,C'?'                                                     
         B     VR076                                                            
VR074    MVC   SVCLTSPR,0(RE)                                                   
*                                                                               
VR076    OI    DIVCCDH+4,X'20'     SET CLIENT VALIDATED                         
*                                                                               
* VALIDATING PRODUCT                                                            
*                                                                               
VR100    LA    R2,DIVPCDH                                                       
         MVI   BPRD,0                                                           
         MVI   BPRD2,0                                                          
         XC    QPRD,QPRD                                                        
         XC    QPRD2,QPRD2                                                      
         MVI   SVIHCVPR,0                                                       
         MVI   SVIHCVP2,0                                                       
         XC    SVIHCPRD,SVIHCPRD                                                
         XC    SVIHCPR2,SVIHCPR2                                                
         MVI   SVIHCPRD,0                                                       
         MVI   SVIHCPR2,0                                                       
         MVI   SVIHCVES,0                                                       
         MVI   SVIHCPKG,0                                                       
         MVI   SVESTSPR,0                                                       
*                                                                               
         XC    DIVPNMD,DIVPNMD                                                  
         OI    DIVPNMDH+6,X'80'                                                 
         XC    DIVPN2D,DIVPN2D                                                  
         OI    DIVPN2DH+6,X'80'                                                 
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VR200                NO                                          
         MVI   COMPNUM,X'00'                                                    
*                                                                               
         LA    RE,PBLOCK                                                        
         LHI   RF,L'PBLOCK                                                      
         XCEFL                                                                  
*                                                                               
         GOTO1 VPARSNIP,DMCB,(R2),(4,PBLOCK),                          +        
               ('PSNMVOKQ+PSNNPARQ',PARSEPTR)                                   
         CLI   8(R1),0                                                          
         BNE   INVAL                                                            
         CLI   4(R1),0            ANY INPUT AT ALL?                             
         BE    INVAL                                                            
         CLI   4(R1),3            MORE THAN 3?                                  
         BH    PRDENTER                                                         
*                                                                               
         LA    R4,PBLOCK                                                        
         USING PSND,R4                                                          
*                                                                               
VR110    DS    0H                  VALIDATE LOOP                                
         IC    R0,COMPNUM          INCREMENT COMPONENT COUNTER                  
         AHI   R0,1                                                             
         STC   R0,COMPNUM                                                       
*                                  PREPARE FAKE FIELD                           
         XC    FAKEFLDH,FAKEFLDH                                                
         XC    FAKEFLD,FAKEFLD                                                  
         MVI   FAKEFLDH,11         8 BYTES HEADER + MAX 3 CHARS                 
         LLC   RF,PSNLEN           LENGTH OF COMPONENT                          
         STC   RF,FAKEFLDH+5                                                    
         L     R1,PSNCOMP          A(COMPONENT)                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD(0),0(R1)    COPY COMPONENT INTO FAKE FIELD               
         LA    R2,FAKEFLDH                                                      
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         CLI   COMPNUM,1                                                        
         BE    VR120               HAS TO BE PRODUCT                            
         CLI   COMPNUM,3                                                        
         BE    VR170               HAS TO BE ESTIMATE                           
*                         SECOND COMPONENT CAN BE ESTIMATE OR PIGGYBACK         
         TM    PSNSTAT,PSNNUMQ     NUMERIC - MUST BE ESTIMATE                   
         BO    VR170                                                            
*                                  OTHERWISE - PIGGYBACK PRODUCT                
VR120    DS    0H                  VALIDATING PRODUCT                           
         MVI   ERROPT,C'Y'                                                      
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,C'N'                                                      
         CLI   ERROR,X'00'         ANY ERRORS?                                  
         BE    VR130               NO - PROCEED                                 
*                                                                               
         LA    R2,DIVPCDH          POINT CURSOR TO PRD,PTR-EST FIELD            
         MVC   QPRD,FAKEFLD                                                     
         B     NOSPTPRD            INVALID PRODUCT ERROR                        
*                                                                               
VR130    DS    0H                  PRODUCT VALIDATION OK                        
         MVI   CHANGESW,C'Y'       SET INVOICE CHANGED                          
         LA    RF,DIVPNMDH                                                      
         CLI   COMPNUM,1           FIRST FIELD?                                 
         BE    *+8                 YES, THIS MEANS FIRST PRODUCT                
         LA    RF,DIVPN2DH         OTHERWISE - SECOND PRODUCT                   
         MVC   8(20,RF),WORK+4     TRANSMIT NAME                                
         OI    6(RF),X'80'                                                      
*                                                                               
         CLI   COMPNUM,1           FIRST FIELD?                                 
         BNE   VR140               YES, THIS MEANS FIRST PRODUCT                
         MVC   QPRD,WORK                                                        
         MVC   BPRD,WORK+3                                                      
         MVC   SVIHCVPR,WORK+3                                                  
         MVC   SVIHCPRD,WORK                                                    
         B     VR150                                                            
*                                                                               
VR140    MVC   QPRD2,WORK                                                       
         MVC   BPRD2,WORK+3                                                     
         MVC   SVIHCVP2,WORK+3                                                  
         MVC   SVIHCPR2,WORK                                                    
*                                                                               
VR150    DS    0H                  PRODUCT VALIDATION OK                        
         L     R4,PSNFLD           NEXT FIELD                                   
         CHI   R4,0                IS THERE NEXT FIELD?                         
         BE    VR190               IF NO - EXIT                                 
         B     VR110               ON TO VALIDATING NEXT COMPONENT              
*                                                                               
VR170    DS    0H                  VALIDATE ESTIMATE                            
         MVI   ERROPT,C'Y'                                                      
         GOTO1 VALIEST                                                          
         MVI   ERROPT,C'N'                                                      
         CLI   ERROR,X'00'         ANY ERRORS?                                  
         BE    VR175               NO - PROCEED                                 
         LA    R2,DIVPCDH          POINT CURSOR TO PRD,PTR-EST FIELD            
         MVC   QEST,FAKEFLD                                                     
         B     INVEST              INVALID ESTIMATE ERROR                       
*                                                                               
VR175    DS    0H                                                               
         MVC   SVIHCVES,PSNNUM+3   SAVE BINARY ESTIMATE                         
         OC    QPRD2,QPRD2                                                      
         BZ    VR180                                                            
         MVC   SVQPRD,QPRD                                                      
         MVC   SVBPRD,BPRD                                                      
         MVI   ERROPT,C'Y'                                                      
         GOTO1 VALIEST                                                          
         MVC   QPRD,SVQPRD                                                      
         MVC   BPRD,SVBPRD                                                      
         MVI   ERROPT,C'N'                                                      
         CLI   ERROR,X'00'         ANY ERRORS?                                  
         BE    VR180               NO - PROCEED                                 
         LA    R2,DIVPCDH          POINT CURSOR TO PRD,PTR-EST FIELD            
         MVC   QEST,FAKEFLD                                                     
         B     INVEST              INVALID ESTIMATE ERROR                       
*                                                                               
VR180    DS    0H                                                               
*                                  MAKE SURE ESTIMATE IS THE LAST FIELD         
         LA    R2,DIVPCDH          POINT CURSOR TO PRD,PTR-EST FIELD            
         L     R4,PSNFLD           NEXT FIELD                                   
         CHI   R4,0                IS THERE NEXT FIELD?                         
         BNE   PRDENTER            YES - ERROR.  NOTHING AFTER ESTIMATE         
*                                                                               
         L     R1,AIO                                                           
         MVC   SVESTDTS,ESTART-ESTHDR(R1)                                       
         CLC   SVESTSTR,SVIHCMEN   EST STR AFTER MOS END                        
         BH    ESTDATER                                                         
         CLC   SVESTEND,SVIHCMST   EST END BEFORE MOS START                     
         BL    ESTDATER                                                         
*                                                                               
* SAVE ESTIMATE SPECIAL RATE CODE *                                             
*                                                                               
         CLI   ERATE-ESTHDR(R1),C'0'      SPECIAL RATE CODE                     
         BE    VR190                       NO                                   
         CLI   ERATE-ESTHDR(R1),0         SPECIAL RATE CODE                     
         BE    VR190                       NO                                   
*                                                                               
         B     VR190     ** TEMP **                                             
*                                                                               
         LA    R0,7                                                             
         LA    RE,=C'SFNQVXP'                                                   
         LA    RF,=C'1234567'                                                   
VR184    CLC   ERATE-ESTHDR(1,R1),0(RF)  SPECIAL RATE CODE                      
         BE    VR186                                                            
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,VR184                                                         
*                                                                               
         MVI   SVESTSPR,0          NEVER FOUND VALID CODE                       
         B     VR190                                                            
*                                                                               
VR186    MVC   SVESTSPR,0(RE)                                                   
*                                                                               
VR190    XC    FILENAME,FILENAME                                                
*        OI    DIVPCDH+4,X'20'     SET PRODUCT VALIDATED                        
*                                                                               
* TRANS NEW SPEC RATE IF ANY HERE                                               
*                                                                               
VR200    DS    0H                                                               
         OI    DIVPCDH+4,X'20'     SET PRODUCT VALIDATED                        
*                                  VALIDATE PACKAGE                             
         CLI   SPOTNETS,C'N'       NET MEDIA?                                   
         BNE   VR215                                                            
*                                                                               
         NIY   SVIHFL2,X'FF'-EZIHF2NOPKGQ  NO-PACKAGE '#' OVERRIDE              
         MVC   SVIHCPKG,SVPKGTWA   SAVE PREVIOUS OVERRIDE VALUE                 
         LA    R2,DIVPKGH                                                       
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BO    VR210               YES - EXIT                                   
         MVI   SVIHCPKG,0                                                       
         CLI   5(R2),0                                                          
         BE    VR210                                                            
*                                                                               
         CLI   DIVPKG,C'#'                                                      
         BNE   VR205                                                            
         OIY   SVIHFL2,EZIHF2NOPKGQ  NO-PACKAGE '#' OVERRIDE                    
         OI    DIVPKGH+4,X'20'     SET PACKAGE VALIDATED                        
         B     VR215                                                            
*                                                                               
VR205    DS    0H                                                               
         TM    4(R2),X'08'                                                      
         BZ    INVLFLD                                                          
         LLC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         CHI   RE,1                TEST IN RANGE 1-255                          
         BL    INVLFLD                                                          
         CHI   RE,255                                                           
         BH    INVLFLD                                                          
         STC   RE,SVIHCPKG                                                      
*                                                                               
VR210    DS    0H                                                               
         CLI   SVIHCPKG,X'00'                                                   
         BE    *+12                                                             
         CLI   DIVPCDH+5,X'00'     PRODUCT ENTERED?                             
         BE    NOPKG                                                            
         OI    DIVPCDH+4,X'20'     SET PRODUCT VALIDATED                        
         OI    DIVPKGH+4,X'20'     SET PACKAGE VALIDATED                        
*                                                                               
VR215    DS    0H                                                               
         MVC   BYTE,SVCLTSPR       CLIENT SPECIAL RATE                          
         CLI   SVESTSPR,0                                                       
         BE    *+10                                                             
         MVC   BYTE,SVESTSPR       ESTIMATE OVERRIDES CLIENT                    
*                                                                               
         CLC   DIVSPRT,BYTE                                                     
         BE    *+14                                                             
         MVC   DIVSPRT,BYTE                                                     
         OI    DIVSPRTH+6,X'80'                                                 
*                                                                               
         TM    DIVOVRTH+4,X'20'     OVERRIDE SPECIAL RATE ALREADY VAL           
         BO    VR240                 YES                                        
         LA    R2,DIVOVRTH                                                      
         CLI   5(R2),0                                                          
         BNE   VR220                                                            
         CLI   SVIHSPRT,0          WAS THERE A SPECIAL RATE                     
         MVI   SVIHSPRT,0                                                       
         BNE   VR236                YES, THIS IS A CHANGE                       
         B     VR238                                                            
*                                                                               
VR220    LA    R0,7                                                             
         LA    R1,=C'SFNQVXP'                                                   
*                                                                               
VR232    CLC   8(1,R2),0(R1)                                                    
         BE    VR234                                                            
         LA    R1,1(,R1)                                                        
         BCT   R0,VR232                                                         
         B     SPRATERR                                                         
*                                                                               
VR234    MVC   SVIHSPRT,8(R2)                                                   
*                                                                               
VR236    OI    GENSTAT2,RETEQSEL   SET TO RETURN THIS SELECTION                 
*                                                                               
         OI    DIVFLGH+1,X'01'     SET MODIFIED BIT ON                          
         OI    DIVFLGH+6,X'80'     FORCE TRANSMIT                               
*                                                                               
         MVI   CHANGESW,C'Y'       SET INVOICE CHANGED                          
VR238    OI    DIVOVRTH+4,X'20'     SET VALIDATED                               
*                                                                               
VR240    DS    0H                                                               
         TM    DIVCSTH+4,X'20'     CK FOR CANCEL, DELETE, OR RECONVERT          
         BO    VR270                                                            
*                                                                               
         LA    R2,DIVCSTH                                                       
         CLI   5(R2),0                                                          
         BE    VR266                                                            
         LLC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
*                                                                               
         EX    RF,VR2CLCA          CANCEL RECONVERT?                            
         BNE   VR252                                                            
*                                                                               
         TM    SVIHCVST,EZIHRCVQ   IS INVOICE IN "RECONVERT" STATUS?            
         BZ    NOCONVER            NO - CAN'T CANCEL                            
         MVI   CONVRTSW,C'Y'       TURN ON CONVERT FLAG                         
         MVI   CHANGESW,C'Y'       SET INVOICE CHANGED                          
         B     VR266                                                            
*                                                                               
VR252    DS    0H                                                               
         EX    RF,VR2CLCB          DELETE                                       
         BNE   VR254                                                            
*                                                                               
         TM    SVIHCVST,EZIHCVQ    TEST CONVERTED                               
         BO    CNVDELER                                                         
         MVI   DELETESW,C'Y'       SET INVOICE DELETED                          
         MVI   CHANGESW,C'Y'       SET INVOICE CHANGED                          
         OI    IMSWITCH,IMSWDELQ   INDICATE HAVE A DELETE                       
         B     VR266                                                            
*                                                                               
VR254    EX    RF,VR2CLC           RECONVERT                                    
         BNE   VR256                                                            
*                                                                               
         TM    SVIHCVST,EZIHCVQ    WAS THIS CONVERTED?                          
         BZ    CONRECER                                                         
         MVI   RECONVSW,C'Y'                                                    
         MVI   CHANGESW,C'Y'       SET INVOICE CHANGED                          
         OI    DIVCSTSH+6,X'80'                                                 
         MVC   DIVCSTS,=C'RECONVERT'                                            
         B     VR258                                                            
*                                                                               
VR256    EX    RF,VR2CLCC          RESTORE                                      
         BNE   CONVRTER                                                         
*                                                                               
         TM    SVIHCVST,EZIHCDEL   WAS THIS DELETED                             
         BZ    DELRECER                                                         
         MVI   RESTORSW,C'Y'                                                    
         MVI   CHANGESW,C'Y'       SET INVOICE CHANGED                          
         OI    DIVCSTSH+6,X'80'                                                 
         MVC   DIVCSTS,=C'RESTORED '                                            
         OI    IMSWITCH,IMSWRESQ   INDICATE WE HAVE A RESTORE                   
*                                                                               
VR258    OI    GENSTAT2,RETEQSEL   SET TO RETURN THIS SELECTION                 
         OI    DIVFLGH+1,X'01'     SET MODIFIED BIT ON                          
         OI    DIVFLGH+6,X'80'     FORCE TRANSMIT                               
         B     VR266                                                            
*                                                                               
VR266    OI    DIVCSTH+4,X'20'                                                  
         B     VR270                                                            
*                                                                               
VR2CLC   CLC   8(0,R2),=C'RECONVERT'                                            
VR2CLCA  CLC   8(0,R2),=C'CANCEL '                                              
VR2CLCB  CLC   8(0,R2),=C'DELETED'                                              
VR2CLCC  CLC   8(0,R2),=C'RESTORE '                                             
*                                                                               
VR270    DS    0H                                                               
         LA    R2,DIVREPH                                                       
         TM    DIVREPH+1,X'0C'     FIELD ZERO INTENSITY?                        
         BO    VR275               YES - NO REP VALIDATION                      
*                                                                               
         TM    DIVREPH+4,X'20'     VALIDATED ALREADY?                           
         BZ    *+12                                                             
         TM    DIVREPH+4,X'80'     INPUT THIS TIME?                             
         BZ    VR272                                                            
*                                                                               
         TM    SVIHCVST,EZIHCVQ    THIS INVOICE CONVERTED                       
         BZ    VR272                NO, OK                                      
         CLI   RECONVSW,C'Y'       ARE WE RE-CONVERTING?                        
         BNE   CVTCHER2             NO, NO CHANGES ALLOWED                      
*                                                                               
VR272    DS    0H                                                               
         XC    SVIHCREP,SVIHCREP                                                
*                                                                               
         CLI   DIVREPH+5,X'00'     ANYTHING?                                    
         BE    *+18                NO, NOTHING TO SAVE                          
         CLI   DIVDARF,C' '        SOME DATA. USER OVERRIDE?                    
         BH    *+10                NO, DON'T SAVE IT                            
         MVC   SVIHCREP,DIVREP     YES - SAVE IT                                
*                                                                               
         CLI   DIVREPH+5,X'00'                                                  
         BE    VR274                                                            
         TM    DIVREPH+4,X'80'     FIELD INPUT THIS TIME?                       
         BZ    VR274               NO                                           
*                                                                               
         MVI   DIVDARF,C' '        YES - INDICATE USER OVERRIDE                 
         OI    DIVDARFH+6,X'80'                                                 
*                                                                               
         CLC   =C'###',SVIHCREP                                                 
         BE    VR274                                                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING REPREC,R3                                                        
         MVC   REPKEY,=15C'0'                                                   
         MVI   REPKTYPE,C'R'                                                    
*                                                                               
         MVC   REPKMED,EQVMED                                                   
*                                                                               
         MVC   REPKREP,DIVREP                                                   
         OC    REPKREP,SPACES                                                   
         MVC   REPKAGY,AGENCY                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION ',REPKEY,AIO1                 
         L     R3,AIO1                                                          
         CLC   KEY(L'REPKEY),0(R3)                                              
         BNE   INVREPER                                                         
         MVC   SVIHCREP,REPKREP                                                 
         DROP  R3                                                               
*                                                                               
VR274    DS    0H                                                               
         OI    DIVREPH+4,X'20'     SET VALIDATED                                
         OI    DIVREPH+6,X'80'                                                  
         MVI   CHANGESW,C'Y'       SET INVOICE CHANGED                          
*                                                                               
VR275    DS    0H                                                               
         CLI   DIVCCDH+5,0         CLIENT NOT ENTERED                           
         BE    VR280                                                            
         XC    BCLT,BCLT                                                        
*                                                                               
VR280    DS    0H                                                               
         OC    SVIHBMOS,SVIHBMOS   WAS MONTH OF SERVICE BLANK                   
         BNZ   *+12                                                             
         TM    SVIHCVST,EZIHCDEL   WAS THIS DELETED                             
         BZ    MOSVRERR                                                         
*                                                                               
         GOTO1 =A(PREZ),RR=RELO    FIND RECORD, CALL EZMOD, ETC.                
*                                                                               
         OI    DIVFLGH+1,X'01'     SET MODIFIED BIT ON                          
         OI    DIVFLGH+6,X'80'     FORCE TRANSMIT                               
*                                                                               
VRX      DS    0H                                                               
         B     CHGOKMSG                                                         
*                                                                               
***********************************************************************         
*   VRHOOK - VALIDATE REC EZMOD HOOK                                  *         
***********************************************************************         
         DS    0H                                                               
VRHOOK   NTR1                                                                   
         CLI   RECNUM,X'12'        CONSUM                                       
         BE    *+12                                                             
         CLI   RECNUM,X'0F'        CONLIST                                      
         BNE   VRH010                                                           
         CLI   SVLINNUM,X'00'                                                   
         BE    *+10                                                             
         MVC   SELLISTN,SVLINNUM                                                
*                                                                               
VRH010   DS    0H                                                               
         MVC   LISTAR,SPACES                                                    
*                                                                               
         CLI   EZMODE,EZBATL       PROCESS BATCH END                            
         BE    VRH500                                                           
*                                                                               
         CLI   EZMODE,EZINVP       PROCESS INVOICE                              
         BNE   VRHX                 NO                                          
*                                                                               
         LLC   R5,SELLISTN         GET LINE NUMBER                              
         MH    R5,=AL2(L'INVENT)                                                
         LA    R5,INVLIST(R5)                                                   
         USING INVLISTD,R5                                                      
*                                                                               
         CLC   EZINVSEQ,INVRSEQ    ARE WE AT RIGHT INVOICE?                     
         BL    VRH410              NOT REACHED IT YET                           
         BH    VRH400              PASSED IT ALREADY                            
*                                                                               
         DROP  R5                  INVLISTD,R5                                  
*                                                                               
         MVI   FOUNDSW,C'Y'                                                     
*                                                                               
         L     R5,EZWKRREC         INVOICE HEADER RECORD                        
         LA    R5,7(,R5)           SKIP RECLEN(4),RECCODE(2),DELIM(1)           
*                                                                               
         USING EZIHCNVS,R5                                                      
*                                                                               
         TM    EZIHCVST,EZIHIMQ                                                 
         BZ    *+8                                                              
         OI    IMSWITCH,IMSWIMQ    INDICATE CAME FROM IM                        
*                                                                               
         NI    EZIHCVST,X'FF'-EZIHCOVR                                          
*                                                                               
         OC    SVIHCVAD,SVIHCVAD   IS THERE AN OVERRIDE CLT?                    
         BZ    *+8                  NO                                          
         OI    EZIHCVST,EZIHCOVR   SET OVERRIDE SWITCH                          
*                                                                               
         CLI   RECONVSW,C'Y'       RECONVERT REQUEST                            
         BNE   *+8                                                              
         OI    EZIHCVST,EZIHRCVQ                                                
*                                                                               
         CLI   CONVRTSW,C'Y'       RECONVERT CANCEL?                            
         BNE   *+8                                                              
         NI    EZIHCVST,X'FF'-EZIHRCVQ                                          
*                                                                               
         CLI   RESTORSW,C'Y'       RESTORE REQUEST                              
         BNE   *+8                                                              
         NI    EZIHCVST,X'FF'-EZIHCDEL                                          
*                                                                               
         CLI   DELETESW,C'Y'       DELETE REQUEST                               
         BNE   *+8                                                              
         OI    EZIHCVST,EZIHCDEL                                                
*                                                                               
VRH190   DS    0H                                                               
         MVC   EZIHCVAD,SVIHCVAD                                                
         MVC   EZIHCVPR,SVIHCVPR                                                
         MVC   EZIHCVP2,SVIHCVP2                                                
         MVC   EZIHCVES,SVIHCVES                                                
         MVC   EZIHSPRT,SVIHSPRT                                                
         MVI   EZIHCVND,X'80' STOP DROP TRAILING BLANKS                         
         MVC   EZIHCVNP,SVIHCPKG  BINARY PACKAGE                                
*                                                                               
         DROP  R5                  EZIHCNVS,R5                                  
*                                                                               
         LHI   R0,36               COLUMN 36 - SECOND SAVE FIELD                
         L     R1,EZWKRREC         INVOICE HEADER RECORD                        
         BRAS  RE,FINDFLD                                                       
         BNE   VRH195                                                           
*                                                                               
         MVC   EZIHSFL2-EZIHSAV(L'EZIHSFL2,R1),SVIHFL2 SAVE FLAG2               
*                                                                               
* PROCESS THE REP CODE                                                          
*                                                                               
         MVC   EZIHSREP-EZIHSAV(L'EZIHSREP,R1),SVIHCREP    YES, SAVE IT         
         OC    SVIHCREP,SVIHCREP   USER OVERRIDE?                               
         BNZ   VRH193              YES - SKIP LOOKUP                            
*                                                                               
* HERE WE HAVE PROFILE-DERIVED REP CODE                                         
* SINCE THE CLIENT HAS CHANGED, WE NEED TO LOOK IT UP AGAIN                     
*                                                                               
         BRAS  RE,ISTRADE                                                       
         BNE   VRH193              INVOICE NOT TRADE                            
         BRAS  RE,GETTRREP                                                      
         OC    WORK,WORK                                                        
         BZ    VRH193                                                           
         MVC   DIVREP,WORK                                                      
         MVI   DIVDARF,C'#'        LOOKED UP FROM DAR                           
*                                                                               
         BRAS  RE,LKREP                                                         
         BNE   *+8                                                              
         MVI   DIVDARF,C'*'        FROM DAR, INVALID                            
*                                                                               
         OI    DIVREPH+6,X'80'                                                  
         OI    DIVDARFH+6,X'80'                                                 
*                                                                               
VRH193   DS    0H                                                               
         MVC   EZIHSPRD-EZIHSAV(L'EZIHSPRD,R1),SVIHCPRD                         
         MVC   EZIHSPR2-EZIHSAV(L'EZIHSPR2,R1),SVIHCPR2                         
         B     VRH196                                                           
*                                                                               
VRH195   DS    0H                                                               
         LA    R2,DIVPCDH                                                       
         CLC   SVIHCPRD,SPACES                                                  
         BH    INVPRER                                                          
*                                                                               
VRH196   DS    0H                                                               
         CLI   CHANGESW,C'Y'       WAS INVOICE CHANGED                          
         BNE   VRH420              NO                                           
*                                                                               
         LAY   R4,EZWRKIOB                                                      
         USING WRKIOD,R4                                                        
         MVI   WRKIACTN,WRKIAPUT                                                
         GOTO1 EZWRKIO,(R4)                                                     
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                  WRKIOB,R4                                    
*                                                                               
         TM    EZIHCVST,EZIHIMQ    CAME FROM IM?                                
         BZ    VRH420              NO - DON'T SEND DATA                         
         CLI   DELETESW,C'Y'                                                    
         BE    *+12                                                             
         CLI   RESTORSW,C'Y'                                                    
         BZ    *+8                                                              
         BRAS  RE,WRTIMGR                                                       
         B     VRH420                                                           
*                                                                               
VRH400   CLI   FOUNDSW,C'Y'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VRH410   MVI   EZMODE,EZINVL       SKIP TO NEXT INVOICE                         
*                                                                               
* CHECK EACH INVOICE - CONVERTED OR DELETED FOR KEEP OR UNKEEP STATUS *         
VRH420   DS    0H                                                               
         L     R5,EZWKRREC         INVOICE HEADER RECORD                        
         LA    R5,7(,R5)           SKIP RECLEN(4),RECCODE(2),DELIM(1)           
         USING EZIHCNVS,R5                                                      
*                                                                               
         MVC   BYTE,EZIHCVST                                                    
* TURN OFF ALL FLAGS EXCEPT CONV, DEL, RECONV.                                  
         NI    BYTE,EZIHCVQ+EZIHRCVQ+EZIHCDEL                                   
         OC    KPFLAGS,BYTE                                                     
         TM    EZIHCVST,EZIHCVQ+EZIHRCVQ+EZIHCDEL                               
         BNZ   VRHX                                                             
         OI    KPFLAGS,KPUNCONQ  INDICATE HAVE UNCONV INV IN BATCH              
         B     VRHX                                                             
*                                                                               
         DROP  R5                                                               
*                                                                               
* AT BATCH END, SET OR RESET BOTH CONVERTED AND KEEP STATUS *                   
* IF RESTORE, OR RECONVERT FOR BATCH IN KEEP STATUS, UNKEEP *                   
* IF DELETE, AND ALL OTHER INVOICES IN BATCH CONVERTED OR   *                   
* DELETED, SET BATCH CONVERT BIT ON AND STATUS TO KEEP      *                   
*                                                                               
VRH500   LAY   R4,EZWRKIOB                                                      
         USING WRKIOD,R4                                                        
*                                                                               
         TM    KPFLAGS,KPUNCONQ+EZIHRCVQ ANY UNCONV/RECONV IN BATCH?            
         BZ    VRH510                                                           
*                                                                               
* HAVE UNCONVERTED OR RECONVERTED INVOICES IN THIS BATCH                        
* MAKE SURE STATUS "KEEP" IS TURNED OFF                                         
         TM    WRKEZSTA,X'08'      IS IT IN "KEEP" STATUS?                      
         BZ    VRHX                NO - EXIT                                    
         MVI   WRKIACTN,WRKIAUKE   YES - TURN KEEP STATUS OFF                   
         B     VRH600                                                           
*                                                                               
* NO UNCONVERTED OR RECONVERTED INVOICES IN THIS BATCH                          
* MAKE SURE BATCH IS MARKED "KEEP"                                              
VRH510   DS    0H                                                               
         TM    WRKEZSTA,X'08'      IS IT ALREADY IN "KEEP" STATUS?              
         BO    VRHX                YES - EXIT                                   
         MVI   WRKIACTN,WRKIAKEE                                                
*                                                                               
VRH600   GOTO1 EZWRKIO,(R4)                                                     
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VRHX     B     XIT                                                              
         DROP  R4                                                               
*                                                                               
*                                                                               
*                                                                               
TSTRO    DS    0H                                                               
         CLI   OFFLINE,C'Y'                                                     
         BER   RE                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CXTRAINF-COMFACSD(RF)                                      
         BZR   RE                                                               
         USING XTRAINFD,RF                                                      
         TM    XIFLAG1,XITSTADV    TST?                                         
         BNZR  RE                  YES - WHO CARES?                             
         TM    XIFLAG1,XIROSYS+XIROMODE+XIWRONGF                                
         BZR   RE                                                               
         DROP  RF                                                               
*                                                                               
         LA    R2,CONRECH                                                       
         B     NOUPDER                                                          
*                                                                               
         DC    H'0'                MAY NOT RETURN FROM THE ERROR!               
*                                                                               
*                                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
*                                                                               
*                                                                               
NOESTFND L     R1,=A(NOESTMS)                                                   
         B     ERREXIT                                                          
INVEST   L     R1,=A(INVESTMS)                                                  
         B     ERREXIT                                                          
ESTENTER L     R1,=A(ESTENTMS)                                                  
         B     ERREXIT                                                          
MORPRDER L     R1,=A(MORPRDMS)                                                  
         B     ERREXIT                                                          
PRDSIZER L     R1,=A(PRDSIZMS)                                                  
         B     ERREXIT                                                          
PRDENTER L     R1,=A(PRDENTMS)                                                  
         B     ERREXIT                                                          
CONVRTER L     R1,=A(CONVRTMS)                                                  
         B     ERREXIT                                                          
CONRECER L     R1,=A(CONRECMS)                                                  
         B     ERREXIT                                                          
CMBRECER L     R1,=A(CMBRECMS)                                                  
         B     ERREXIT                                                          
DELRECER L     R1,=A(DELRECMS)                                                  
         B     ERREXIT                                                          
NOCONVER L     R1,=A(NOCONVMS)                                                  
         B     ERREXIT                                                          
NOCLTFND L     R1,=A(NOCLTMS)                                                   
         B     ERREXIT                                                          
NOPKG    L     R1,=A(NOPKGMSG)                                                  
         B     ERREXIT                                                          
INVREPER L     R1,=A(INVRPMSG)                                                  
         B     ERREXIT                                                          
CVTCHGER L     R1,=A(CVTCHGMS)                                                  
         LA    R2,DIVCCDH                                                       
         B     ERREXIT                                                          
CVTCHER2 L     R1,=A(CVTCHGM2)                                                  
         LA    R2,DIVCCDH                                                       
         B     ERREXIT                                                          
PGERR    L     R1,=A(PGERRMSG)                                                  
         LA    R2,DIVCCDH                                                       
         B     ERREXIT                                                          
*                                                                               
* CAN'T CHANGE RECS ON OTHER USERS *                                            
*                                                                               
UIDVRERR L     R1,=A(UIDVRMS)                                                   
         LA    R2,CONRECH                                                       
         B     ERREXIT                                                          
MOSVRERR L     R1,=A(MOSVRMS)                                                   
         LA    R2,CONRECH                                                       
         B     ERREXIT                                                          
CHGOKMSG XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'CHGOKMS),CHGOKMS                                       
         LA    R2,DIVCCDH                                                       
         B     ERREXITA                                                         
NOSPTCLT XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOSPCLMS),NOSPCLMS                                     
         MVC   CONHEAD+22(3),QCLT                                               
         B     ERREXITA                                                         
NOSPTPRD XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOSPPRMS),NOSPPRMS                                     
         MVC   CONHEAD+18(3),QPRD                                               
         B     ERREXITA                                                         
CNVDELER L     R1,=A(CNVDELMS)                                                  
         B     ERREXIT                                                          
SPRATERR L     R1,=A(SPRATMS)                                                   
         B     ERREXIT                                                          
ESTDATER L     R1,=A(ESTDATMS)                                                  
         B     ERREXIT                                                          
MAXIOSER L     R1,=A(MAXIOSMS)                                                  
         B     ERREXITC                                                         
NOINV    L     R1,=A(NOINVMSG)                                                  
         L     R2,SVR2                                                          
         B     ERREXIT                                                          
NOUPDER  L     R1,=A(NOCHANGE)                                                  
         B     ERREXIT                                                          
INVPRER  L     R1,=A(INVPRMSG)                                                  
         B     ERREXIT                                                          
*                                                                               
MISSCLT  L     R1,=A(MISSCMS)                                                   
*                                                                               
ERREXITC LA    R2,DIVCCDH                                                       
*                                                                               
ERREXIT  XC    CONHEAD,CONHEAD                                                  
         A     R1,RELO                                                          
         BCTR  R1,0                                                             
         CLI   0(R1),L'CONHEAD-1-10                                             
         BNH   *+6                                                              
         DC    H'0'                                                             
         LLC   RF,0(R1)                                                         
         EX    RF,ERREXITM                                                      
         MVC   CONHEAD(10),=C'* ERROR * '                                       
*                                                                               
ERREXITA GOTO1 ERREX2                                                           
ERREXITM MVC   CONHEAD+10(0),1(R1)                                              
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
BADATE   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
INVAL    MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
INVLFLD  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
CHGOKMS  DC    C'* IF CHANGES OK, HIT ENTER TO RESUME *'                        
         DC    AL1(L'NOSPCLMS-1)                                                
NOSPCLMS DC    C'* ERROR * CLIENT CODE XXX NOT ON SPOTPAK *'                    
         DC    AL1(L'NOSPPRMS-1)                                                
NOSPPRMS DC    C'* ERROR * PRODUCT XXX NOT ON SPOTPAK *'                        
         DC    AL1(L'SPRATMS-1)                                                 
SPRATMS  DC    C'SPECIAL RATES ARE - S, F, N, Q, V, X, P *'                     
         DC    AL1(L'MAXIOSMS-1)                                                
MAXIOSMS DC    C'BE MORE SPECIFIC, TOO MUCH DATA FOR ONLINE *'                  
         DC    AL1(L'ESTDATMS-1)                                                
ESTDATMS DC    C'ESTIMATE DATES OUTSIDE MONTH OF SERVICE *'                     
         DC    AL1(L'CNVDELMS-1)                                                
CNVDELMS DC    C'CAN NOT DELETE CONVERTED INVOICE *'                            
         DC    AL1(L'VFTRMS-1)                                                  
VFTRMS   DC    C'CAN''T USE CONVERT/UNCOVERT TOGETHER *'                        
         DC    AL1(L'VFTRMDMS-1)                                                
VFTRMDMS DC    C'VALID MEDIAS - T, R, N, X *'                                   
         DC    AL1(L'CCLENMS-1)                                                 
CCLENMS  DC    C'CLIENT CODE MUST BE 2 OR 3 CHARACTERS *'                       
         DC    AL1(L'PCLENMS-1)                                                 
PCLENMS  DC    C'PRODUCT CODE MUST BE 2 OR 3 CHARACTERS *'                      
         DC    AL1(L'INVLENMS-1)                                                
INVLENMS DC    C'INVOICE CAN''T BE MORE THAN 10 CHARACTERS *'                   
         DC    AL1(L'USIDLNMS-1)                                                
USIDLNMS DC    C'USER ID CAN''T BE MORE THAN 8 CHARACTERS *'                    
         DC    AL1(L'UIDVRMS-1)                                                 
UIDVRMS  DC    C'USER ID CAN''T DO CHANGE *'                                    
         DC    AL1(L'MOSVRMS-1)                                                 
MOSVRMS  DC    C'MONTH OF SERVICE INVALID, CALL DDS *'                          
         DC    AL1(L'MISSCMS-1)                                                 
MISSCMS  DC    C'CLIENT MUST BE ENTERED FOR PRODUCT *'                          
         DC    AL1(L'MISPRDMS-1)                                                
MISPRDMS DC    C'STATION REQUIRES PRODUCT *'                                    
         DC    AL1(L'NOESTMS-1)                                                 
NOESTMS  DC    C'NO ESTIMATE FOR THIS PROD(S) *'                                
         DC    AL1(L'INVESTMS-1)                                                
INVESTMS DC    C'INVALID ESTIMATE *'                                            
         DC    AL1(L'ESTENTMS-1)                                                
ESTENTMS DC    C'ESTIMATE MUST BE ENTERED LAST *'                               
         DC    AL1(L'MORPRDMS-1)                                                
MORPRDMS DC    C'ONLY PRD AND PTR ALLOWED *'                                    
         DC    AL1(L'PRDSIZMS-1)                                                
PRDSIZMS DC    C'PRD, PTR, EST MAX SIZE = 3 *'                                  
         DC    AL1(L'PRDENTMS-1)                                                
PRDENTMS DC    C'ENTER PRD/PRD,EST/PRD,PTR-EST'                                 
         DC    AL1(L'CONVRTMS-1)                                                
CONVRTMS DC    C'ENTER RECONVERT/CANCEL/RESTORE, OR BLANKS *'                   
         DC    AL1(L'NOCONVMS-1)                                                
NOCONVMS DC    C'NO CANCEL UNLESS RECONVERT REQUEST *'                          
         DC    AL1(L'CONRECMS-1)                                                
CONRECMS DC    C'INVOICE NOT CONVERTED YET *'                                   
         DC    AL1(L'CMBRECMS-1)                                                
CMBRECMS DC    C'INVOICE WAS COMBINED, NO RECONVERT *'                          
         DC    AL1(L'DELRECMS-1)                                                
DELRECMS DC    C'INVOICE NOT DELETED *'                                         
         DC    AL1(L'NOCLTMS-1)                                                 
NOCLTMS  DC    C'NO RECORD FOR 25 CHAR ADVERTISER CODE *'                       
         DC    AL1(L'CVTCHGMS-1)                                                
CVTCHGMS DC    C'NO CLIENT/PRODUCT/EST CHANGE IF CONVERTED *'                   
         DC    AL1(L'CVTCHGM2-1)                                                
CVTCHGM2 DC    C'INVOICE CONVERTED, NO CHANGES ALLOWED *'                       
         DC    AL1(L'NOPKGMSG-1)                                                
NOPKGMSG DC    C'CLIENT AND PRODUCT REQUIRED FOR PACKAGE *'                     
         DC    AL1(L'NOINVMSG-1)                                                
NOINVMSG DC    C'NO INVOICE NUMBER - CANNOT SELECT RECORD *'                    
         DC    AL1(L'NOCHANGE-1)                                                
NOCHANGE DC    C'NO UPDATES ALLOWED *'                                          
         DC    AL1(L'INVRPMSG-1)                                                
INVRPMSG DC    C'INVALID REP ID *'                                              
         DC    AL1(L'INVPRMSG-1)                                                
INVPRMSG DC    C'INVALID PRODUCT CODE *'                                        
         DC    AL1(L'PGERRMSG-1)                                                
PGERRMSG DC    C'CLIENT CODE NOT AUTHORIZED FOR THIS STATION'                   
         DROP  R7,RB,RC                                                         
*                                                                               
*                                                                               
SVBPRD   DS    X                                                                
SVQPRD   DS    CL3                                                              
COMPNUM  DS    X                                                                
PARSEPTR DC    AL1(1),C'/'         PARSNIP SEPARATOR OVERRIDE LIST              
         DC    AL1(3),C' ,-'                                                    
         DC    AL1(1),C','                                                      
         DC    AL1(1),C','                                                      
PBLOCK   DS    360X                                                             
*                                                                               
*                                                                               
***********************************************************************         
*   PREZ - FIND RECORD, CALL EZMOD                                    *         
***********************************************************************         
         DS    0H                                                               
PREZ     NMOD1 0,**PREZ**                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
*                                                                               
         BRAS  RE,INITEZB                                                       
         BRAS  RE,INITWKB                                                       
*                                                                               
         GOTO1 CALLOV,DMCB,(X'10',0),ATWA    GET EZMOD                          
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         ST    RF,VEZMOD                                                        
*                                                                               
         MVI   USEIO,C'Y'                                                       
         MVC   EZSPTNET,SPOTNETS                                                
*                                                                               
         CLI   RECNUM,X'12'        CONSUM                                       
         BE    *+12                                                             
         CLI   RECNUM,X'0F'        CONLIST                                      
         BNE   *+18                                                             
         CLI   SVLINNUM,X'00'                                                   
         BE    *+10                                                             
         MVC   SELLISTN,SVLINNUM                                                
*                                                                               
         LA    R5,INVLIST                                                       
         USING INVLISTD,R5                                                      
*                                                                               
         MVC   SVBUID,INVUID                                                    
         MVC   SVBWKFLN,INVBSEQ                                                 
         MVC   SVBINVSQ,INVRSEQ                                                 
*                                                                               
         LLC   RF,SELLISTN         RELATIVE LINE NUMBER                         
         MH    RF,=AL2(L'INVENT)                                                
         LA    R5,INVLIST(RF)                                                   
*                                                                               
         MVC   SRCESTA,INVSTA      STATION                                      
         GOTO1 VEQVSTA             GO GET EQUIVALENT STATION                    
         MVC   EZEQVSTA,EQUISTA                                                 
*                                                                               
         LAY   R4,EZWRKIOB                                                      
         USING WRKIOD,R4                                                        
*                                                                               
         XC    WRKEZKEY,WRKEZKEY                                                
         MVC   WRKEZUID,INVUID                                                  
         MVC   WRKEZSQN,INVBSEQ    BATCH SEQ NUMBER                             
         OI    WRKINDS,WRKISEQQ                                                 
         TM    FTRFLAG2,FTREXP                                                  
         BO    *+8                                                              
         OI    WRKINDS,WRKINOXQ                                                 
         MVI   WRKIACTN,WRKIANDX                                                
         GOTO1 EZWRKIO,(R4)                                                     
         CLI   WRKIERRS,WRKIEEOF                                                
         BE    BATMOVER                                                         
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R5                  INVLISTD,R5                                  
*                                                                               
         BRAS  RE,FAKEIND                                                       
*                                                                               
         MVI   WRKIACTN,WRKIAGET                                                
         GOTO1 EZWRKIO,(R4)                                                     
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVWCMNT,WRKEZDSC                                                 
         DROP  R4                  WRKIOB,R4                                    
*                                                                               
         L     RE,=A(DRHOOK)       DISPLAY HOOK ROUTINE                         
         CLI   MODE,DISPREC                                                     
         BE    *+8                                                              
         L     RE,=A(VRHOOK)       VALIDATE HOOK ROUTINE                        
         A     RE,RELO                                                          
         ST    RE,EZHOOK                                                        
*                                                                               
         MVI   FOUNDSW,C'N'                                                     
*                                                                               
         L     RF,4(,RD)           BACK UP CHAIN 1                              
         L     R7,48(,RF)                                                       
         L     RB,64(,RF)                                                       
*                                                                               
         GOTO1 VEZMOD,DMCB,(R6)                                                 
         J     XIT             RET AT BATCH END-DONE ON RET FROM EZMOD          
*                                                                               
BATMOVER L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BATMOVMS),BATMOVMS                                     
         LA    R2,LINSTAH                                                       
         GOTO1 ERREX2                                                           
*                                                                               
BATMOVMS DC    C'* ERROR * BATCH MOVED OR SYSTEM ERROR-CALL DDS *'              
         DS    0H                                                               
         DROP  RB,RC                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*   HEADHOOK ROUTINE FOR REPORT                                       *         
***********************************************************************         
*                                                                               
         DS    0H                                                               
HDHK     NMOD1 0,**HDHK**                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
*                                                                               
         CLI   EOJSW,C'Y'                                                       
         BE    HDHKX                                                            
*                                                                               
         MVC   H4+14(4),SVWCSRCE                                                
         MVC   H5+14(7),LSTA                                                    
         GOTO1 DATCON,DMCB,(2,SVWKDTEC),(5,H6+14)                               
         ICM   R0,15,SVWEZIND+W_FILENO-W_RECD                                   
         EDIT  (R0),(6,H6+29),COMMAS=YES                                        
*                                                                               
         MVC   H6+37(10),=C'** DONE **'                                         
         TM    SVWKSTAT,X'08'      BATCH FULLY CONVERTED/DELETED                
         BO    *+10                                                             
         MVC   H6+37(19),=C'** NOT CONVERTED **'                                
*                                                                               
HDHKX    J     XIT                                                              
         DROP  RB,RC                                                            
*                                                                               
*                                                                               
*                                                                               
HEADING  SSPEC H1,3,REQUESTOR                                                   
         SSPEC H2,3,C'EASI'                                                     
         SSPEC H4,3,C'SOURCE'                                                   
         SSPEC H5,3,C'STATION'                                                  
         SSPEC H6,3,C'BATCH DATE'                                               
         SSPEC H6,26,C'SEQ'                                                     
         SSPEC H1,52,C'INVOICE LIST'                                            
         SSPEC H2,52,C'------------'                                            
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         SSPEC H8,3,C'ADVERTISER NAME'                                          
         SSPEC H9,3,C'---------------'                                          
         SSPEC H8,29,C'CODE'                                                    
         SSPEC H9,29,C'----'                                                    
         SSPEC H8,35,C'PRODUCT NAME'                                            
         SSPEC H9,35,C'------------'                                            
         SSPEC H8,62,C'EST'                                                     
         SSPEC H9,62,C'---'                                                     
         SSPEC H8,67,C'PRD/PTR'                                                 
         SSPEC H9,67,C'-------'                                                 
         SSPEC H8,76,C'EST'                                                     
         SSPEC H9,76,C'---'                                                     
         SSPEC H8,81,C'INVOICE'                                                 
         SSPEC H9,81,C'-------'                                                 
         SSPEC H8,92,C'MONTH'                                                   
         SSPEC H9,92,C'-----'                                                   
         SSPEC H8,99,C'SPOTS'                                                   
         SSPEC H9,99,C'-----'                                                   
         SSPEC H8,110,C'DOLLARS'                                                
         SSPEC H9,110,C'-------'                                                
         SSPEC H8,119,C'STATUS'                                                 
         SSPEC H9,119,C'------'                                                 
         DC    X'00'                                                            
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*   DISLIN DISPLAY LINE ON LISTAR, OR PRINT LINE TO SPOOL             *         
***********************************************************************         
*                                                                               
         DS    0H                                                               
DISLIN   NMOD1 0,**DISL**                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
*                                                                               
         XC    LISTAR,LISTAR                                                    
         CLI   EZMODE,EZINVL       END OF INVOICE                               
         BE    DL200                                                            
*                                                                               
         CLI   EZMODE,EZINVP       PROCESS INVOICE                              
         BNE   DLX                                                              
*                                                                               
         OC    SVLSTBAT,SVLSTBAT   ANY LAST BATCH DATA                          
         BZ    DL100                NO JUST SEE IF WE WANT THIS ONE             
*                                                                               
* THIS IS A RESTART AT TOP OF SCREEN, SKIP TO NEXT INVOICE *                    
*                                                                               
         CLC   EZINVSEQ,SVBINVSQ   SAME INVOICE WITHIN BATCH                    
         BL    DL360                                                            
*        BNH   DL360                                                            
*                                                                               
DL100    DS    0H                                                               
         TM    FTRFLAG2,FTRIM      IM FILTER?                                   
         BZ    *+12                NO                                           
         TM    EZIHCVST,EZIHIMQ    CAME FROM IM?                                
         BZ    DL360               NO - SKIP IT                                 
*                                                                               
         TM    FTRFLAG2,FTRNONIM   NON-IM FILTER?                               
         BZ    *+12                NO                                           
         TM    EZIHCVST,EZIHIMQ    CAME FROM IM?                                
         BO    DL360               YES, SKIP IT                                 
*                                                                               
         CLI   FTRCLTN,0           TEST HAVE CLIENT NAME FILTER                 
         BNH   DL140                                                            
         LLC   RF,FTRCLTNL         LENGTH - 1                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZIHADVN(0),FTRCLTN                                              
         BNE   DL360                                                            
*                                                                               
DL140    CLI   FTRMOS,0            TEST HAVE MONTH OF SERVICE FILTER            
         BNH   DL150                                                            
         CLC   EZIHDMOS,FTRMOS                                                  
         BNE   DL360                                                            
*                                                                               
DL150    CLI   FTRPRDN,0           TEST HAVE PROD FILTER                        
         BNH   DL160                                                            
         LLC   RF,FTRPRDNL         TEST PRODUCT FILTER                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZIHPRDN(0),FTRPRDN                                              
         BNE   DL360                                                            
*                                                                               
DL160    CLI   FTRINVNO,0          TEST HAVE INVOICE FILTER                     
         BNH   DL170                                                            
         LLC   RF,FTRINVLN         GET INVNO LENGTH                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZIHINV(0),FTRINVNO                                              
         BNE   DL360                                                            
*                                                                               
DL170    CLI   FTRDATE,0           FILTER ON CONVERTED DATE                     
         BNH   DL180                                                            
*                                                                               
         OC    EZIHCVDT,EZIHCVDT   IS THIS CONVERTED                            
         BZ    DL360                NO                                          
*                                                                               
         CLI   FTRDATES,0          FILTER ON CONVERTED DATE RANGE               
         BNE   DL174                                                            
*                                                                               
         CLC   EZIHCVDT,FTRDATE    FILTER ON EXACT DATE                         
         BNE   DL360                                                            
         B     DL180                                                            
DL174    CLI   FTRDATES,C'+'       PLUS                                         
         BE    DL176                                                            
         CLI   FTRDATES,C'-'       MINUS                                        
         BE    DL178                                                            
         DC    H'0'                                                             
DL176    CLC   EZIHCVDT,FTRDATE                                                 
         BL    DL360                                                            
         B     DL180                                                            
DL178    CLC   EZIHCVDT,FTRDATE                                                 
         BH    DL360                                                            
*                                                                               
DL180    TM    FTRFLAG,FTRALL      ALLOW ALL OR INVOICE #                       
         BO    DL196                YES                                         
*                                                                               
         TM    FTRFLAG2,FTRMID     MID FLIGHT CLEARANCES ONLY                   
         BZ    DL181                NO                                          
*                                                                               
         CLC   =C'MID FLIGHT CL',EZIHORD THIS SPECIAL 'PRE' INVOICE             
         BNE   DL360                                                            
*                                                                               
DL181    DS    0H                                                               
         TM    FTRFLAG,FTRDONE     DONE ONLY                                    
         BZ    DL182                                                            
         TM    EZIHCVST,EZIHRCVQ   RECONVERT                                    
         BO    DL360                                                            
         TM    EZIHCVST,EZIHCVQ    CONVERTED                                    
         BO    DL196                YES                                         
         TM    EZIHCVST,EZIHCDEL   DELETED                                      
         BO    DL196                YES                                         
         B     DL360                                                            
*                                                                               
DL182    TM    FTRFLAG,FTRUCVQ     UNCONVERTED ONLY                             
         BZ    DL183                YES                                         
*                                                                               
* IF CONVERTED, DELETED, OR EVEN A RECONVERT (HAS CONVERT ON) BYPASS *          
*                                                                               
         TM    EZIHCVST,EZIHCVQ+EZIHCDEL   CONVERTED OR DELETED                 
         BNZ   DL360                        YES, BYPASS                         
         B     DL196                                                            
*                                                                               
DL183    TM    FTRFLAG,FTRRCVQ     RECONVERT ONLY                               
         BZ    DL184                                                            
         TM    EZIHCVST,EZIHRCVQ   RECONVERT                                    
         BO    DL196                YES                                         
         B     DL360                                                            
*                                                                               
DL184    TM    FTRFLAG,FTRCVQ      CONVERTED ONLY                               
         BZ    DL186                                                            
         TM    EZIHCVST,EZIHRCVQ   RECONVERT                                    
         BO    DL360                                                            
         TM    EZIHCVST,EZIHCVQ    CONVERTED                                    
         BO    DL196                YES                                         
         B     DL360                                                            
*                                                                               
DL186    TM    FTRFLAG,FTROVR      OVERRIDES ONLY                               
         BZ    DL188                                                            
         TM    EZIHCVST,EZIHCOVR   OVERRIDE                                     
         BO    DL196                YES                                         
         B     DL360                                                            
*                                                                               
DL188    TM    FTRFLAG,FTRDEL      DELETES ONLY                                 
         BZ    DL190                                                            
         TM    EZIHCVST,EZIHCDEL   DELETED                                      
         BZ    DL360                NO, BYPASS                                  
         B     DL196                                                            
*                                                                               
DL190    TM    EZIHCVST,EZIHCDEL   DELETED                                      
         BO    DL360                YES, BYPASS                                 
*                                                                               
DL196    MVC   SVINVSEQ,EZRECSEQ     SAVE INVOICE HEADER SEQ                    
         B     DLX                                                              
*                                                                               
* HAVE FILTERED AT INVOICE HEADER, NOW HAVE HEADER AND TOTAL *                  
*                                                                               
DL200    GOTO1 DATCON,DMCB,(2,SVWKDTEC),(4,LBDTE)                               
*                                                                               
*        SR    R0,R0                                                            
*        ICM   R0,3,SVWKFILN                                                    
*        ICM   R0,15,EZWKRIND+W_FILENO-W_RECD                                   
         ICM   R0,15,SVWEZIND+W_FILENO-W_RECD                                   
         EDIT  (R0),LBSEQ                                                       
*                                                                               
         OC    EZIHCVDT,EZIHCVDT                                                
         BZ    DL210                                                            
         GOTO1 DATCON,DMCB,(1,EZIHCVDT),(4,LCDTE)                               
*                                                                               
         TM    EZIHCVST,EZIHCVQ    THIS BETTER BE CONVERTED                     
         BO    *+6                                                              
         DC    H'0'                OOPS!                                        
*                                                                               
DL210    MVC   LSRCE,SVWCSRCE                                                   
         MVC   LMON(3),EZIHDMOS                                                 
         MVC   LMON+3(2),EZIHDMOS+4                                             
         MVC   LSTA(7),PRTSTA7C                                                 
*                                                                               
* BUILD STATION MASTER KEY                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(L'STAKEY-1),KEY                                            
*                                                                               
         MVI   KEY,C'S'                                                         
*                                                                               
         MVC   KEY+1(1),EQVMED                                                  
*                                                                               
         MVC   KEY+2(5),EQUISTA                                                 
         OI    KEY+5,X'40'         FORCE BLANK IF BINARY ZERO                   
*                                                                               
         MVC   KEY+7(2),AGENCY                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO1                     
         CLI   8(R1),0             ANY ERROR                                    
         BE    *+8                  NO                                          
         MVI   LSTA+7,C'*'         INDICATE INVALID STATION                     
*                                                                               
         TM    FTRFLAG2,SHOWEQU                                                 
         BZ    DL212                                                            
         CLC   EQUISTA,SRCESTA                                                  
         BE    DL212                                                            
         MVI   LSTA+7,C'E'                                                      
         NI    LSTA+7,X'FF'-X'40'  LOWERCASE                                    
*                                                                               
DL212    DS    0H                                                               
         CLC   EZIHINST,=CL25'BILLING INVOICE REVERSAL'                         
         BNE   DL214                                                            
         MVI   LDEL,C'D'                                                        
*                                                                               
DL214    OC    EZIHAAID,EZIHAAID   TEST HAVE AGENCY ADVERTISER CODE             
         BZ    DL230                                                            
*                                                                               
         MVC   LCLT,EZIHAAID                                                    
*                                                                               
         CLI   EZIHAAID,C'A'                                                    
         BL    DL216                                                            
         CLI   EZIHAAID,C'Z'                                                    
         BH    DL216                                                            
         CLI   EZIHAAID+1,C'A'                                                  
         BL    DL216                                                            
         CLI   EZIHAAID+1,C'Z'                                                  
         BH    DL216                                                            
         CLI   EZIHAAID+3,C' '                                                  
         BNH   DL220                                                            
*                                                                               
DL216    MVI   LCLT+3,C'*'                                                      
*                                                                               
DL220    OC    EZIHAPID,EZIHAPID   TEST HAVE AGENCY PRODUCT CODE                
         BZ    DL224                                                            
         MVC   LPRD(L'EZIHAPID),EZIHAPID                                        
         CLI   EZIHAPID,C'A'                                                    
         BL    DL222                                                            
         CLI   EZIHAPID,C'Z'                                                    
         BH    DL222                                                            
         CLI   EZIHAPID+1,C'A'                                                  
         BL    DL222                                                            
         CLI   EZIHAPID+1,C'Z'                                                  
         BNH   DL221                                                            
*                                                                               
         CLI   SPOTNETS,C'N'       THIS NET                                     
         BNE   DL222                                                            
         CLI   EZIHAPID+1,C'0'                                                  
         BL    DL222                                                            
         CLI   EZIHAPID+1,C'9'                                                  
         BH    DL222                                                            
*                                                                               
DL221    DS    0H                                                               
         CLI   EZIHAPID+3,C' '                                                  
         BNH   DL224                                                            
*                                                                               
DL222    DS    0H                                                               
         MVI   LPRD+3,C'*'                                                      
         MVC   LPRD+4(4),SPACES                                                 
*                                                                               
DL224    CLC   EZIHEST,SPACES      TEST HAVE AGENCY ESTIMATE CODE               
         BNH   DL230                                                            
*                                                                               
         MVC   LEST,EZIHEST                                                     
         OC    LEST,SPACES                                                      
*                                                                               
         LA    R1,LEST                                                          
         LHI   R0,3                                                             
*                                                                               
         BRAS  RE,ISNUMSPC                                                      
         BNE   DL226                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
         B     *+8                                                              
*                                                                               
DL226    MVI   LEST+3,C'*'                                                      
*                                                                               
DL230    TM    EZIHCVST,EZIHCVQ    TEST CONVERTED                               
         BO    *+12                 YES                                         
         TM    EZIHCVST,EZIHCOVR   TEST OVERRIDE CLT/PRD/EST                    
         BZ    DL250                NO                                          
*                                                                               
         OC    EZIHCVAD,EZIHCVAD   TEST HAVE CLIENT CODE                        
         BNZ   *+6                                                              
         DC    H'0'           OVERRIDE FLAG SET, BUT NO CLIENT CODE             
         MVC   LCLT(16),SPACES                                                  
*                                                                               
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
* SIMULATE GOTO1 VALICLT                                                        
*                                                                               
         OC    FUIDNUM,FUIDNUM     HAS ID BEEN SET AS OPTION                    
         BNZ   DL250                                                            
*                                                                               
         LA    R1,EQVMED                                                        
         ICM   R1,8,=AL1(EZMTMEDQ)                                              
         GOTO1 VGETMED                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    BAGYMD,X'F0'        TURN OFF MEDIA BITS                          
         OC    BAGYMD,EZMTHEX-EZMEDTBD(RF) GET HEX MEDIA FROM MEDTAB            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
*                                                                               
         MVC   KEY+2(2),EZIHCVAD                                                
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     BIG BAD ERROR                                
*        BE    *+6                 NO, OKAY                                     
*        DC    H'0'                                                             
         BE    *+14                CLIENT FOUND, PROCEED                        
         MVC   LCLT(3),=C'???'     CLIENT NOT ON FILE - DISPLAY '???'           
         B     DL250               SKIP PRODUCT, EST DISPLAY                    
*                                                                               
         L     R1,AIO1                                                          
         ST    R1,AIO                                                           
         MVC   FILENAME+3(3),=C'FIL'                                            
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
* SAVE CLIENT PRODUCT LIST *                                                    
*                                                                               
         LA    R4,CLIST-CLTHDR(R1)                                              
         LA    R5,880                                                           
         LA    RE,SVCLIST                                                       
         LR    RF,R5                                                            
         MVCL  RE,R4                                                            
*                                                                               
         LA    R4,CLIST2-CLTHDR(R1)                                             
         LHI   R5,140              LENGTH OF CLIST2                             
         LR    RF,R5                                                            
         CLC   CLEN-CLTHDR(2,R1),=H'1280'   TEST CLIST2 IN RECORD               
         BH    *+8                 YES                                          
         SR    R5,R5               ELSE JUST CLEAR REST OF LIST                 
         SR    RF,RF                                                            
         MVCL  RE,R4                                                            
*                                                                               
         MVC   BYTE,CPROF+6-CLTHDR(R1)                                          
         GOTO1 CLUNPK,DMCB,(BYTE,EZIHCVAD),LCLT                                 
*                                                                               
         CLI   EZIHCVPR,0          TEST HAVE PRODUCT                            
         BE    DL233                NO                                          
         LA    RE,255                                                           
         LA    RF,SVCLIST                                                       
*                                                                               
DL232    CLC   EZIHCVPR,3(RF)                                                   
         BE    DL234                                                            
         LA    RF,4(,RF)                                                        
         CLI   0(RF),C' '                                                       
         BNH   *+8                                                              
         BCT   RE,DL232                                                         
         LA    RF,=C'???'                                                       
         B     DL234                                                            
*                                                                               
DL233    DS    0H                  NO 1-BYTE PRD OVERRIDE HERE                  
         CLI   SPOTNETS,C'N'       THIS NET                                     
         BNE   DL250               SPOT = NO PRODUCT                            
*                                                                               
         CLC   EZIHSPRD,SPACES                                                  
         BNH   DL250                                                            
         LA    RF,EZIHSPRD                                                      
*                                                                               
DL234    MVC   LPRD,0(RF)                                                       
*                                                                               
         CLI   EZIHCVP2,0          TEST HAVE PRODUCT 2                          
         BE    DL237                                                            
         LA    RE,255                                                           
         LA    RF,SVCLIST                                                       
*                                                                               
DL236    CLC   EZIHCVP2,3(RF)                                                   
         BE    DL238                                                            
         LA    RF,4(,RF)                                                        
         CLI   0(RF),C' '                                                       
         BNH   *+8                                                              
         BCT   RE,DL236                                                         
         LA    RF,=C'???'                                                       
         B     DL238                                                            
*                                                                               
DL237    DS    0H                                                               
         CLI   SPOTNETS,C'N'       THIS NET                                     
         BNE   DL240               SPOT = NO PRODUCT                            
*                                                                               
         CLC   EZIHSPR2,SPACES                                                  
         BNH   DL240                                                            
         LA    RF,EZIHSPR2                                                      
*                                                                               
DL238    MVC   LPTR,0(RF)                                                       
*                                                                               
DL240    DS    0H                                                               
         CLI   EZIHCVES,0                                                       
         BE    DL250                                                            
         LLC   RE,EZIHCVES                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LEST,DUB                                                         
*                                                                               
DL250    OC    LCLT(16),SPACES                                                  
         CLI   FTRQCLT,0           FILTER ON CLIENT CODE                        
         BE    *+14                                                             
         CLC   LCLT,FTRQCLT                                                     
         BNE   DL360                                                            
*                                                                               
         CLI   FTRQPRD,0           FILTER ON PRODUCT CODE                       
         BE    *+14                                                             
         CLC   LPRD,FTRQPRD                                                     
         BNE   DL360                                                            
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    DL260                                                            
         CLC   LCLT(16),SPACES     ANY CLT/PRD/EST FOUND                        
         BNE   DL260                YES                                         
         MVC   LCLT(15),EZIHADVN                                                
*                                                                               
DL260    DS    0H                                                               
         MVC   LINV,EZIHINV                                                     
         OC    LINV,SPACES                                                      
         CLC   LINV,SPACES                                                      
         BNE   *+10                                                             
         MVC   LINV,=10C'*'                                                     
*                                                                               
         CLC   EZIHTSPN,=AL4(9999)                                              
         BNH   DL260A                                                           
*                                                                               
         MVC   LSPTS,=C'>10K'                                                   
         NI    LSPTS+3,X'BF'                                                    
         B     DL260B                                                           
*                                                                               
DL260A   DS    0H                                                               
         EDIT  (B4,EZIHTSPN),LSPTS,ZERO=NOBLANK                                 
*                                                                               
DL260B   DS    0H                                                               
         TM    EZIHCVST,EZIHCVQ    TEST CONVERTED                               
         BZ    *+8                  NO                                          
         MVI   LCVQ,C'C'                                                        
*                                                                               
         TM    EZIHCVST,EZIHCDEL   TEST DELETED                                 
         BZ    *+8                  NO                                          
         MVI   LCVQ,C'D'                                                        
*                                                                               
         TM    EZIHCVST,EZIHRCVQ   TEST RECONVERT                               
         BZ    *+8                  NO                                          
         MVI   LRCVQ,C'R'                                                       
*                                                                               
         TM    EZIHCVST,EZIHCOVR   TEST OVERRIDE CLT/PRD/EST                    
         BZ    *+8                  NO                                          
         MVI   LOVR,C'O'                                                        
*                                                                               
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   DL280                                                            
*                                                                               
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   DL280                                                            
         TM    EZIHCVST,EZIHIMQ                                                 
         BZ    DL280                                                            
         MVI   LIMFLAG,C'I'                                                     
         NI    LIMFLAG,X'BF'       LOWERCASE                                    
*                                                                               
DL280    LM    RE,R0,BTOT                                                       
         LA    RE,1(,RE)                                                        
         A     RF,EZIHTSPN                                                      
         ICM   R1,15,EZITBDUE                                                   
         AR    R0,R1                                                            
         STM   RE,R0,BTOT                                                       
*                                                                               
         CLI   MODE,PRINTREP       DON'T DO IT FOR PRINTED REPORT               
         BE    DL290                                                            
*                                                                               
         XC    LINTL,LINTL                                                      
         OI    LINTLH+6,X'80'                                                   
*                                                                               
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   DL284                                                            
         EDIT  (B4,IDXRDCT),(6,LINTL),COMMAS=YES                                
         MVC   LINTL+6(6),=C'=INDEX'                                            
         EDIT  (B4,RECRDCT),(6,LINTL+13),COMMAS=YES                             
         MVC   LINTL+19(5),=C'=GETS'                                            
*                                                                               
DL284    L     R5,INVLPTR          POINTER INTO SAVE LIST                       
         LA    R0,WRKFBUFR                                                      
         CR    R0,R5                                                            
         BH    *+6                                                              
         DC    H'0'                                                             
         USING INVLISTD,R5                                                      
*                                                                               
         MVC   INVMEDIA,EQVMED                                                  
*                                                                               
         MVC   INVUID,SVWEZIND                                                  
         MVC   INVSTA,SVSTA                                                     
         MVC   INVBDTE,SVWKDTEC                                                 
         MVC   INVBSEQ,SVWEZIND+W_FILENO-W_RECD                                 
         MVC   INVRSEQ,EZINVSEQ                                                 
         MVC   INVNET,EZIHNET                                                   
         LA    R5,INVNEXT                                                       
         ST    R5,INVLPTR                                                       
         DROP  R5                                                               
*                                                                               
         MVC   DMDSKADD(4),=X'0000FFFF'   DUMMY DISK ADDRESS                    
         MVI   NLISTS,NUMLINS                                                   
         MVC   SVBUID,SVWEZIND                                                  
         MVC   SVBWKFLN,SVWEZIND+W_FILENO-W_RECD                                
         MVC   SVBINVSQ,EZINVSEQ                                                
*                                                                               
         GOTO1 LISTMON                                                          
*                                                                               
         B     DLX                                                              
*                                                                               
DL290    MVC   PADVNM,EZIHADVN                                                  
         MVC   PADVCD,LCLT                                                      
         MVC   PPRDNM,EZIHPRDN                                                  
         MVC   PESTO,EZIHEST                                                    
         MVC   PPRDCD,LPRD                                                      
         MVC   PEST,LEST                                                        
         MVC   PINV,EZIHINV                                                     
*                                                                               
         MVC   PMON(3),EZIHDMOS                                                 
         MVC   PMON+3(2),EZIHDMOS+4                                             
*                                                                               
         EDIT  (B4,EZIHTSPN),(5,PSPTS),COMMAS=YES,ZERO=NOBLANK                  
         EDIT  (B4,EZITBDUE),(13,PDOLS),2,COMMAS=YES,ZERO=NOBLANK,     C        
               MINUS=YES                                                        
*                                                                               
         MVC   PSTAT(1),LRCVQ                                                   
         MVC   PSTAT+1(1),LOVR                                                  
         TM    EZIHCVST,EZIHCVQ                                                 
         BZ    DL310                                                            
         OC    EZIHCVDT,EZIHCVDT   CONVERTED DATE                               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   PSTAT+2,C'C'                                                     
         MVI   PSTAT+3,C'-'                                                     
         GOTO1 DATCON,DMCB,(1,EZIHCVDT),(5,PSTAT+4)                             
*                                                                               
DL310    CLI   LCLT+3,C'*'                                                      
         BNE   *+16                                                             
         MVC   PADVCD+3(1),EZIHAAID+3                                           
         MVC   PADVCD+132,=C'***'                                               
*                                                                               
         CLI   LPRD+3,C'*'                                                      
         BNE   *+16                                                             
         MVC   PPRDCD+3(1),EZIHAPID+3                                           
         MVC   PPRDCD+132,=C'***'                                               
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   DLX                                                              
         MVC   PSTAT(3),=C'IH='                                                 
         EDIT  (B2,SVINVSEQ),(5,PSTAT+3),ALIGN=LEFT                             
         MVC   PSTAT+8(3),=C'IT='                                               
         EDIT  (B2,EZRECSEQ),(5,PSTAT+11),ALIGN=LEFT                            
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         B     DLX                                                              
*                                                                               
DL360    XC    LISTAR,LISTAR                                                    
         MVI   EZMODE,EZINVL       SKIP TO NEXT INVOICE                         
*                                                                               
DLX      J     XIT                                                              
*                                                                               
         LTORG                                                                  
         DROP  RB,RC                                                            
*                                                                               
***********************************************************************         
* VALIDATE FILTERS (IF ANY)                                           *         
***********************************************************************         
*                                                                               
         DS    0H                                                               
VFTR     NMOD1 0,**VFTR**                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
         XC    FILTERS,FILTERS                                                  
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VFTRX               NO                                           
*                                                                               
         TM    WHENOK,X'78'        ONLY ALLOWED ONLINE                          
         BM    *+8                                                              
         B     FTRTIMER                                                         
*                                                                               
         CLI   8(R2),C'?'          HELP                                         
         BE    VFTRHLP             YES                                          
         CLI   5(R2),4                                                          
         BNH   VFTR02                                                           
         LA    R1,3                                                             
         B     VFTR04                                                           
VFTR02   LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
VFTR04   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL4'HELP'                                               
         BE    VFTRHLP                                                          
         LA    R0,25               NON-STANDARD LENGTH                          
         MVI   BYTE,1                                                           
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
         GOTO1 SCANNER,DMCB,((R0),(R2)),(5,(R4)),0                              
         CLI   4(R1),0                                                          
         BE    MISSERRA             SCANNER DIDN'T FIND ANYTHING                
         SR    R3,R3                                                            
         ICM   R3,1,DMCB+4         GET NUMBER OF BLOCKS                         
VFTR10   LLC   R1,0(R4)            GET LENGTH                                   
*                                                                               
         CH    R1,=H'2'                                                         
         BL    FTRLENER                                                         
*                                                                               
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         CLI   0(R5),C'+'          PLUS                                         
         BE    VFTR12               YES, SAVE IT                                
         CLI   0(R5),C'-'          MINUS                                        
         BNE   VFTR14               NO, NETHER                                  
VFTR12   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
*                                                                               
VFTR14   EX    R1,VFTRCLCA         ACTIVITY DATE                                
         BNE   VFTR16                                                           
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),WORK                                        
         OC    DMCB(4),DMCB        WAS DATE VALID                               
         BZ    BADATEA              NO                                          
         GOTO1 DATCON,(R1),(0,WORK),(1,FTRDATE)                                 
         MVC   FTRDATES,HOLDSIGN                                                
         B     VFTR92                                                           
*                                                                               
VFTR16   EX    R1,VFTRCLCQ         ALL                                          
         BNE   VFTR17                                                           
         OI    FTRFLAG,FTRALL                                                   
         B     VFTR92                                                           
*                                                                               
VFTR17   EX    R1,VFTRCLCV         IM?                                          
         BNE   VFTR17A                                                          
         OI    FTRFLAG2,FTRIM                                                   
         B     VFTR92                                                           
*                                                                               
VFTR17A  EX    R1,VFTRCLCW         NON-IM?                                      
         BNE   VFTR18                                                           
         OI    FTRFLAG2,FTRNONIM                                                
         B     VFTR92                                                           
*                                                                               
VFTR18   EX    R1,VFTRCLCR         MID - MID FLIGHT CL                          
         BNE   VFTR19                                                           
         OI    FTRFLAG2,FTRMID                                                  
         B     VFTR92                                                           
*                                                                               
VFTR19   EX    R1,VFTRCLCS         DISPLAYING EXPIRED DATA?                     
         BNE   VFTR20                                                           
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   VFTRERR                                                          
         OI    FTRFLAG2,FTREXP                                                  
         B     VFTR92                                                           
*                                                                               
VFTR20   EX    R1,VFTRCLCB         CLIENT CODE (CC)                             
         BNE   VFTR21                                                           
         CLI   1(R4),2                                                          
         BL    CCLENER                                                          
         CLI   1(R4),3                                                          
         BH    CCLENER                                                          
         MVC   FTRQCLT,22(R4)                                                   
         B     VFTR90                                                           
*                                                                               
VFTR21   EX    R1,VFTRCLCX         INDICATING EQUIVALENT STATIONS?              
         BNE   VFTR22                                                           
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   VFTRERR                                                          
         OI    FTRFLAG2,SHOWEQU                                                 
         B     VFTR92                                                           
*                                                                               
VFTR22   EX    R1,VFTRCLCC         CLIENT NAME (CN)                             
         BNE   VFTR24                                                           
         MVC   FTRCLTN,22(R4)                                                   
         LLC   R0,1(R4)                                                         
         BCTR  R0,0                                                             
         STC   R0,FTRCLTNL                                                      
         B     VFTR90                                                           
*                                                                               
VFTR24   EX    R1,VFTRCLCD         PRODUCT CODE (PC)                            
         BNE   VFTR26                                                           
         CLI   1(R4),2                                                          
         BL    PCLENER                                                          
         CLI   1(R4),3                                                          
         BH    PCLENER                                                          
         MVC   FTRQPRD,22(R4)                                                   
         B     VFTR90                                                           
*                                                                               
VFTR26   EX    R1,VFTRCLCE         PRODUCT NAME (PN)                            
         BNE   VFTR30                                                           
         MVC   FTRPRDN,22(R4)                                                   
         LLC   R0,1(R4)                                                         
         BCTR  R0,0                                                             
         STC   R0,FTRPRDNL                                                      
         B     VFTR90                                                           
*                                                                               
VFTR30   EX    R1,VFTRCLCF         CONVERTED (CONV)                             
         BNE   VFTR32                                                           
         TM    FTRFLAG,FTRUCVQ                                                  
         BNZ   VFTRERRC                                                         
         OI    FTRFLAG,FTRCVQ                                                   
         B     VFTR92                                                           
*                                                                               
VFTR32   EX    R1,VFTRCLCG         RECONVERTS (RECONV)                          
         BNE   VFTR34                                                           
         OI    FTRFLAG,FTRRCVQ                                                  
         B     VFTR90                                                           
*                                                                               
VFTR34   EX    R1,VFTRCLCH         UNCONVERTED (UNCONV)                         
         BNE   VFTR36                                                           
         TM    FTRFLAG,FTRCVQ                                                   
         BNZ   VFTRERRC                                                         
         OI    FTRFLAG,FTRUCVQ                                                  
         B     VFTR92                                                           
*                                                                               
VFTR36   EX    R1,VFTRCLCO         OVERRIDES                                    
         BNE   VFTR40                                                           
         OI    FTRFLAG,FTROVR                                                   
         B     VFTR92                                                           
*                                                                               
VFTR40   EX    R1,VFTRCLCI         INVOICE                                      
         BNE   VFTR50                                                           
         CLI   1(R4),10                                                         
         BH    INVLENER                                                         
         MVC   FTRINVNO,22(R4)                                                  
         LLC   R0,1(R4)                                                         
         BCTR  R0,0                                                             
         STC   R0,FTRINVLN                                                      
         OI    FTRFLAG,FTRALL                                                   
         B     VFTR90                                                           
*                                                                               
VFTR50   EX    R1,VFTRCLCJ         SOURCE                                       
         BNE   VFTR54                                                           
         CLI   1(R4),4                                                          
         BH    SRCLENER                                                         
         MVC   FTRSRCE,22(R4)                                                   
         B     VFTR92                                                           
*                                                                               
VFTR54   EX    R1,VFTRCLCK         DONE (MEANS CONV/DEL INC KEEP)               
         BNE   VFTR60                                                           
         OI    FTRFLAG,FTRDONE                                                  
         B     VFTR90                                                           
*                                                                               
VFTR60   EX    R1,VFTRCLCL         BATCH DATE(S)                                
         BNE   VFTR66                                                           
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),WORK                                        
         ICM   RE,15,DMCB          WAS DATE VALID                               
         BZ    BADATEA              NO                                          
         MVC   WORK+6(6),WORK                                                   
         CLM   RE,1,1(R4)          WAS ONLY 1 DATE ENTERED                      
         BE    VFTR64                                                           
         LA    R5,1(RE,R5)                                                      
         GOTO1 (RF),(R1),(0,(R5)),WORK+6                                        
         OC    DMCB,DMCB                                                        
         BZ    BADATEA                                                          
*                                                                               
VFTR64   GOTO1 DATCON,(R1),(0,WORK),(2,FTRBSDT)                                 
         GOTO1 (RF),(R1),(0,WORK+6),(2,FTRBEDT)                                 
         B     VFTR92                                                           
*                                                                               
VFTR66   EX    R1,VFTRCLCM         MEDIA                                        
         BNE   VFTR70                                                           
         LA    R5,22(,R4)                                                       
         CLI   0(R5),C'T'          TV                                           
         BE    VFTR68                                                           
         CLI   0(R5),C'R'          RADIO                                        
         BE    VFTR68                                                           
         CLI   0(R5),C'N'          NETWORK                                      
         BE    VFTR68                                                           
         CLI   0(R5),C'X'          NETWORK RADIO                                
         BNE   VFTRMDER                                                         
*                                                                               
VFTR68   MVC   FTRMEDIA,0(R5)                                                   
         B     VFTR92                                                           
*                                                                               
VFTR70   EX    R1,VFTRCLCN         TRACE                                        
         BNE   VFTR72                                                           
         OI    FTRFLAG,FTRTRACE                                                 
         B     VFTR92                                                           
*                                                                               
VFTR72   EX    R1,VFTRCLCT         DELETE                                       
         BNE   VFTR74                                                           
         OI    FTRFLAG,FTRDEL                                                   
         B     VFTR92                                                           
*                                                                               
VFTR74   EX    R1,VFTRCLCU         USER ID                                      
         BNE   VFTR80                                                           
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFFLINE, ACCEPT                           
         BE    *+12                                                             
         CLI   TWAOFFC,C'*'        ONLINE - ONLY FOR DDS TERMS                  
         BNE   VFTRHLP                                                          
*                                                                               
         CLI   1(R4),8                                                          
         BH    USIDLNER                                                         
         MVC   FUID,22(R4)                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),SPACES                                                
         LLC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   KEY+15(0),22(R4)                                                 
         L     R6,AIO1                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,(R6)                    
         CLI   8(R1),0                                                          
         BNE   USERIDER                                                         
*                                                                               
         MVI   ELCODE,X'02'                                                     
         MVC   DATADISP,=AL2(28)                                                
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FUIDNUM,2(R6)       BINARY USER ID (ORIGIN)                      
         B     VFTR92                                                           
*                                                                               
VFTR80   EX    R1,VFTRCLCP         MOS - MONTH OF SERVICE                       
         BNE   VFTRERR                                                          
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),WORK                                        
         ICM   RE,15,DMCB          WAS MO/DA/YR DATE VALID                      
         BNZ   MOSERR               NO, ERROR                                   
*                                                                               
         GOTO1 (RF),(R1),(2,(R5)),WORK                                          
         ICM   RE,15,DMCB          WAS MO/YR DATE VALID                         
         BZ    MOSERR               NO, ERROR                                   
*                                                                               
         GOTO1 DATCON,(R1),(0,WORK),(6,FTRMOS)                                  
         GOTO1 (RF),(R1),(0,WORK),(X'20',WORK+6)                                
*                                                                               
         PACK  DUB,WORK+6(4)                                                    
         ICM   R0,7,DUB+5                                                       
         SRL   R0,4                                                             
         STCM  R0,3,FTRBMOS                                                     
*                                                                               
         B     VFTR92                                                           
*                                                                               
VFTR90   DS   0H                                                                
         TM    WHEN,X'20'          ALLOWED IF SOON                              
         BO    VFTR92                                                           
         OC    RQSTA,RQSTA         WAS STATION ENTERED                          
         BNZ   VFTR92                                                           
*                                                                               
         OC    RQBSEQ,RQBSEQ       WAS SEQUENCE # ENTERED                       
         BZ    MISENTER                                                         
*                                                                               
VFTR92   LLC   RE,BYTE             UP FIELD NUMBER CTR                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         LA    R4,47(R4)           NOTE- NON-STANDARD LENGTH                    
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
VFTRX    J     XIT                                                              
*                                                                               
VFTRCLCA CLC   12(0,R4),=CL9'ACTIVITY' DATE                                     
VFTRCLCB CLC   12(0,R4),=CL3'CC'       CLIENT CODE                              
VFTRCLCC CLC   12(0,R4),=CL3'CN'              NAME                              
VFTRCLCD CLC   12(0,R4),=CL3'PC'       PRODUCT CODE                             
VFTRCLCE CLC   12(0,R4),=CL3'PN'               NAME                             
VFTRCLCF CLC   12(0,R4),=CL5'CONV'                                              
VFTRCLCG CLC   12(0,R4),=CL7'RECONV'                                            
VFTRCLCH CLC   12(0,R4),=CL7'UNCONV'                                            
VFTRCLCI CLC   12(0,R4),=CL8'INVOICE'                                           
VFTRCLCJ CLC   12(0,R4),=CL7'SOURCE'                                            
VFTRCLCK CLC   12(0,R4),=CL5'DONE'                                              
VFTRCLCL CLC   12(0,R4),=CL6'BATCH'                                             
VFTRCLCM CLC   12(0,R4),=CL6'MEDIA'                                             
VFTRCLCN CLC   12(0,R4),=CL6'TRACE'                                             
VFTRCLCO CLC   12(0,R4),=CL9'OVERRIDE'                                          
VFTRCLCP CLC   12(0,R4),=CL4'MOS'                                               
VFTRCLCQ CLC   12(0,R4),=CL4'ALL'                                               
VFTRCLCR CLC   12(0,R4),=CL4'MID'                                               
VFTRCLCS CLC   12(0,R4),=CL7'EXPIRED'                                           
VFTRCLCT CLC   12(0,R4),=CL7'DELETE'                                            
VFTRCLCU CLC   12(0,R4),=CL5'USER'                                              
VFTRCLCV CLC   12(0,R4),=CL2'IM'                                                
VFTRCLCW CLC   12(0,R4),=CL5'NONIM'                                             
VFTRCLCX CLC   12(0,R4),=CL7'SHOWEQU'                                           
*                                                                               
*                                                                               
*                                                                               
USERIDER L     R1,=A(INVUIDMS)                                                  
         B     VFTRERX                                                          
USIDLNER L     R1,=A(UIDLENMS)                                                  
         B     VFTRERX                                                          
VFTRMDER L     R1,=A(VFTRMDMS)                                                  
         B     VFTRERX                                                          
FTRTIMER L     R1,=A(FTRTIMS)                                                   
         B     VFTRERX                                                          
VFTRERRC L     R1,=A(VFTRMS)                                                    
         B     VFTRERX                                                          
CCLENER  L     R1,=A(CCLENMS)                                                   
         B     VFTRERX                                                          
PCLENER  L     R1,=A(PCLENMS)                                                   
         B     VFTRERX                                                          
INVLENER L     R1,=A(INVLENMS)                                                  
         B     VFTRERX                                                          
SRCLENER L     R1,=A(SRCLENMS)                                                  
         B     VFTRERX                                                          
MOSERR   L     R1,=A(MOSERMS)                                                   
         B     VFTRERX                                                          
FTRLENER L     R1,=A(FTRLENMS)                                                  
         B     VFTRERX                                                          
*                                                                               
MISENTER L     R1,=A(MISENTMS)                                                  
         LA    R2,LINSTAH          STATION                                      
         B     VFTRERX                                                          
*                                                                               
VFTRHLP  L     R1,=A(FTRHELP)                                                   
*                                                                               
VFTRERX  XC    CONHEAD,CONHEAD                                                  
         A     R1,RELO                                                          
         BCTR  R1,0                                                             
         CLI   0(R1),L'CONHEAD-1                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
         LLC   RF,0(R1)                                                         
         EX    RF,*+8                                                           
         B     VFTRERY                                                          
         MVC   CONHEAD(0),1(R1)                                                 
VFTRERR  XC    CONHEAD,CONHEAD                                                  
         OI    BYTE,X'F0'                                                       
         MVC   CONHEAD(30),=C'* ERROR * INVALID FILTER FIELD'                   
         MVC   CONHEAD+31(1),BYTE                                               
         MVI   CONHEAD+33,C'*'                                                  
*                                                                               
VFTRERY  GOTO1 ERREX2                                                           
MISSERRA MVI   ERROR,MISSING                                                    
         B     TRAPERRA                                                         
BADATEA  MVI   ERROR,INVDATE                                                    
*                                                                               
TRAPERRA GOTO1 ERREX                                                            
         DC    AL1(L'FTRHELP-1)                                                 
FTRHELP  DC    CL60'FILTERS=ACT/CC/CN/PC/PN/CONV/RECONV/INV/SOURCE/DEL/C        
               UNCONV *'                                                        
         DC    AL1(L'INVUIDMS-1)                                                
INVUIDMS DC    C'* ERROR * INVALID USER ID *'                                   
         DC    AL1(L'FTRTIMS-1)                                                 
FTRTIMS  DC    CL60'** ERROR * FILTERS NOT ALLOWED FOR REPORT *'                
         DC    AL1(L'UIDLENMS-1)                                                
UIDLENMS DC    C'* ERROR * USERID CAN''T BE MORE THAN 8 CHARACTERS *'           
         DC    AL1(L'SRCLENMS-1)                                                
SRCLENMS DC    C'* ERROR * SOURCE CAN''T BE MORE THAN 4 CHARACTERS *'           
         DC    AL1(L'MOSERMS-1)                                                 
MOSERMS  DC    C'* ERROR * ENTER MOS MO/YR OR MOMYR *'                          
         DC    AL1(L'FTRLENMS-1)                                                
FTRLENMS DC    C'* ERROR * ENTER AT LEAST 2 CHARACTERS OF CODE *'               
         DC    AL1(L'MISENTMS-1)                                                
MISENTMS DC    C'* ERROR * MUST ENTER STATION OR BATCH SEQ *'                   
         LTORG                                                                  
         DROP  RB,RC                                                            
*                                                                               
*                                                                               
***********************************************************************         
*   DRHOOK - DISPLAY REC EZMOD HOOK                                   *         
***********************************************************************         
*                                                                               
         DS    0H                                                               
DRHOOK   NMOD1 0,**DRHK**                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
         ST    R0,SVR0                                                          
*                                                                               
         MVI   SVSFLAG1,X'00'                                                   
         XC    SVCLTTWA,SVCLTTWA                                                
         MVI   SVOFC,X'00'                                                      
*                                                                               
         CLI   RECNUM,X'12'        CONSUM                                       
         BE    *+12                                                             
         CLI   RECNUM,X'0F'        CONLIST                                      
         BNE   DRH10                                                            
         CLI   SVLINNUM,X'00'                                                   
         BE    *+10                                                             
         MVC   SELLISTN,SVLINNUM                                                
*                                                                               
DRH10    DS    0H                                                               
         LLC   RF,SELLISTN         GET LINE NUMBER                              
         MH    RF,=AL2(L'INVENT)                                                
         LA    R5,INVLIST(RF)                                                   
         USING INVLISTD,R5                                                      
*                                                                               
         CLI   EZMODE,EZINVL       END OF INVOICE                               
         BE    DRH020                                                           
*                                                                               
         CLI   EZMODE,EZINVP       PROCESS INVOICE                              
         BNE   DRHX                                                             
*                                                                               
         OI    DIVREPH+4,X'20'     SET REP VALID                                
         OI    DIVREPH+6,X'80'                                                  
*                                                                               
         LHI   R0,36               COLUMN 36 - SECOND SAVE FIELD                
         L     R1,EZWKRREC         INVOICE HEADER RECORD                        
         BRAS  RE,FINDFLD                                                       
         BE    DRH15                                                            
*                                                                               
         OI    DIVREPTH+1,X'0C'     PROTECTED, ZERO INTENSITY                   
         OI    DIVREPTH+6,X'80'                                                 
         OI    DIVREPH+1,X'2C'     PROTECTED, ZERO INTENSITY                    
         OI    DIVREPH+6,X'80'                                                  
*                                                                               
DRH15    DS    0H                                                               
         CLC   EZINVSEQ,INVRSEQ      ARE WE AT RIGHT INVOICE                    
         BL    DRH360                                                           
         BH    DRH500                                                           
*                                                                               
         MVC   SVINVSEQ,EZRECSEQ     SAVE INVOICE HEADER SEQ                    
         XC    RECORDSW,RECORDSW   BUG CATCHER                                  
         MVI   FOUNDSW,C'Y'                                                     
         B     DRHX                                                             
*                                                                               
* SAVE FOR VR RTN                                                               
*                                                                               
DRH020   DS    0H                                                               
         MVC   SVIHADVN,EZIHADVN   25 CHAR ADVERTISER NAME                      
         MVC   SVIHAAID,EZIHAAID    8 CHAR ADVERTISER CODE                      
         MVC   SVIHCNVS,EZIHCNVS    SAVE CONVERSION STATUS FIELD                
         MVI   SVCLTSPR,0          CLIENT SPECIAL RATE                          
         MVI   SVESTSPR,0          ESTIMATE  "     "                            
         XC    QPRD,QPRD                                                        
         XC    QPRD2,QPRD2                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(2,EZIHCMST),(0,SVIHCMST)                            
         GOTO1 (RF),(R1),(2,EZIHCMEN),(0,SVIHCMEN)                              
*                                                                               
         MVC   SRCESTA,INVSTA      STATION                                      
         CLI   SRCESTA+3,C'.'                                                   
         BNE   *+8                                                              
         MVI   SRCESTA+3,C' '                                                   
*                                                                               
         GOTO1 VEQVSTA             GO GET EQUIVALENT STATION                    
*                                                                               
* SET MEDIA STRAIGHT - USE EQUIVALENT STATION'S MEDIA                           
*                                                                               
         MVC   SVSNMED,EQVMED                                                   
         MVC   SVMEDTWA,EQVMED                                                  
*                                                                               
         LA    R1,EQVMED                                                        
         ICM   R1,8,=AL1(EZMTMEDQ)                                              
         GOTO1 VGETMED                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    BAGYMD,X'F0'                                                     
         OC    BAGYMD,EZMTHEX-EZMEDTBD(RF)                                      
*                                                                               
         XC    WORK(L'LINSTA),WORK          BATCH STATION                       
*                                                                               
         MVC   WORK(L'PRTSTA7C),PRTSTA7C                                        
         CLC   LINSTA,WORK                                                      
         BE    *+14                                                             
         MVC   LINSTA,WORK                                                      
         OI    LINSTAH+6,X'80'                                                  
*                                                                               
* READ STATION MASTER RECORD TO OBTAIN SFLAG1                                   
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(L'STAKEY-1),KEY                                            
         LA    R4,KEY                                                           
         USING STAREC,R4                                                        
         MVI   STAKTYPE,C'S'                                                    
*                                                                               
         MVC   STAKMED,EQVMED                                                   
*                                                                               
         MVC   STAKCALL,EQUISTA                                                 
         MVC   STAKAGY,AGENCY                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO1                     
         CLI   8(R1),0             ANY ERROR                                    
         BNE   DRH030               NO                                          
         L     R4,AIO1                                                          
         MVC   SVSFLAG1,SFLAG1                                                  
         DROP  R4                                                               
*                                                                               
DRH030   DS    0H                                                               
         XC    WORK(L'LINBDT),WORK          BATCH DATE                          
         GOTO1 DATCON,DMCB,(2,INVBDTE),(5,WORK)                                 
         CLC   LINBDT,WORK                                                      
         BE    *+14                                                             
         MVC   LINBDT,WORK                                                      
         OI    LINBDTH+6,X'80'                                                  
*                                                                               
         XC    ELEM(L'LINBSQ),ELEM          BATCH SEQ                           
         SR    R0,R0                                                            
         ICM   R0,15,INVBSEQ                                                    
         EDIT  (R0),(6,ELEM),ALIGN=LEFT                                         
         CLC   LINBSQ,ELEM                                                      
         BE    *+14                                                             
         MVC   LINBSQ,ELEM                                                      
         OI    LINBSQH+6,X'80'                                                  
         DROP  R5                                                               
*                                                                               
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         CLC   DIVCNM,EZIHADVN                                                  
         BE    *+14                                                             
         MVC   DIVCNM,EZIHADVN                                                  
         OI    DIVCNMH+6,X'80'                                                  
*                                                                               
         CLC   DIVCDE,EZIHAAID                                                  
         BE    *+20                                                             
         MVC   DIVCDE,EZIHAAID                                                  
         MVC   SVCLTTWA,EZIHAAID                                                
         OI    DIVCDEH+6,X'80'                                                  
*                                                                               
         OC    EZIHCVAD,EZIHCVAD   TEST HAVE CLIENT OVERRIDE                    
         BNZ   DRH036              YES - USE THAT, NOT STATION CLIENT           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         GOTO1 CLPACK,DMCB,EZIHAAID,KEY+2                                       
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     BIG BAD ERROR                                
         BNE   DRH036                                                           
*                                                                               
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         MVC   FILENAME+3(3),=C'FIL'                                            
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
         MVC   SVOFC,COFFICE-CLTHDR(R4)   OFFICE NUMBER                         
*                                                                               
DRH036   DS    0H                                                               
         CLC   DIVPNM,EZIHPRDN                                                  
         BE    *+14                                                             
         MVC   DIVPNM,EZIHPRDN                                                  
         OI    DIVPNMH+6,X'80'                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'EZIHAPID),EZIHAPID                                        
*                                                                               
* ONLY SHOW ESTIMATE IF PRESENT *                                               
*                                                                               
         LA    RE,L'EZIHAPID                                                    
         LA    RF,WORK-1+L'EZIHAPID                                             
DRH040   CLI   0(RF),C' '          FIND LAST NON-BLANK CHAR                     
         BH    DRH044                                                           
         BCTR  RF,0                                                             
         BCT   RE,DRH040                                                        
*                                                                               
DRH044   CLC   EZIHEST,SPACES      IS THERE AN ESTIMATE NUMBER                  
         BE    DRH050               NO                                          
         OC    EZIHEST,EZIHEST     IS THERE AN ESTIMATE NUMBER                  
         BZ    DRH050               NO                                          
         MVI   1(RF),C'-'                                                       
         MVC   2(L'EZIHEST,RF),EZIHEST AND MOVE IN ESTIMATE                     
*                                                                               
DRH050   CLC   DIVPPE,WORK                                                      
         BE    *+14                                                             
         MVC   DIVPPE,WORK                                                      
         OI    DIVPPEH+6,X'80'                                                  
*                                                                               
         CLC   DIVINO(L'EZIHINV),EZIHINV                                        
         BE    *+14                                                             
         MVC   DIVINO(L'EZIHINV),EZIHINV                                        
         OI    DIVINOH+6,X'80'                                                  
*                                                                               
         CLC   EZIHINST,=CL25'BILLING INVOICE REVERSAL'                         
         BE    *+10                                                             
         MVC   EZIHINST,SPACES                                                  
         CLC   DIVREV(L'EZIHINST),EZIHINST                                      
         BE    *+14                                                             
         MVC   DIVREV(L'EZIHINST),EZIHINST                                      
         OI    DIVREVH+6,X'80'                                                  
*                                                                               
         MVC   DIVMOS(3),EZIHDMOS                                               
         MVC   DIVMOS+3(2),EZIHDMOS+4                                           
         OI    DIVMOSH+6,X'80'                                                  
*                                                                               
         XC    DIVMOSE,DIVMOSE                                                  
         OI    DIVMOSEH+6,X'80'                                                 
         OC    EZIHBMOS,EZIHBMOS   IS MONTH OF SERVICE BLANK                    
         BNZ   *+10                                                             
         MVC   DIVMOSE,=C'* ERROR *'   ONLY USES 1ST 7 CHARACTERS               
*                                                                               
         CLI   SPOTNETS,C'N'       NET MEDIA?                                   
         BNE   DRH60                                                            
*                                                                               
         MVI   SVPKGTWA,X'00'      INITIALIZE SAVED OVERRIDE                    
         MVC   DIVPKG,SPACES                                                    
         OI    DIVPKGH+6,X'80'                                                  
         OI    DIVPKGH+4,X'20'     SET VALIDATED                                
*                                                                               
         TMY   EZIHSFL2,EZIHF2NOPKGQ  NO-PACKAGE '#' OVERRIDE?                  
         BZ    DRH53                                                            
*                                                                               
         MVI   DIVPKG,C'#'                                                      
         OI    DIVPKGH+6,X'80'                                                  
         OI    DIVPKGH+4,X'20'     SET VALIDATED                                
         B     DRH60                                                            
*                                                                               
DRH53    DS    0H                                                               
         OC    EZIHCVNP,EZIHCVNP   PACKAGE OVERRIDE                             
         BZ    DRH55                                                            
         EDIT  EZIHCVNP,DIVPKG,ALIGN=LEFT                                       
         MVC   SVPKGTWA,EZIHCVNP   SAVE OVERRIDE VALUE                          
         B     DRH60                                                            
*                                                                               
DRH55    DS    0H                                                               
         CLC   EZIHNPKG,SPACES                                                  
         BNH   DRH57                                                            
         MVC   DIVPKG,EZIHNPKG                                                  
         OC    DIVPKG,SPACES                                                    
         OI    DIVPKGH+6,X'80'                                                  
         OI    DIVPKGH+4,X'20'     SET VALIDATED                                
         B     DRH60                                                            
*                                                                               
DRH57    DS    0H                                                               
         CLC   EZIHNET,SPACES                                                   
         BNH   DRH60                                                            
         MVC   DIVPKG,EZIHNET                                                   
         OC    DIVPKG,SPACES                                                    
         OI    DIVPKGH+6,X'80'                                                  
         OI    DIVPKGH+4,X'20'     SET VALIDATED                                
*                                                                               
DRH60    DS    0H                                                               
         EDIT  (B4,EZIHTSPN),(5,DIVSPTS),COMMAS=YES,ZERO=NOBLANK                
         OI    DIVSPTSH+6,X'80'                                                 
*                                                                               
         OC    EZITBDUE,EZITBDUE                                                
         BZ    DRH60A                                                           
*                                                                               
         EDIT  (B4,EZITBDUE),(16,DIVORDR),2,COMMAS=YES,ZERO=NOBLANK,   C        
               MINUS=YES                                                        
         B     DRH60B                                                           
*                                                                               
DRH60A   DS    0H                                                               
         LA    R3,EZBLOCKD                                                      
         AHI   R3,EZITPDUE-EZBLOCKD                                             
         EDIT  (P8,0(R3)),(16,DIVORDR),2,COMMAS=YES,ZERO=NOBLANK,      C        
               MINUS=YES                                                        
*                                                                               
DRH60B   DS    0H                                                               
         OI    DIVORDRH+6,X'80'                                                 
*                                                                               
         OC    EZITBACT,EZITBACT                                                
         BZ    DRH60C                                                           
*                                                                               
         EDIT  (B4,EZITBACT),(16,DIVGRSS),2,COMMAS=YES,ZERO=NOBLANK,   C        
               MINUS=YES                                                        
         B     DRH60D                                                           
*                                                                               
DRH60C   DS    0H                                                               
         LA    R3,EZBLOCKD                                                      
         AHI   R3,EZITPACT-EZBLOCKD                                             
         EDIT  (P8,0(R3)),(16,DIVGRSS),2,COMMAS=YES,ZERO=NOBLANK,      C        
               MINUS=YES                                                        
*                                                                               
DRH60D   DS    0H                                                               
         OI    DIVGRSSH+6,X'80'                                                 
*                                                                               
         MVC   SVITBACT,EZITBACT                                                
         MVC   SVITBDUE,EZITBDUE                                                
         MVC   SVIHBMOS,EZIHBMOS   SAVE MONTH OF SERVICE                        
*                                                                               
         CLC   DIVSRCE,SVWCSRCE                                                 
         BE    DRH061                                                           
         MVC   DIVSRCE,SVWCSRCE                                                 
         OI    DIVSRCEH+6,X'80'                                                 
*                                                                               
DRH061   DS    0H                                                               
         XC    DIVSRC2,DIVSRC2                                                  
         OI    DIVSRC2H+6,X'80'                                                 
*                                                                               
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   DRH062                                                           
*                                                                               
         TM    EZIHCVST,EZIHIMQ         INVOICE CAME FROM IM?                   
         BZ    *+10                                                             
         MVC   DIVSRC2(2),=C'IM'                                                
*                                                                               
* READ EZ PROFILE *                                                             
*                                                                               
DRH062   XC    PARAS(24),PARAS                                                  
         MVC   PARAS(4),=C'S0EZ'                                                
         MVC   PARAS+4(2),AGENCY                                                
         GOTO1 GETPROF,DMCB,(X'D0',PARAS),WORK,DATAMGR                          
         CLI   WORK+13,C'Y'        SHOW ORDER TYPE?                             
         BNE   DRH070                                                           
         NI    DIVOTYTH+1,X'FF'-X'0C'  MAKE FIELD VISIBLE                       
         MVC   DIVOTYP,EZIHORD                                                  
         OI    DIVOTYPH+6,X'80'                                                 
*                                                                               
DRH070   MVC   DIVCST,SPACES       CLEAR STATUS                                 
         MVC   DIVACTD,SPACES      AND   DATE FIELD                             
*                                                                               
         MVC   SVIHCVST,EZIHCVST   SAVE STATUS                                  
*                                                                               
         TM    EZIHCVST,EZIHCDEL   TEST DELETED                                 
         BZ    DRH080                                                           
         MVC   DIVCST(7),=C'DELETED'                                            
         B     DRH090                                                           
*                                                                               
DRH080   TM    EZIHCVST,EZIHCVQ    TEST CONVERTED                               
         BNZ   DRH100                                                           
         MVC   DIVCST(2),=C'NO'                                                 
*                                                                               
DRH090   OC    EZIHCVDT,EZIHCVDT   BETTER BE CONSISTENT                         
         BZ    *+6                                                              
         DC    H'0'                OOPS! UNCONVERTED MUST HAVE NO DATE          
*                                                                               
         B     DRH130                                                           
*                                                                               
DRH100   DS    0H                                                               
         MVC   DIVCST(3),=C'YES'                                                
         GOTO1 DATCON,DMCB,(1,EZIHCVDT),(8,DIVACTD)                             
*                                                                               
         OC    EZIHCVDT,EZIHCVDT   BETTER BE CONSISTENT                         
         BNZ   DRH130                                                           
         DC    H'0'                OOPS! CONVERTED MUST HAVE DATE               
*                                                                               
DRH130   OI    DIVCSTH+4,X'20'     VALIDATED                                    
         OI    DIVCSTH+6,X'80'                                                  
         OI    DIVACTDH+6,X'80'                                                 
*                                                                               
         MVC   DIVCVTP,=C'    '    BLANK OUT                                    
*        OI    DIVCVTPH+1,X'0C'    ZERO INTENSITY                               
*                                                                               
*        CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
*        BNE   DRH130A                                                          
         TM    EZIHCVST,EZIHCONS   CONVERTED SOON?                              
         BZ    DRH130A                                                          
         NI    DIVCVTPH+1,X'FF'-X'0C'                                           
         MVC   DIVCVTP,=C'SOON'                                                 
*                                                                               
DRH130A  DS    0H                                                               
         OI    DIVCVTPH+6,X'80'                                                 
*                                                                               
         OC    EZIHCVAD,EZIHCVAD   TEST HAVE CLIENT CODE                        
         BZ    DRH140                                                           
         LA    R2,DIVCCDH                                                       
*                                                                               
* SIMULATE GOTO1 VALICLT                                                        
*                                                                               
         OC    FUIDNUM,FUIDNUM     HAS ID BEEN SET AS OPTION                    
         BNZ   DRH140                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),EZIHCVAD                                                
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     BIG BAD ERROR                                
         BE    DRH131                                                           
         MVC   DIVCCD(3),=C'???'                                                
         MVI   DIVCCDH+5,3         SET LENGTH                                   
*                                                                               
         XC    DIVPCD,DIVPCD       CLEAR P1,P2-EST FIELD                        
         OI    DIVCCDH+4,X'20'     SET CLIENT VALIDATED                         
         OI    DIVCCDH+6,X'80'     TRANS                                        
         OI    DIVPCDH+4,X'20'     SET PRODUCT VALIDATED                        
         OI    DIVPCDH+6,X'80'     TRANS                                        
         B     DRH240                                                           
*                                                                               
DRH131   DS    0H                                                               
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         MVC   FILENAME+3(3),=C'FIL'                                            
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
* SAVE CLIENT PRODUCT LIST *                                                    
*                                                                               
         LA    R0,CLIST-CLTHDR(R4)                                              
         LA    R1,880                                                           
         LA    RE,SVCLIST                                                       
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R0,CLIST2-CLTHDR(R4)                                             
         LHI   R1,140              LENGTH OF CLIST2                             
         LR    RF,R1                                                            
         CLC   CLEN-CLTHDR(2,R4),=H'1280'   TEST CLIST2 IN RECORD               
         BH    *+8                 YES                                          
         SR    R1,R1               ELSE JUST CLEAR REST OF LIST                 
         SR    RF,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   CLTNM,CNAME-CLTHDR(R4)     SAVE CLIENT NAME                      
         MVC   BYTE,CPROF+6-CLTHDR(R4)    PRINT CLT CODE AS AAN                 
         MVC   SVOFC,COFFICE-CLTHDR(R4)   OFFICE NUMBER                         
*                                                                               
         GOTO1 CLUNPK,DMCB,(BYTE,EZIHCVAD),DIVCCD                               
         MVC   SVCLTTWA,DIVCCD                                                  
*                                                                               
         MVI   DIVCCDH+5,3         SET LENGTH                                   
         CLI   DIVCCD+2,C' '                                                    
         BH    *+8                                                              
         MVI   DIVCCDH+5,2                                                      
*                                                                               
         MVC   DIVCNMD(20),CLTNM                                                
         B     DRH142                                                           
*                                                                               
DRH140   XC    DIVCCD,DIVCCD                                                    
         XC    DIVCNMD,DIVCNMD                                                  
*                                                                               
DRH142   OI    DIVCNMDH+6,X'80'     TRANS                                       
         OI    DIVCCDH+4,X'20'     SET CLIENT VALIDATED                         
         OI    DIVCCDH+6,X'80'     TRANS                                        
*                                                                               
         XC    DIVPCD,DIVPCD                                                    
         MVI   BPRD,0                                                           
         MVI   BPRD2,0                                                          
         LA    R2,DIVPCDH                                                       
         LA    R4,DIVPCD+3                                                      
*                                                                               
         OC    FUIDNUM,FUIDNUM     IS USER = OPTION                             
         BNZ   DRH400               YES                                         
*                                                                               
         CLI   EZIHCVPR,0          TEST HAVE PRODUCT                            
*        BE    DRH440               NO                                          
         BE    DRH213               NO                                          
         XC    DIVPCD,DIVPCD                                                    
         LA    RE,255                                                           
         LA    RF,SVCLIST                                                       
*                                                                               
DRH212   CLC   EZIHCVPR,3(RF)                                                   
         BE    DRH214                                                           
         LA    RF,4(,RF)                                                        
         CLI   0(RF),C' '                                                       
         BNH   *+8                                                              
         BCT   RE,DRH212                                                        
         B     DRH213                                                           
*                                                                               
         MVC   DIVPCD(3),=C'???'   NO PRODUCT FOUND                             
         LA    R5,=CL20'PRODUCT NOT FOUND'                                      
         B     DRH216                                                           
*                                                                               
DRH213   DS    0H                  EZIHCVPR IS EMPTY                            
         CLI   SPOTNETS,C'N'       NET?                                         
         BNE   DRH440                                                           
         CLC   EZIHSPRD,SPACES                                                  
         BNH   DRH440                                                           
         LA    RF,EZIHSPRD                                                      
*                                                                               
DRH214   MVC   DIVPCD(3),0(RF)                                                  
         MVC   QPRD,0(RF)                                                       
         CLI   2(RF),C' '                                                       
         BH    *+6                                                              
         BCTR  R4,0                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),EZIHCVAD                                                
         MVC   KEY+4(3),QPRD                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         LA    R5,=CL20'PRODUCT NOT FOUND'                                      
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DRH216                                                           
         L     R1,AIO1                                                          
         ST    R1,AIO                                                           
         MVC   FILENAME+3(3),=C'FIL'                                            
         GOTO1 GETREC                                                           
         LA    R5,PNAME-PRDHDR(R1)                                              
*                                                                               
DRH216   XC    FILENAME,FILENAME                                                
         LLC   RE,0(R2)                                                         
         LA    RF,0(R2,RE)                                                      
         MVC   8(20,RF),0(R5)                                                   
         OI    6(RF),X'80'                                                      
*                                                                               
         CLI   EZIHCVP2,0          TEST HAVE PRODUCT 2                          
*        BE    DRH228                                                           
         BE    DRH223                                                           
         LA    RE,255                                                           
         LA    RF,SVCLIST                                                       
*                                                                               
DRH220   CLC   EZIHCVP2,3(RF)                                                   
         BE    DRH224                                                           
         LA    RF,4(,RF)                                                        
         CLI   0(RF),C' '                                                       
         BNH   *+8                                                              
         BCT   RE,DRH220                                                        
         B     DRH223                                                           
*                                                                               
         MVC   1(3,R4),=C'???'   NO PRODUCT FOUND                               
         LA    R4,4(,R4)                                                        
         LA    R5,=CL20'PRODUCT NOT FOUND'                                      
         B     DRH226                                                           
*                                                                               
DRH223   DS    0H                                                               
         CLI   SPOTNETS,C'N'       THIS NET                                     
         BNE   DRH228                                                           
         CLC   EZIHSPR2,SPACES                                                  
         BNH   DRH228                                                           
         LA    RF,EZIHSPR2                                                      
*                                                                               
DRH224   MVI   0(R4),C','                                                       
         MVC   1(3,R4),0(RF)                                                    
         MVC   QPRD2,0(RF)                                                      
         LA    R4,4(,R4)                                                        
         CLI   2(RF),C' '                                                       
         BH    *+6                                                              
         BCTR  R4,0                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),EZIHCVAD                                                
         MVC   KEY+4(3),QPRD2                                                   
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         LA    R5,=CL20'PRODUCT NOT FOUND'                                      
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DRH226                                                           
         L     R1,AIO1                                                          
         ST    R1,AIO                                                           
         MVC   FILENAME+3(3),=C'FIL'                                            
         GOTO1 GETREC                                                           
         LA    R5,PNAME-PRDHDR(R1)                                              
*                                                                               
DRH226   XC    FILENAME,FILENAME                                                
         LLC   RE,0(R2)                                                         
         LA    RF,0(R2,RE)                                                      
         LLC   RE,0(RF)                                                         
         AR    RF,RE                                                            
         MVC   8(20,RF),0(R5)                                                   
         OI    6(RF),X'80'                                                      
         B     DRH230                                                           
*                                                                               
DRH228   XC    DIVPN2D,DIVPN2D                                                  
         OI    DIVPN2DH+6,X'80'                                                 
*                                                                               
DRH230   CLI   EZIHCVES,0                                                       
         BE    DRH240                                                           
         LLC   RE,EZIHCVES                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   0(R4),C'-'                                                       
         UNPK  1(3,R4),DUB                                                      
*                                                                               
* GET ESTIMATE HEADER FOR DATES AND SPECIAL RATE *                              
*                                                                               
         OC    FUIDNUM,FUIDNUM     HAS ID BEEN SET AS OPTION                    
         BNZ   DRH240                                                           
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),EZIHCVAD                                                
         MVC   KEY+4(3),QPRD       PRODUCT                                      
         MVC   KEY+7(1),EZIHCVES   ESTIMATE                                     
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     BIG BAD ERROR                                
*        BE    *+6                  NO, OKAY                                    
*        DC    H'0'                                                             
         BE    *+14                                                             
         MVC   1(3,R4),=C'???'                                                  
         B     DRH240                                                           
*                                                                               
         L     R1,AIO1                                                          
         ST    R1,AIO                                                           
         MVC   FILENAME+3(3),=C'FIL'                                            
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
DRH240   OI    DIVPCDH+6,X'80'                                                  
         OI    DIVPCDH+4,X'20'     SET PRODUCT VALIDATED                        
*                                                                               
         MVC   BYTE,SVCLTSPR                                                    
         CLI   SVESTSPR,0                                                       
         BE    *+10                                                             
         MVC   BYTE,SVESTSPR                                                    
*                                                                               
         CLC   DIVSPRT,BYTE                                                     
         BE    *+14                                                             
         MVC   DIVSPRT,BYTE                                                     
         OI    DIVSPRTH+6,X'80'                                                 
*                                                                               
         MVI   DIVOVRT,0                                                        
         CLC   SVIHSPRT,SVCLTSPR   WAS THERE AN OVERRIDE SPECIAL RATE           
         BE    *+10                 NO                                          
         MVC   DIVOVRT,SVIHSPRT                                                 
         OI    DIVOVRTH+6,X'80'                                                 
         OI    DIVOVRTH+4,X'20'                                                 
*                                                                               
DRH250   DS    0H                                                               
         XC    DIVCSTS,DIVCSTS                                                  
         OI    DIVCSTSH+6,X'80'                                                 
         OI    DIVCSTSH+4,X'20'                                                 
*                                                                               
         TM    EZIHCVST,EZIHRCVQ   TEST RECONVERT                               
         BZ    DRH260                                                           
         MVC   DIVCSTS,=C'RECONVERT'                                            
*                                                                               
DRH260   DS    0H                                                               
         MVC   DIVPRP,SPACES                                                    
         OI    DIVPRPH+6,X'80'                                                  
         LA    R1,EZBLOCKD                                                      
         AHI   R1,EZIHPREP-EZBLOCKD                                             
         CLC   0(L'EZIHPREP,R1),SPACES                                          
         BNH   *+10                                                             
         MVC   DIVPRP,0(R1)                                                     
*                                                                               
         MVC   DIVREP,SPACES                                                    
         OI    DIVREPH+6,X'80'                                                  
         MVI   DIVDARF,C' '                                                     
         OI    DIVDARFH+6,X'80'                                                 
*                                                                               
         TM    DIVREPH+1,X'0C'     FIELD ZERO INTENSITY?                        
         BO    DRH280              YES - DON'T DISPLAY REP                      
*                                                                               
         OC    EZIHSREP,EZIHSREP   REP OVERRIDE?                                
         BZ    DRH270              NO - DON'T DISPLAY ANYTHING                  
*                                                                               
         MVC   DIVREP,EZIHSREP                                                  
         B     DRH280              DONE                                         
*                                                                               
DRH270   DS    0H                  CHECK IF DOING TRADE REPS                    
         BRAS  RE,ISTRADE                                                       
         BNE   DRH280              INVOICE NOT TRADE                            
         BRAS  RE,GETTRREP                                                      
         BNE   DRH280                                                           
         OC    WORK(3),WORK                                                     
         BZ    DRH280                                                           
*                                                                               
         MVC   DIVREP,WORK                                                      
         MVI   DIVDARF,C'#'                                                     
*                                                                               
         BRAS  RE,LKREP                                                         
         BNE   *+8                                                              
         MVI   DIVDARF,C'*'                                                     
*                                                                               
         OI    DIVREPH+6,X'80'                                                  
         OI    DIVDARFH+6,X'80'                                                 
*                                                                               
* DISPLAY DDS-ONLY INFORMATION HERE:                                            
*                                                                               
DRH280   DS    0H                                                               
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   DRH300                                                           
*                                                                               
* INVOICE HEADER RECORD NUMBER (WITHIN BATCH)                                   
*                                                                               
         LA    R2,DIVCDH                                                        
         BRAS  RE,XCFLD                                                         
*                                                                               
         LA    R2,DIVCD                                                         
*                                                                               
         MVC   0(3,R2),=C'IH='                                                  
         LA    R2,3(R2)                                                         
         EDIT  (B2,SVINVSEQ),(5,0(R2)),COMMAS=YES,ALIGN=LEFT                    
         LA    R2,5(R2)                                                         
*                                                                               
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,1(R2)                                                         
*                                                                               
* STATION ORDER NUMBER                                                          
         CLC   EZIHSORD,SPACES                                                  
         BNH   DRH281                                                           
*                                                                               
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         MVC   0(8,R2),=C'STA ORD='                                             
         LA    R2,8(R2)                                                         
         MVC   0(L'EZIHSORD,R2),EZIHSORD                                        
         OC    0(L'EZIHSORD,R2),SPACES                                          
         LA    R2,L'EZIHSORD(R2)                                                
*                                                                               
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,1(R2)                                                         
*                                                                               
DRH281   DS    0H                                                               
* REP ORDER NUMBER                                                              
         CLC   EZIHRORD,SPACES                                                  
         BNH   DRH282                                                           
*                                                                               
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         MVC   0(8,R2),=C'REP ORD='                                             
         LA    R2,8(R2)                                                         
         MVC   0(L'EZIHRORD,R2),EZIHRORD                                        
         OC    0(L'EZIHRORD,R2),SPACES                                          
         LA    R2,L'EZIHRORD(R2)                                                
*                                                                               
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,1(R2)                                                         
*                                                                               
DRH282   DS    0H                                                               
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         MVC   0(4,R2),=C'STA='                                                 
         LA    R2,4(R2)                                                         
         MVC   0(L'EZSNSTA,R2),EZSNSTA     '22' STA                             
         LA    R2,L'EZSNSTA(R2)                                                 
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,1(R2)                                                         
         MVI   0(R2),X'5E'         SEMICOLON                                    
         LA    R2,1(R2)                                                         
         MVC   0(L'EZSNMED,R2),EZSNMED     '22' MEDIA                           
         LA    R2,L'EZSNMED(R2)                                                 
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,1(R2)                                                         
         MVI   0(R2),X'5E'         SEMICOLON                                    
         LA    R2,1(R2)                                                         
         MVC   0(L'EZSNBND,R2),EZSNBND     '22' BAND                            
         LA    R2,L'EZSNBND(R2)                                                 
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,1(R2)                                                         
*                                                                               
         MVI   0(R2),C'/'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         MVC   0(L'SRCESTA,R2),SRCESTA     INDEX STATION                        
         LA    R2,L'SRCESTA(R2)                                                 
*                                                                               
DRH283   DS    0H                                                               
         OI    DIVCDH+6,X'80'                                                   
*                                                                               
DRH300   L     RD,SVR0             DON'T GO BACK TO EZMOD                       
         B     DRHX                                                             
*                                                                               
DRH360   MVI   EZMODE,EZINVL       SKIP TO NEXT INVOICE                         
*                                                                               
DRHX     J     XIT                                                              
*                                                                               
* ONLY COME HERE WITH USER= OPTION                                              
*                                                                               
DRH400   XC    DIVPCD,DIVPCD                                                    
         OI    DIVPCDH+6,X'80'                                                  
         CLI   EZIHCVPR,0          TEST HAVE PRODUCT                            
         BNE   DRH401                                                           
*                                                                               
* NO 1-BYTE OVERRIDE HERE                                                       
         CLI   SPOTNETS,C'N'       NET?                                         
         BNE   DRH250              NO,WE'RE SPOT.  NO PRODUCT                   
         CLC   EZIHSPRD,SPACES                                                  
         BNH   DRH250                                                           
*                                                                               
* PRODUCT OVERRIDE PRESENT                                                      
DRH401   DS    0H                                                               
         MVC   DIVPCD(3),=C'???'                                                
         LA    R1,DIVPCD+3                                                      
*                                                                               
         CLI   EZIHCVP2,0          TEST HAVE PRODUCT 2                          
         BNE   DRH402              YES, WE HAVE 1-BYTE OVERRIDE                 
*                                                                               
         CLI   SPOTNETS,C'N'       NET?                                         
         BNE   DRH402              NO,WE'RE SPOT.  NO PRODUCT                   
         CLC   EZIHSPR2,SPACES                                                  
         BNH   DRH410                                                           
*                                                                               
DRH402   DS    0H                                                               
         MVC   0(4,R1),=C',???'                                                 
         LA    R1,4(,R1)                                                        
*                                                                               
DRH410   CLI   EZIHCVES,0          ESTIMATE                                     
         BE    DRH250               NO                                          
         LLC   RE,EZIHCVES                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   0(R1),C'-'                                                       
         UNPK  1(3,R1),DUB                                                      
         B     DRH250                                                           
*                                                                               
* ONLY COME HERE WITH PROD = 0                                                  
*                                                                               
DRH440   XC    DIVPCD,DIVPCD                                                    
         OI    DIVPCDH+4,X'20'     SET PRODUCT VALIDATED                        
         OI    DIVPCDH+6,X'80'     TRANS                                        
*                                                                               
         CLI   EZIHCVPR,0          TEST HAVE PRODUCT                            
         BNE   DRH441                                                           
         CLI   SPOTNETS,C'N'       NET?                                         
         BNE   DRH441A                                                          
         CLC   EZIHSPRD,SPACES     TEST HAVE PRODUCT                            
         BNH   *+6                                                              
DRH441   DC    H'0'                                                             
*                                                                               
DRH441A  DS    0H                                                               
         CLI   EZIHCVP2,0          TEST HAVE PRODUCT 2                          
         BNE   DRH442                                                           
         CLI   SPOTNETS,C'N'       NET?                                         
         BNE   DRH442A                                                          
         CLC   EZIHSPR2,SPACES     TEST HAVE PRODUCT 2                          
         BNH   *+6                                                              
DRH442   DC    H'0'                                                             
*                                                                               
DRH442A  DS    0H                                                               
         CLI   EZIHCVES,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    DIVPNMDH+6,X'80'     TRANS                                       
         XC    DIVPNMD,DIVPNMD                                                  
         OI    DIVPN2DH+6,X'80'     TRANS                                       
         XC    DIVPN2D,DIVPN2D                                                  
*                                                                               
         B     DRH250                                                           
*                                                                               
DRH500   CLI   FOUNDSW,C'Y'        WAS INVOICE FOUND                            
         BE    DRH300               YES                                         
         DC    H'0'                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*********                                                                       
* SETUP *                                                                       
*********                                                                       
SETUP    NTR1  BASE=*,LABEL=*                                                   
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A2C' DDWRKIO                                   
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VWRKIO,0(R1)                                                     
*                                                                               
         CLI   SVFIRST,C'N'                                                     
         BE    SETUP05                                                          
*                                                                               
         MVI   SVFIRST,C'N'                                                     
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+CTIKNUM-CTIKEY(2),TWAORIG                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,AIO1                      
         L     R6,AIO1                                                          
         CLC   0(L'CTIKEY,R6),KEY                                               
         BNE   SETUP05                                                          
*                                                                               
         MVI   ELCODE,X'02'                                                     
         MVC   DATADISP,=AL2(28)                                                
         BRAS  RE,GETEL                                                         
         BNE   SETUP05                                                          
         CLC   =X'02',0(R6)                                                     
         BNE   SETUP05                                                          
*                                                                               
         LLC   R1,1(R6)                                                         
         SHI   R1,3                                                             
         CHI   R1,L'SVCHUID-1                                                   
         BNH   *+8                                                              
         LHI   R1,L'SVCHUID-1                                                   
         CHI   R1,0                                                             
         BH    *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVCHUID(0),2(R6)            USER ID                              
         OC    SVCHUID,SPACES                                                   
*                                                                               
SETUP05  DS    0H                                                               
         LA    R1,LINWORK                                                       
         LA    R1,DIVWORK                                                       
         L     R1,ACOMFACS                                                      
         MVC   VPARSNIP,CPARSNIP-COMFACSD(R1)                                   
*                                                                               
         CLI   SPOTNETS,C'N'       NET MEDIA?                                   
         BE    SETUP10                                                          
         OI    DIVPKGLH+1,X'2C'                                                 
         OI    DIVPKGH+1,X'2C'                                                  
         OI    DIVPKGLH+6,X'80'                                                 
         OI    DIVPKGH+6,X'80'                                                  
*                                                                               
SETUP10  DS    0H                                                               
*                                                                               
         BRAS  RE,TESTSEL                                                       
         JNE   NEQXIT                                                           
*                                                                               
         CLI   RECNUM,X'12'        CONSUM                                       
         BE    *+12                                                             
         CLI   RECNUM,X'0F'        CONLIST                                      
         BNE   SETUPX                                                           
         OI    GENSTAT1,NOSETEFH                                                
         OI    GLSTSTAT,APPLCDSP+NOSELFLD+CHNGLIST                              
*                                                                               
         CLC   TWASCR,CALLSTCK                                                  
         BNE   *+12                                                             
         MVI   CALLSTCK,0                                                       
         MVI   CALLSP,0                                                         
*                                                                               
         XC    DIVPFKY,DIVPFKY     CLEAR PFKEY DISPLAY FIELD ON SCREEN          
*                                                                               
         OI    CONSERVH+6,X'80'                                                 
         OI    CONSERVH+1,X'01'                                                 
*                                                                               
         MVC   DIVPFKY+50(11),=C'PF12=RETURN'  DISPLAY PF KEY INFO              
         OI    DIVPFKYH+6,X'80'    TRANSMIT                                     
*                                                                               
         OC    PFKEY,PFKEY         ANY PF KEY PRESSED?                          
         BZ    SETUPX           IF NO, MAKES NO SENSE TO CALL INITPFKY          
*                                                                               
         LA    R2,LINSTAH          STATION                                      
         GOTO1 INITPFKY,DMCB,PFTABLE                                            
*                                                                               
SETUPX   J     EQXIT                                                            
*                                                                               
* * * * * * * * * * * *                                                         
*                                                                               
PFTABLE  DS    0H                                                               
*        RETURN CALLER                                                          
         DC    AL1(PF12X-*,12,PFTRPROG,0,0)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
         DC   X'FF'                                                             
         LTORG                                                                  
*                                                                               
*                                                                               
TESTSEL  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACTNUM,ACTLIST      IF LIST SCREEN                               
         BNE   TSELX                                                            
*                                                                               
         LA    R2,LINSELH          LOOP THROUGH SELECT FIELDS                   
         LA    R0,LINLAST                                                       
TSEL2    DS    0H                                                               
         CR    R2,R0                                                            
         BNL   TSELX                                                            
         CLI   5(R2),0                                                          
         BE    TSEL6                                                            
         CLC   73(10,R2),=10C'*'                                                
         BNE   *+12                                                             
         ST    R2,SVR2                                                          
         JE    NEQXIT                                                           
*                                                                               
TSEL6    AHI   R2,94              NEXT LIST LINE                                
         B     TSEL2               SELECT FIELD                                 
*                                                                               
TSELX    J     EQXIT                                                            
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* R1 EXPECTED TO ADDRESS THE RECORD                                             
* R0 EXPECTED TO HAVE THE COLUMN NUMBER                                         
* NOTE, THAT RECORD, FIELD DELIMITERS ARE HARD-CODED                            
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
FINDFLD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CHI   R0,0                                                             
         BNH   FINDNQX                                                          
*                                                                               
         BCTR  R0,0                NUMBER OF SEMICOLONS, REALLY                 
         CHI   R0,0                                                             
         BE    FINDX                                                            
*                                                                               
         LH    R2,0(R1)            REC LEN                                      
         AR    R2,R1               R2 POINTS TO THE END OF RECORD               
         LA    R1,4(R1)            SKIP THE 4 REC LENGTH BYTES                  
*                                                                               
         CLC   =C'31',0(R1)        INV HEADER?                                  
         BNE   FIND20                                                           
* FOR INV HDR SKIP REC TYPE, DELIMITER, 12-BYTE SAVE FLD                        
         LA    R1,16(R1)                                                        
* DO NOT COUNT THE 12-BYTE FIELD'S SEMICOLON                                    
         SHI   R0,1                UPDATE FIELD COUNT                           
         CHI   R0,0                                                             
         BNH   FINDX                                                            
         B     FIND20                                                           
*                                                                               
FIND10   LA    R1,1(R1)                                                         
FIND20   CR    R1,R2               REACHED EOR?                                 
         BNL   FINDNQX                                                          
         CLI   0(R1),X'15'         EOR?                                         
         BE    FINDNQX                                                          
         CLI   0(R1),X'5E'         EOF? (FIELD DELIMITER = SEMICOLON)           
         BNE   FIND10                                                           
         BCT   R0,FIND10                                                        
*                                                                               
FINDX    LA    R1,1(R1)            ADVANCE TO THE FIRST CHAR OF THE FLD         
         CR    RB,RB               EXIT WITH EQUAL CONDITION CODE               
         XIT1  REGS=(R1)                                                        
FINDNQX  J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
WRTIMGR  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,BLOCK                                                         
         MVC   0(L'WRTIMQNM,R2),WRTIMQNM                                        
         LA    R2,L'WRTIMQNM(R2)                                                
         LHI   R3,L'WRTIMQNM+4                                                  
         EDIT  (R3),(4,(R2))                                                    
         OC    0(4,R2),=C'0000'                                                 
         LA    R2,4(R2)                                                         
         LHI   R3,WMGRDLQ+4                                                     
         EDIT  (R3),(4,(R2))                                                    
         OC    0(4,R2),=C'0000'                                                 
         LA    R2,4(R2)                                                         
*                                                                               
         LR    RE,R2                                                            
         LHI   RF,WMGRDLQ                                                       
         XCEFL                                                                  
*                                                                               
         USING WMGRD,R2                                                         
*                                                                               
* LINE BREAK                                                                    
         MVC   WMGRSEP,=C'<DDSSEPERATOR>'                                       
*                                                                               
* ACTION                                                                        
         CLI   DELETESW,C'Y'       DELETING?                                    
         BNE   *+8                                                              
         MVI   WMGRACT,C'D'        DELETE                                       
         CLI   RESTORSW,C'Y'       RESTORING DELETED?                           
         BNE   *+8                                                              
         MVI   WMGRACT,C'R'                                                     
*                                                                               
* INVOICE NUMBER                                                                
         MVC   WMGRINV,EZIHINV                                                  
* INVOICE DATE                                                                  
         GOTO1 DATCON,DMCB,(0,EZIHIDAT),(5,WMGRDATE)                            
* STATION (ORIGINAL)                                                            
         MVC   WMGRSTA,EZSNSTA     FROM THE 22 RECORD                           
* BAND (ORIRINAL)                                                               
         MVC   WMGRBND,EZSNBND     FROM THE 22 RECORD                           
* MEDIA (ORIGINAL)                                                              
         MVC   WMGRMED,EZSNMED     FROM THE 22 RECORD                           
* USER ID                                                                       
         MVC   WMGRUID,SVCHUID     USER ID                                      
         OC    WMGRUID,SPACES                                                   
* AGENCY ALPHA                                                                  
         MVC   WMGRAGY,AGENCY                                                   
*                                                                               
* NEW STATION                                                                   
         MVC   WMGRNSTA,EZEQVSTA                                                
*                                                                               
* NEW MEDIA                                                                     
         LA    R1,EZEQVSTA+4                                                    
*        ICM   R1,8,=AL1(EZMTPMDQ)                                              
         ICM   R1,8,=AL1(EZMTBNDQ)                                              
         GOTO1 VGETMED                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WMGRNMED,EZMTMED-EZMEDTBD(RF)                                    
*                                                                               
* NEW BAND                                                                      
         MVC   WMGRNBND,EZEQVSTA+4                                              
*                                                                               
         CLI   WMGRACT,C'C'                                                     
         BE    *+12                                                             
         CLI   WMGRACT,C'V'                                                     
         BNE   WRT50               SKIP CLT,PRD,EST                             
*                                                                               
* CLIENT                                                                        
         MVC   WMGRCLT,EZIHLADC                                                 
*                                                                               
* PRODUCT1                                                                      
         OC    EZIHLPRC,EZIHLPRC                                                
         BZ    *+10                                                             
         MVC   WMGRPRD1,EZIHLPRC                                                
*                                                                               
* PRODUCT2                                                                      
         OC    EZIHLP2C,EZIHLP2C                                                
         BZ    *+10                                                             
         MVC   WMGRPRD1,EZIHLP2C                                                
*                                                                               
* ESTIMATE                                                                      
         OC    EZIHBEST,EZIHBEST                                                
         BZ    WRT40                                                            
         EDIT  EZIHBEST,WMGREST                                                 
*                                                                               
WRT40    DS    0H                                                               
* PAYING REP                                                                    
         MVC   WMGRPREP,EZIHLREP                                                
*                                                                               
* PACKAGE CODE                                                                  
         OC    EZIHCVNP,EZIHCVNP                                                
         BZ    WRT50                                                            
         EDIT  EZIHCVNP,WMGRPKG                                                 
*                                                                               
WRT50    DS    0H                                                               
* INVOICE VERSION NUMBER                                                        
         OC    EZIHSVER,EZIHSVER                                                
         BZ    WRT55                                                            
         EDIT  EZIHSVER,WMGRVER                                                 
*                                                                               
WRT55    DS    0H                                                               
         MVC   WMGRMOS,EZIHMON                                                  
*                                                                               
         MVI   WMGRACT+L'WMGRACT,X'4F'                                          
         MVI   WMGRINV+L'WMGRINV,X'4F'                                          
         MVI   WMGRDATE+L'WMGRDATE,X'4F'                                        
         MVI   WMGRSTA+L'WMGRSTA,X'4F'                                          
         MVI   WMGRMED+L'WMGRMED,X'4F'                                          
         MVI   WMGRBND+L'WMGRBND,X'4F'                                          
         MVI   WMGRUID+L'WMGRUID,X'4F'                                          
         MVI   WMGRAGY+L'WMGRAGY,X'4F'                                          
         MVI   WMGRNSTA+L'WMGRNSTA,X'4F'                                        
         MVI   WMGRNMED+L'WMGRNMED,X'4F'                                        
         MVI   WMGRNBND+L'WMGRNBND,X'4F'                                        
         MVI   WMGRCLT+L'WMGRCLT,X'4F'                                          
         MVI   WMGRPRD1+L'WMGRPRD1,X'4F'                                        
         MVI   WMGRPRD2+L'WMGRPRD2,X'4F'                                        
         MVI   WMGREST+L'WMGREST,X'4F'                                          
         MVI   WMGRPREP+L'WMGRPREP,X'4F'                                        
         MVI   WMGRPKG+L'WMGRPKG,X'4F'                                          
         MVI   WMGRVER+L'WMGRVER,X'4F'                                          
         MVI   WMGRMOS+L'WMGRMOS,X'4F'                                          
         MVI   WMGRNINV+L'WMGRNINV,X'4F'                                        
         MVI   WMGRRVER+L'WMGRRVER,X'4F'                                        
         MVI   WMGRBDAT+L'WMGRBDAT,X'4F'                                        
*                                                                               
         OC    WMGRACT,SPACES                                                   
         OC    WMGRINV,SPACES                                                   
         OC    WMGRDATE,SPACES                                                  
         OC    WMGRSTA,SPACES                                                   
         OC    WMGRMED,SPACES                                                   
         OC    WMGRBND,SPACES                                                   
         OC    WMGRUID,SPACES                                                   
         OC    WMGRAGY,SPACES                                                   
         OC    WMGRNSTA,SPACES                                                  
         OC    WMGRNMED,SPACES                                                  
         OC    WMGRNBND,SPACES                                                  
         OC    WMGRCLT,SPACES                                                   
         OC    WMGRPRD1,SPACES                                                  
         OC    WMGRPRD2,SPACES                                                  
         OC    WMGREST,SPACES                                                   
         OC    WMGRPREP,SPACES                                                  
         OC    WMGRPKG,SPACES                                                   
         OC    WMGRVER,SPACES                                                   
         OC    WMGRMOS,SPACES                                                   
         OC    WMGRNINV,SPACES                                                  
         OC    WMGRRVER,SPACES                                                  
         OC    WMGRBDAT,SPACES                                                  
*                                                                               
         L     RF,ACOMFACS                                                      
         LHI   R3,WMGRDLQ+L'WRTIMQNM+8                                          
         ICM   RF,15,CMQIO-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=CL8'PUT',BLOCK,(R3)                                   
         OI    IMSWITCH,IMSWWMQQ                                                
*                                                                               
WRTX     DS    0H                                                               
         J     EQXIT                                                            
         DROP  R2                                                               
         LTORG                                                                  
*                                                                               
WRTIMQNM DC    C'IMSTATUS********'                                              
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *                 
* BYTE EXPECTED TO HAVE THE NEW MOS (X'BA' = OCT 2011)                          
* ON EXIT HALF WILL HAVE THE OLD FORMAT MOS (X'1110')                           
* DUB IS USED FOR CONVERSION                                                    
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *                 
NEW2OLD  NTR1  BASE=*,LABEL=*                                                   
         LLC   RF,BYTE                                                          
         NILL  GRF,X'000F'         ZERO OUT YEAR (RF, GRAND INSTR)              
         CVD   RF,DUB                                                           
         SR    RF,RF                                                            
         ICM   RF,3,DUB+6                                                       
         SRL   RF,4                                                             
         STC   RF,HALF+1                                                        
*                                                                               
         LLC   RF,BYTE                                                          
         SRL   RF,4                GET RID OF THE MONTH                         
         CVD   RF,DUB                                                           
         SR    RF,RF                                                            
         ICM   RF,3,DUB+6                                                       
         SRL   RF,4                                                             
         STC   RF,HALF                                                          
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
* R0 = A(TEXT TO SEARCH), HOB = LENGTH OF TEXT TO SEARCH                        
* HALF = LENGTH OF TEXT                                                         
* FULL = A(TEXT)                                                                
* ON EXIT FULL = 0 IF NOT FOUND OR A(SUBSTRING)                                 
SUBSTR   NTR1  BASE=*,LABEL=*                                                   
         LR    RE,R0                                                            
         SRL   RE,24               RE = LENGTH OF TEXT TO SEARCH                
*                                                                               
         LR    R2,R0               R2 = ADD OF TEXT TO SEARCH                   
*                                                                               
         L     R1,FULL             R1 = A(TEXT)                                 
*                                                                               
         LR    RF,R1               RF = A(TEXT)                                 
         AH    RF,HALF             RF - PAST END OF TEXT                        
         SR    RF,RE               ADDR FOR LAST CLC                            
*                                                                               
         BCTR  RE,0                FOR EXECUTED CLC                             
*                                                                               
SUBS10   CR    R1,RF                                                            
         BH    SUBSNQX                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),0(R2)                                                    
         BE    SUBSQX                                                           
         LA    R1,1(R1)                                                         
         B     SUBS10                                                           
*                                                                               
SUBSQX   DS    0H                                                               
         ST    R1,FULL                                                          
         J     EQXIT                                                            
SUBSNQX  DS    0H                                                               
         J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* XCFLD - CLEARS THE SCREEN FIELD                                               
*                                                                               
* REQUIRES GFLEN                                                                
*                                                                               
* ON ENTRY: R2 = A(SCREEN FIELD HEADER)                                         
***********************************************************************         
XCFLD    NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,GFLEN                                                         
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         MVI   5(R2),0                                                          
         MVI   8(R2),C' '                                                       
         EX    RF,*+8                                                           
         J     EQXIT                                                            
         MVC   9(0,R2),8(R2)                                                    
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* GFLEN - DETERMINES THE LENGTH OF A SCREEN FIELD                               
*                                                                               
* ON ENTRY: R2 = A(SCREEN FIELD HEADER)                                         
* ON EXIT:  RF = FIELD LENGTH                                                   
***********************************************************************         
GFLEN    LLC   RF,0(R2)            LENGTH OF THE FIELD                          
         SHI   RF,8                MINUS L(FIELD HEADER)                        
         TM    1(R2),X'02'         EXTENDED HEADER?                             
         BZR   RE                  NO - EXIT                                    
         SHI   RF,8                YES - SUBTRACT L(EXT HEADER)                 
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* CHECKS WHETHER THE INVOICE IS TRADE OR NOT                                    
***********************************************************************         
ISTRADE  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,EZIHORD                                                       
         ST    R1,FULL                                                          
         LHI   R1,L'EZIHORD                                                     
         STH   R1,HALF                                                          
         LA    R0,=C'TRADE'                                                     
         ICM   R0,8,=X'05'                                                      
         BRAS  RE,SUBSTR                                                        
         JNE   NEQXIT                                                           
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* OBTAINS THE REP CODE FROM DAR PROFILE (RETURNED IN WORK)                      
* EQUAL CONDITION - REP RETURNED IN WORK                                        
*                 - WORK EMPTY -> NOT DOING TRADE REPS                          
* UNEQUAL CONDITION IF DAR PROFILE NOT SET UP                                   
***********************************************************************         
GETTRREP NTR1  BASE=*,LABEL=*                                                   
         XC    PARAS(24),PARAS                                                  
*                                                                               
         MVC   PARAS(4),=C'S0Z5'                                                
         MVC   PARAS+4(2),AGENCY                                                
*                                                                               
         MVC   PARAS+6(1),SVSNMED                                               
*                                                                               
         MVC   PARAS+7(3),SVCLTTWA                                              
         MVI   PARAS+10,C'*'                                                    
         MVC   PARAS+11(1),SVOFC                                                
*                                                                               
         GOTO1 GETPROF,DMCB,(X'D0',PARAS),WORK,DATAMGR                          
         CLI   WORK+EZTRDREP-EZPROF2,C'Y'     DOING TRADE REPS?                 
         BE    *+14                YES - READ DAR PROFILE                       
         XC    WORK,WORK           XC WORK AND EXIT OTHERWISE                   
         B     GETRQX                                                           
*                                                                               
* YES - READ DAR PROFILE                                                        
*                                                                               
         MVC   PARAS(4),=C'SDAR'                                                
         NI    PARAS,X'FF'-X'40'    LOWERCASE 'S'                               
         GOTO1 GETPROF,DMCB,(X'D0',PARAS),WORK,DATAMGR                          
*                                                                               
         OC    WORK+6(3),WORK+6                                                 
         BZ    GETRNQX                                                          
         CLC   WORK+6(3),=C'000'                                                
         BE    GETRNQX                                                          
         CLC   WORK+6(3),SPACES                                                 
         BNH   GETRNQX                                                          
*                                                                               
         MVC   WORK(3),WORK+6                                                   
         OC    WORK(3),SPACES                                                   
*                                                                               
GETRQX   DS    0X                                                               
         J     EQXIT                                                            
*                                                                               
GETRNQX  DS    0X                                                               
         XC    WORK,WORK                                                        
         J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* LOOKS UP REP CODE FOR CURRENT AGENCY                                          
* WORK EXPECTED TO HAVE REP CODE                                                
***********************************************************************         
LKREP    NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING REPREC,R3                                                        
*                                                                               
         MVC   REPKEY,=15C'0'                                                   
         MVI   REPKTYPE,C'R'                                                    
         MVC   REPKMED,SVSNMED                                                  
*                                                                               
         MVC   REPKREP,WORK                                                     
         OC    REPKREP,SPACES                                                   
         MVC   REPKAGY,AGENCY                                                   
         DROP  R3                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION ',KEY,AIO3                    
         L     R3,AIO3                                                          
         CLC   KEY(L'REPKEY),0(R3)                                              
         JE    EQXIT                                                            
         J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* ISALPHA/ISNUM - DETERMINES WHETHER A CHARACTER IS ALPHA OR NUMERIC            
* ISASPC - ALPHA + SPACE                                                        
* ISNUMSPC - NUMERIC + SPACE                                                    
*                                                                               
* ON ENTRY:    R1 = A(CHARACTER TO CHECK)                                       
*                                                                               
* ON EXIT:     EQUAL CONDITION CODE RETURNED IF                                 
*              THE CHARACTER IS ALPHA/NUMERIC                                   
*              UNEQUAL COND. CODE RETURNED OTHERWISE                            
***********************************************************************         
ISASPC   CLI   0(R1),C' '                                                       
         JE    ISEQX                                                            
*                                                                               
ISALPHA  CLI   0(R1),C'A'                                                       
         JL    ISNEQX                                                           
         CLI   0(R1),C'Z'                                                       
         JH    ISNEQX                                                           
         J     ISEQX                                                            
*                                                                               
ISNUMSPC CLI   0(R1),C' '                                                       
         JE    ISEQX                                                            
*                                                                               
ISNUM    CLI   0(R1),C'0'                                                       
         JL    ISNEQX                                                           
         CLI   0(R1),C'9'                                                       
         JH    ISNEQX                                                           
*                                                                               
ISEQX    CR    RB,RB                                                            
         BR    RE                                                               
ISNEQX   LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* INITIALIZE EZBLOCK                                                            
***********************************************************************         
INITEZB  NTR1  BASE=*,LABEL=*                                                   
         LR    RE,R6               CLEAR EZBLOCK                                
         LHI   RF,EZBLOCKL                                                      
         XCEFL                                                                  
*                                                                               
         MVC   EZWKRFIL,EASIWK                                                  
         LA    R1,WRKFBUFR                                                      
         ST    R1,EZWKRBUF                                                      
         LA    RE,WRKFREC                                                       
         ST    RE,EZWKRREC                                                      
         MVC   EZAREC,AIO1                                                      
         MVC   EZCOMFCS,ACOMFACS                                                
         MVI   EZLOOKSW,EZLNOADV+EZLNOPRD+EZLNOCML+EZLNOREP                     
         MVI   EZTEST,C'Y'                                                      
         TM    FTRFLAG,FTRTRACE                                                 
         BZ    *+8                                                              
         OI    EZTRACE,X'F0'                                                    
         MVI   EZWRITE,C'N'                                                     
         MVC   EZWRKIO,VWRKIO                                                   
         MVC   EZPRINT,VPRINT                                                   
*                                                                               
         OI    EZFLAG,EZFBATLQ                                                  
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* INITIALIZE WRKIO BLOCK                                                        
***********************************************************************         
INITWKB  NTR1  BASE=*,LABEL=*                                                   
         MVI   EZWRKIOF,C'Y'                                                    
*                                                                               
         LAY   R1,EZWRKIOB                                                      
         USING WRKIOD,R1                                                        
*                                                                               
         MVC   WRKIACOM,ACOMFACS                                                
         LA    RF,WRKFBUFR                                                      
         ST    RF,WRKIABUF                                                      
         LA    RF,WRKFREC                                                       
         ST    RF,WRKIAREC                                                      
         MVI   WRKIFTYP,WRKIFTEZ                                                
*                                                                               
         DROP  R1                                                               
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
* FAKEIND - ROUTINE TO FILL IN INDEX FIELDS                                     
* THIS IS TO BE USED WITH DDWRKIO CALLS                                         
* ROUTINE WILL POPULATE THE FOLLOWING FIELDS                                    
* WITH DATA TAKEN FROM DDWRKIOD:                                                
* EZWIMED                                                                       
* EZWISTN                                                                       
* EZWKRIND(2) - USER ID                                                         
* W_AGELD                                                                       
* W_STAT                                                                        
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
FAKEIND  NTR1  BASE=*,LABEL=*                                                   
         LAY   R4,EZWRKIOB                                                      
         USING WRKIOD,R4                                                        
*                                                                               
         LA    R2,SVWEZIND                                                      
         USING EZWKRIXD,R2                                                      
*                                                                               
         MVC   EZWIUID,WRKEZUID                                                 
         MVC   EZWISTN,WRKEZSCL    STA CALL LETTERS                             
         MVC   EZWIDAY,WRKEZDAY                                                 
         MVC   EZWIMED,WRKEZMED    MEDIA                                        
         DROP  R2                                                               
*                                                                               
         USING W_RECD,R2                                                        
         MVC   W_AGELD,WRKEZBDT    BATCH DATE                                   
         MVC   W_STAT,WRKEZSTA     FILE STATUS                                  
* !!! UKTYPE AND UKATTB SACRIFICED TO FIT IN THE 4-BYTE SEQ NUMBER !!!          
         MVC   W_FILENO(L'WRKEZSQN),WRKEZSQN                                    
         DROP  R4,R2                                                            
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* SPEZFFFD                                                                      
* EZBLOCK                                                                       
* CTGENFILE                                                                     
       ++INCLUDE SPGENEZ                                                        
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE EZBLOCK                                                        
*                                                                               
WRKIOD   DSECT                                                                  
       ++INCLUDE DDWRKIOD                                                       
*                                                                               
       ++INCLUDE DDPARSNIPD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAXTRAINF                                                      
       ++INCLUDE SPEZFFFD                                                       
*                                                                               
         ORG   CONTAGH                                                          
* SPEZFF8D                                                                      
       ++INCLUDE SPEZFF8D                                                       
*                                                                               
         ORG   CONTAGH                                                          
* SPEZFE8D                                                                      
       ++INCLUDE SPEZFE8D                                                       
*                                                                               
         ORG   LINWORK                                                          
VPARSNIP DS    V                                                                
VWRKIO   DS    V                                                                
SVMEDTWA DS    C                                                                
SVCLTTWA DS    CL3                 SAVING 3-CHAR CLIENT CODE                    
SVPKGTWA DS    X                                                                
IMSWITCH DS    X                                                                
IMSWDELQ EQU   X'01'               HAVE A DELETE                                
IMSWRESQ EQU   X'02'               HAVE A RESTORE                               
IMSWWMQQ EQU   X'04'               FEEDBACK WRITTEN TO MQ                       
IMSWIMQ  EQU   X'08'               INVOICE CAME FROM IM                         
SVCHUID  DS    CL8                                                              
SVOFC    DS    C                                                                
SVSFLAG1 DS    X                                                                
*                                                                               
*                                                                               
*                                                                               
* DDGENTWA                                                                      
       ++INCLUDE DDGENTWA                                                       
*                                                                               
* DMWRKRD                                                                       
       ++INCLUDE DMWRKFD                                                        
*                                                                               
* DMWRKRK                                                                       
       ++INCLUDE DMWRKFK                                                        
*                                                                               
* SPGENSTA                                                                      
       ++INCLUDE SPGENSTA                                                       
*                                                                               
* SPGENPRD                                                                      
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
*                                                                               
* SPGENEST                                                                      
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
*                                                                               
* SPGENCLT                                                                      
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
*                                                                               
       ++INCLUDE SPGENREP                                                       
*                                                                               
* CTGENFILE                                                                     
       ++INCLUDE CTGENFILE                                                      
*                                                                               
*SPEZFWORKD                                                                     
       ++INCLUDE SPEZFWORKD                                                     
*                                                                               
       ++INCLUDE DDSPOOK                                                        
       ++INCLUDE SPWMGRD                                                        
       ++INCLUDE SPEZDSCTS                                                      
*                                                                               
*                                                                               
REQD     DSECT                                                                  
REQHDR   DS    0XL26                                                            
       ++INCLUDE DMREQHDR                                                       
REQAREA  DS    0CL80   COLUMN                                                   
REQPROG  DS    0CL2    ------                                                   
REQCODE  DS    CL2        1        PROGRAM CODE                                 
REQAGY   DS    CL2        3        AGENCY CODE                                  
REQMED   DS    CL1        5        MEDIA CODE (R/T)                             
REQCLT   DS    CL3        6        CLIENT CODE                                  
REQPGR   DS    CL1        9        PROCESS BY DIVISION                          
REQMGR   DS    CL1       10        PROCESS BY DISTRICT                          
REQCLOFF DS    CL1       11        CLIENT OFFICE FILTER                         
REQPRD   DS    CL3       12        PRODUCT MNEMONIC                             
REQMKT   DS    CL4       15        MARKET NUMBER                                
REQSTA   DS    CL5       19        STATION CALL LETTERS                         
REQEST   DS    CL3       24        ESTIMATE NUMBER                              
REQESEND DS    CL3       27        LAST NUMBER IN ESTIMATE GROUP                
REQDMOVR DS    CL1       30        Y=DEMO OVERRIDE ACTIVE                       
REQCONT  DS    CL1       31        C'*' ==> DATA IN QAREA2                      
REQSTAUT DS    CL3       32        AUTO REQUEST START DATE                      
REQENAUT DS    CL3       35        AUTO REQUEST END DATE                        
REQSTART DS    CL6       38        REQUEST START DATE                           
REQEND   DS    CL6       44        REQUEST END DATE                             
         DS    CL8       51                                                     
REQAFFIL DS    CL1       57                                                     
         DS    CL3       58                                                     
REQOPT1  DS    CL1       62        OPTION 1                                     
REQOPT2  DS    CL1       63        OPTION 2                                     
REQOPT3  DS    CL1       64        OPTION 3                                     
REQOPT4  DS    CL1       65        OPTION 4                                     
REQOPT5  DS    CL1       66        OPTION 5                                     
REQGRP   DS    CL2       67        GROUP                                        
REQUSTOR DS    CL12      69        REQUESTOR NAME                               
*                                                                               
*                                                                               
* DSECT FOR THIS PROGRAM *                                                      
*                                                                               
*                                                                               
       ++INCLUDE SPEZFSYSD                                                      
*                                                                               
SYSD     DSECT                                                                  
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LBDTE    DS    CL5                                                              
         DS    CL1                                                              
LBSEQ    DS    CL6                                                              
         DS    CL1                                                              
LCDTE    DS    CL5                                                              
         DS    CL1                                                              
LSRCE    DS    CL4                                                              
         DS    CL1                                                              
LMON     DS    CL5                                                              
         DS    CL1                                                              
LSTA     DS    CL8                                                              
         DS    CL1                                                              
LCLT     DS    CL3                                                              
         DS    CL1                                                              
LPRD     DS    CL3                                                              
         DS    CL1                                                              
LPTR     DS    CL3                                                              
         DS    CL1                                                              
LEST     DS    CL3                                                              
         DS    CL1                                                              
LINV     DS    CL10                                                             
         DS    CL1                                                              
LSPTS    DS    CL4                                                              
LDEL     DS    CL1                 DELETE PREVIOUS INVOICE                      
LCVQ     DS    CL1                 CONVERTED                                    
LRCVQ    DS    CL1                 RECONVERT REQUEST                            
LOVR     DS    CL1                 CLT/PRD/EST OVERRIDE                         
*LRECSEQ  DS    CL3          79     DDS - DISPLAY REQ SEQ                       
LIMFLAG  DS    C                   IM FLAG                                      
*                                                                               
*                                                                               
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
PADVNM   DS    CL25                                                             
         DS    CL1                                                              
PADVCD   DS    CL3                                                              
         DS    CL3                                                              
PPRDNM   DS    CL25                                                             
         DS    CL2                                                              
PESTO    DS    CL3                                                              
         DS    CL2                                                              
PPRDCD   DS    CL7                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL2                                                              
PINV     DS    CL10                                                             
         DS    CL1                                                              
PMON     DS    CL5                                                              
         DS    CL2                                                              
PSPTS    DS    CL5                                                              
         DS    CL1                                                              
PDOLS    DS    CL13                                                             
         DS    CL1                                                              
PSTAT    DS    CL13          C-MONDA/YR OR                                      
PCVQ     DS    CL1                                                              
PRCVQ    DS    CL1                                                              
POVR     DS    CL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'156SPEZF07   11/08/17'                                      
         END                                                                    
