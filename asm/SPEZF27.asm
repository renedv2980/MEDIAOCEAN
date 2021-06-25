*          DATA SET SPEZF27    AT LEVEL 135 AS OF 12/14/15                      
*PHASE T23027A                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE: T23027 - EASI INVOICE UPDATIVE LIST                                   
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
         TITLE 'T23027 - UPDATIVE INVOICE LIST'                                 
*                                                                               
T23027   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**3027**,R7,RR=R2                                              
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
*                                                                               
         CLC   =A(WRKFEND-SYSD),LSYSD                                           
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    CONSERVH+6,X'80'                                                 
         OI    CONSERVH+6,X'01'                                                 
*                                                                               
         BRAS  RE,SETUP                                                         
         BNE   INVERR                                                           
*                                                                               
         L     R6,AIO2             SET EZBLOCK                                  
         USING EZBLOCKD,R6                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,LVALREC        VALIDATE RECORD                              
         BE    LVREC                                                            
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
XIT      XIT1                                                                   
*                                                                               
RUNF     DS    0H                                                               
         B     XIT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*   VKEY - VALIDATE KEY                                               *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VKEY     DS    0H                                                               
         MVC   LLIST,=Y(LINNEXT-LIND)     LENGTH OF LIST LINE                   
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    *+12                                                             
         CLI   ACTNUM,ACTSEL                                                    
         BNE   INVAL                                                            
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    INVAL                                                            
*                                                                               
* IF NO FIELDS ARE CHANGED, JUST KEEP ON WITH DISPLAY *                         
*                                                                               
         TM    LUPSTAH+4,X'20'     BATCH STATION                                
         BZ    VK000                                                            
         TM    LUPBDTH+4,X'20'     BATCH DATE                                   
         BZ    VK000                                                            
         TM    LUPBSQH+4,X'20'     BATCH SEQ                                    
         BZ    VK000                                                            
         TM    LUPFTRH+4,X'20'     FILTERS                                      
         BO    VKXIT                                                            
*                                                                               
VK000    MVI   NEWDISP,C'N'                                                     
         GOTO1 VALIMED             VALIMED SETS TO SPOT OR NET                  
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
         LA    R2,LUPSTAH          STATION                                      
         XC    RQSTA,RQSTA                                                      
         CLI   5(R2),0                                                          
         BE    VK100                                                            
         GOTO1 VALISTA                                                          
         CLC   RQSTA,QSTA                                                       
         BE    *+8                                                              
         MVI   NEWDISP,C'Y'                                                     
         MVC   RQSTA,QSTA                                                       
*                                                                               
VK100    OI    4(R2),X'20'                                                      
         LA    R2,LUPBDTH          BATCH DATE                                   
         MVI   DTPLUS,C'N'                                                      
         CLI   5(R2),0             IF NO DATE                                   
         BNE   VK120                                                            
         MVI   NEWDISP,C'Y'        THEN NEW BATCH                               
         XC    RQDTE,RQDTE                                                      
         B     VK200                                                            
*                                                                               
VK120    ZIC   RF,5(R2)               LOOK FOR FINAL +                          
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
         LA    R2,LUPBSQH          SEQ                                          
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
         LA    R2,LUPFTRH          FILTERS                                      
         BRAS  RE,VFTR                                                          
         OI    4(R2),X'20'                                                      
         XC    ATOT,ATOT           ZERO ALL TOTALS                              
         XC    SVLSTBAT,SVLSTBAT   ZERO LAST BATCH DATA                         
*                                                                               
VKXIT    DS    0H                  BUILD DUMMY KEY HERE                         
         XC    LASTSELK,LASTSELK                                                
         MVI   LASTSELK,X'06'           AGENCY RECORD                           
         MVC   LASTSELK+1(2),AGENCY                                             
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'           AGENCY REC                                   
         MVC   KEY+1(2),AGENCY                                                  
*                                                                               
         B     XIT                                                              
*                                                                               
***********************************************************************         
*   LIST - LIST RECORDS                                               *         
***********************************************************************         
*                                                                               
LIST     DS    0H                                                               
         LAY   R4,EZWRKIOB                                                      
         USING WRKIOD,R4                                                        
*                                                                               
         MVI   VALFLAG,C'N'        NO RECORDS HAVE BEEN VALIDATED               
*                                                                               
         BRAS  RE,INITEZB                                                       
         L     RE,=A(DISLIN)                                                    
         A     RE,RELO                                                          
         ST    RE,EZHOOK                                                        
*                                                                               
         BRAS  RE,INITWKB                                                       
*                                                                               
         LA    R1,LUPFRSTH                A(FIRST LIST LINE)                    
         CLI   SVLINNM2,0                 LAST SAVED LINE?                      
         BE    LS10                NONE - FIND NEXT UNPROTECTED FIELD           
         ZIC   R0,SVLINNM2                NUM OF SELECTED LINE                  
         MVI   SVLINNM2,X'00'             RE-SET LAST SELECTED LINE             
         MHI   R0,LUPSCNDH-LUPFRSTH       TIMES LENGTH OF LINE                  
         AR    R1,R0                      A(LAST SELECTED LINE)                 
*                                                                               
LS10     DS    0H                                                               
         ZIC   R0,0(R1)            LENGTH OF CURRENT FIELD                      
         AR    R1,R0               NEXT FIELD                                   
         OI    6(R1),X'C0'         CURSOR TO THIS LINE + TRANSMIT               
*                                                                               
         MVI   SEQNUM,0                                                         
         MVI   NLISTS,NUMLINS                                                   
*                                                                               
         MVI   USEIO,C'Y'          SET USER WILL DO ALL I/O                     
         MVI   EOJSW,C'N'                                                       
*                                                                               
         XCEFL INVLIST,L'INVLIST                                                
         LA    R5,INVLIST          R5 TO START OF LIST                          
         ST    R5,INVLPTR                                                       
         XC    WKRCTS,WKRCTS                                                    
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
LS080    MVC   WRKEZUID,SVBUID                                                  
         MVC   WRKEZSQN,SVBWKFLN                                                
         OI    WRKINDS,WRKISEQQ                                                 
*                                                                               
LS100    DS    0H                                                               
         TM    FTRFLAG2,FTREXP                                                  
         BO    *+8                                                              
         OI    WRKINDS,WRKINOXQ                                                 
         MVI   WRKIACTN,WRKIANDX                                                
*                                                                               
         GOTO1 EZWRKIO,(R4)                                                     
         CLI   WRKIERRS,WRKIEEOF                                                
         BE    LSX                                                              
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   SET ON RETURN                                
         GOTO1 CATCHIOS            SEE IF 90% OF MAX                            
         CLI   ERROR,0             IF ERROR, OVER MAX                           
         BNE   MAXIOSER                                                         
*                                                                               
         BRAS  RE,FAKEIND                                                       
*                                                                               
* LOOP THRU ALL BATCHES - DISLIN DISPLAYS ALL INVOICES IN A BATCH *             
*                                                                               
         L     R1,IDXRDCT                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,IDXRDCT                                                       
*                                                                               
         LA    R3,SVWEZIND                                                      
         USING UKRECD,R3                                                        
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
         OC    WRKEZMOS,WRKEZMOS                                                
         BZ    *+14                                                             
         MVC   HALF,WRKEZMOS                                                    
         B     LS104A                                                           
*                                                                               
         CLI   WRKEZUDT,X'00'                                                   
         JE    *+2                                                              
*                                                                               
         MVC   BYTE,WRKEZUDT                                                    
         BRAS  RE,NEW2OLD                                                       
*                                                                               
LS104A   DS    0H                                                               
         CLC   FTRBMOS,HALF                                                     
         BNE   LS100                                                            
*                                                                               
         DROP  R3                  UKRECD,R3                                    
*                                                                               
         USING EZWKRIXD,R3                                                      
*                                                                               
LS105    DS    0H                                                               
         MVC   SRCESTA(4),EZWISTN  STATION                                      
         MVC   SRCESTA+4(1),EZWIMED                                             
         MVC   ORIGSTA,SRCESTA                                                  
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
         GOTO1 VFILTSYS                                                         
         BNE   LS100                                                            
*                                                                               
LS116    DS    0H                                                               
         OC    RQSTA,RQSTA         STATION FILTER                               
         BZ    *+14                                                             
         CLC   EQUISTA(5),RQSTA                                                 
         BNE   LS100                                                            
*                                                                               
         OC    RQBSEQ,RQBSEQ       BATCH SEQ FILTER                             
         BZ    *+14                                                             
         CLC   RQBSEQ,SVWEZIND+8                                                
         BNE   LS100                                                            
*                                                                               
         MVC   SVSTA(4),EZWISTN  STATION                                        
         MVC   SVSTA+4(1),EZWIMED                                               
*                                                                               
         CLI   FTRMEDIA,0                                                       
         BE    LS128                                                            
*                                                                               
         CLC   FTRMEDIA,EQVMED                                                  
         BNE   LS100                                                            
*                                                                               
         DROP  R3                  EZWKRIXD,R3                                  
*                                                                               
         USING UKRECD,R3                                                        
*                                                                               
LS128    DS    0H                                                               
         OC    SVLSTBAT,SVLSTBAT   ANY LAST BATCH DATA                          
         BZ    LS130                                                            
*                                                                               
LS130    OC    RQDTE,RQDTE         DATE FILTER                                  
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
         DROP  R3                  UKRECD,R3                                    
*                                                                               
LS142    DS    0H                                                               
         L     R1,RECRDCT                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,RECRDCT                                                       
*                                                                               
         MVI   WRKIACTN,WRKIAGET                                                
         GOTO1 EZWRKIO,(R4)                                                     
         CLI   WRKIERRS,WRKIEEOF                                                
         BE    LS100                                                            
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVWCMNT,WRKEZDSC                                                 
*                                                                               
         USING W_RECD,R3                                                        
         MVC   SVWKDTEC,W_AGELD       SAVE DATE AND BATCH SEQ                   
         DROP  R3                  W_RECD,R3                                    
*                                                                               
         LA    R2,SVWCMNT                                                       
         USING EZWKRCMD,R2                                                      
*                                                                               
         OC    FTRSRCE,FTRSRCE     FILTERING ON SOURCE                          
         BZ    LS144                                                            
         CLC   FTRSRCE,EZWCSRCE                                                 
         BNE   LS100                                                            
*                                                                               
         DROP  R2                  EZWKRCMD,R2                                  
*                                                                               
* FOR RECONVERT, CONVERTED, DELETED, DON'T CHECK BATCH CONVERT FLAG             
*                                                                               
* N O T E - UNTIL THE WORKER COMMNENT BUG IS FIXED, DON'T TEST IT *             
*                                                                               
LS144    TM    FTRFLAG,FTRCVQ+FTRDEL+FTRDONE                                    
         BNZ   LS145                                                            
         TM    WRKEZSTA,X'40'      FULLY CONVERTED                              
         NOP   LS100                YES, BYPASS                                 
         B     LS146                                                            
*                                                                               
LS145    TM    WRKEZSTA,X'40'      FULLY CONVERTED                              
         NOP   LS100                NO, BYPASS                                  
*                                                                               
LS146    DS    0H                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 VEZMOD,DMCB,(R6)                                                 
*                                                                               
         XC    SVLSTBAT,SVLSTBAT                                                
*                                                                               
* RETURN HERE ONLY AT END OF BATCH *                                            
*                                                                               
         OC    RQBSEQ,RQBSEQ       BATCH SEQ FILTER                             
         BZ    LS100                NO, NEXT BATCH                              
*                                                                               
LSX      B     XIT                                                              
*                                                                               
         DROP  R4                  WRKIOB,R4                                    
*                                                                               
***********************************************************************         
*  LVREC - VALIDATE RECORD                                            *         
***********************************************************************         
*                                                                               
LVREC    DS    0H                                                               
         MVC   BCLT,=X'0000'                                                    
         MVI   BPRD,0                                                           
         MVI   BPRD2,0                                                          
         XC    QPRD,QPRD                                                        
         XC    QPRD2,QPRD2                                                      
         MVI   SVIHCVPR,0                                                       
         MVI   SVIHCVP2,0                                                       
         XC    SVIHCPRD,SVIHCPRD                                                
         XC    SVIHCPR2,SVIHCPR2                                                
         MVI   SVIHCVES,0                                                       
         MVC   SVIHCVAD,=X'0000'                                                
*                                                                               
         L     R3,ATHISLST                                                      
         USING LIND,R3                                                          
         MVI   SEQNUM,0            START WITH REC SEQ NUMBER 0                  
*                                                                               
         BAS   RE,TSTRO                                                         
*                                                                               
         CLI   LINCLTH+5,0         NO DATA ENTERED?                             
         BE    LVR190                                                           
*                                                                               
* PARSNIP CLIENT-PRODUCT-ESTIMATE LINE                                          
*                                                                               
         LA    R2,LINCLTH                                                       
         XC    BLOCK(50),BLOCK                                                  
         MVI   BYTE,PSNMVOKQ+PSNNPARQ                                           
         GOTO1 VPARSNIP,DMCB,(R2),(4,BLOCK),(BYTE,PARSEPTR)                     
         CLI   8(R1),0                                                          
         BNE   INVINPT                                                          
         MVC   NUMCOMP,4(R1)                                                    
*                                                                               
         CLI   4(R1),X'00'        ANY INPUT AT ALL?                             
         BE    LVR190                                                           
         CLI   4(R1),X'02'        LESS THAN TWO?                                
         BL    INVINPT            PRODUCT MUST BE MISSING                       
         CLI   4(R1),X'04'        MORE THAN 4?                                  
         BH    INVINPT                                                          
*                                                                               
         LA    R4,BLOCK                                                         
         USING PSND,R4                                                          
*                                                                               
* VALIDATE CLIENT                                                               
*     FIRST, FIGURE OUT WHAT MEDIA WE'RE IN                                     
*                                                                               
         ZIC   RF,SELLISTN         GET LINE NUMBER                              
         MH    RF,=AL2(L'INVENT)                                                
         LA    RF,INVLIST(RF)                                                   
         MVC   QMED,INVMEDIA-INVLISTD(RF)                                       
         LA    R1,QMED                                                          
         ICM   R1,8,=AL1(EZMTMEDQ) SEARCH BY MEDIA                              
         GOTO1 VGETMED                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    BAGYMD,X'F0'           TURN OFF MEDIA BITS                       
         OC    BAGYMD,EZMTHEX-EZMEDTBD(RF)                                      
*                                                                               
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
*     BUILD DUMMY FIELD HERE                                                    
*                                                                               
         XC    FAKEFLDH,FAKEFLDH                                                
         XC    FAKEFLD,FAKEFLD                                                  
         MVI   FAKEFLDH,11         8 BYTES HEADER + MAX 3 BYTES CLIENT          
*                                                                               
         ZIC   RF,PSNLEN           LENGTH OF FIRST COMPONENT                    
*                                                                               
         L     R0,PSNCOMP          A(FIRST COMPONENT)                           
         AR    RF,R0               CHAR AFTER FIRST COMPONENT                   
         CLI   0(RF),C' '                                                       
         BNE   INVINPT                                                          
*                                                                               
         ZIC   RF,PSNLEN           LENGTH OF FIRST COMPONENT                    
         STC   RF,FAKEFLDH+5                                                    
         L     R1,PSNCOMP          A(COMPONENT)                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD(0),0(R1)    COPY COMPONENT INTO FAKE FIELD               
         LA    R2,FAKEFLDH                                                      
         MVI   ERROPT,C'Y'                                                      
         GOTO1 VALICLT                                                          
         MVI   ERROPT,C'N'                                                      
         CLI   ERROR,X'00'                                                      
         BE    LVR05                                                            
*                                                                               
         MVC   QCLT(3),FAKEFLD                                                  
         LA    R2,LINCLTH                                                       
         B     TRAPERR                                                          
*                                                                               
LVR05    DS    0H                                                               
         MVC   SVIHCVAD,BCLT       2 BYTE PACKED OVERRIDE CLT                   
         OI    LINCLTH+4,X'20'     SET VALIDATED                                
         MVI   CHANGESW,C'Y'       SET INVOICE CHANGED                          
*                                                                               
* VALIDATE PRODUCT                                                              
*                                                                               
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         L     R4,PSNFLD           NEXT FIELD                                   
*                                                                               
         XC    FAKEFLDH,FAKEFLDH                                                
         XC    FAKEFLD,FAKEFLD                                                  
         MVI   FAKEFLDH,11         8 BYTES HEADER + MAX 3 BYTES PRD             
*                                                                               
         ZIC   RF,PSNLEN           LENGTH OF  COMPONENT                         
         STC   RF,FAKEFLDH+5                                                    
         L     R1,PSNCOMP          A(COMPONENT)                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD(0),0(R1)    COPY COMPONENT INTO FAKE FIELD               
         LA    R2,FAKEFLDH                                                      
         MVI   ERROPT,C'Y'                                                      
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,C'N'                                                      
         CLI   ERROR,X'00'                                                      
         BE    LVR10                                                            
         LA    R2,LINCLTH                                                       
         MVC   QPRD,FAKEFLD                                                     
         B     NOSPTPRD                                                         
*                                                                               
LVR10    DS    0H                                                               
         MVI   CHANGESW,C'Y'       SET INVOICE CHANGED                          
         MVC   QPRD,WORK                                                        
         MVC   BPRD,WORK+3                                                      
         MVC   SVIHCVPR,WORK+3                                                  
         MVC   SVIHCPRD,WORK                                                    
*                                                                               
* CHECK IF NEXT FIELD IS PIGGY OR ESTIMATE                                      
*                                                                               
         MVI   SVESTSPR,0          NO EST, NO EST SPEC RATE                     
         MVC   BYTE,PSNFSEP                                                     
         L     R4,PSNFLD           NEXT FIELD                                   
         CHI   R4,0                IS THERE NEXT FIELD?                         
         BE    LVR190              IF NO - EXIT                                 
*                                                                               
         CLI   BYTE,C'-'   IF SEPARATOR IS "-" EST COMES NEXT                   
         BE    LVR50        OTHERWISE, IT'S PRODUCT 2                           
*                                                                               
* VALIDATE PRODUCT 2                                                            
*                                                                               
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         XC    FAKEFLDH,FAKEFLDH                                                
         XC    FAKEFLD,FAKEFLD                                                  
         MVI   FAKEFLDH,11         8 BYTES HEADER + MAX 3 BYTES PRD             
*                                                                               
         ZIC   RF,PSNLEN           LENGTH OF  COMPONENT                         
         STC   RF,FAKEFLDH+5                                                    
         L     R1,PSNCOMP          A(COMPONENT)                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FAKEFLD(0),0(R1)    COPY COMPONENT INTO FAKE FIELD               
         LA    R2,FAKEFLDH                                                      
         MVI   ERROPT,C'Y'                                                      
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,C'N'                                                      
         CLI   ERROR,X'00'                                                      
         BE    LVR20                                                            
         LA    R2,LINCLTH                                                       
         MVC   QPRD,FAKEFLD                                                     
         B     NOSPTPRD                                                         
*                                                                               
LVR20    DS    0H                                                               
         MVC   QPRD2,WORK                                                       
         MVC   BPRD2,WORK+3                                                     
         MVC   SVIHCVP2,WORK+3                                                  
         MVC   SVIHCPR2,WORK                                                    
*                                                                               
         L     R4,PSNFLD           NEXT FIELD                                   
         CHI   R4,0                IS THERE NEXT FIELD?                         
         BE    LVR190              IF NO - EXIT                                 
*                                                                               
LVR50    DS    0H                                                               
*                                                                               
         LA    R2,LINCLTH                                                       
         L     R1,PSNCOMP                                                       
         ZIC   RF,PSNLEN                                                        
         BCTR  RF,0                                                             
         XC    QEST,QEST                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   QEST(0),0(R1)                                                    
         LA    R2,LINCLTH                                                       
*                                                                               
         TM    PSNSTAT,PSNNUMQ     NUMERIC?                                     
         BNO   INVEST              IF NO - INVALID                              
*                                                                               
LVR60    DS    0H                                                               
*                                                                               
         CLC   PSNNUM,=AL4(255)                                                 
         BH    INVEST                                                           
         MVC   SVIHCVES,PSNNUM+3     SAVE BINARY ESTIMATE                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),SVIHCVAD                                                
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),SVIHCVES                                                
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         LA    R2,LINCLTH                                                       
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NOESTFND                                                         
         OC    QPRD2,QPRD2                                                      
         BZ    LVR180                                                           
         MVC   KEY+4(3),QPRD2                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         LA    R2,LINCLTH                                                       
         BNE   NOESTFND                                                         
*                                                                               
* GET ESTIMATE HEADER FOR DATES AND POSSIBLE SPECIAL RATE *                     
*                                                                               
LVR180   L     R1,AIO1                                                          
         ST    R1,AIO                                                           
         MVC   FILENAME+3(3),=C'FIL'                                            
         GOTO1 GETREC                                                           
*                                                                               
         MVC   SVESTDTS,ESTART-ESTHDR(R1)   SAVE ESTIMATE DATES                 
*                                                                               
LVR190   XC    FILENAME,FILENAME                                                
         OI    LINCLTH+4,X'20'     SET PRODUCT VALIDATED                        
*                                                                               
         MVI   CHANGESW,C'Y'       SET INVOICE CHANGED                          
         MVI   DELETESW,C'N'       SET INVOICE DELETED                          
         MVI   RECONVSW,C'N'       SET RECONVERT OFF                            
         MVI   CONVRTSW,C'N'       SET INVOICE NOT CONVERTED                    
         MVI   RESTORSW,C'N'       SET INVOICE NOT RESTORED                     
*                                                                               
         BRAS  RE,PREZ             FIND RECORD, CALL EZMOD, ETC.                
*                                                                               
LVRX     DS    0H                                                               
         XC    KEY,KEY                                                          
         XC    LASTSELK,LASTSELK                                                
* DEBUG                                                                         
         L     R2,ATHISLST                                                      
         LA    R2,LINCLTH-LIND(R2)                                              
         ST    R2,ACURFORC                                                      
         J     XIT                                                              
         DROP  R4,R3                                                            
*                                                                               
*                                                                               
***********************************************************************         
*   VRHOOK - VALIDATE REC EZMOD HOOK                                  *         
***********************************************************************         
         DS    0H                                                               
VRHOOK   NTR1                                                                   
         LAY   R4,EZWRKIOB                                                      
         USING WRKIOD,R4                                                        
*                                                                               
         CLI   DELETESW,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   RECONVSW,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CONVRTSW,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         ST    R0,SVR0                                                          
         MVC   LISTAR,SPACES                                                    
*                                                                               
         CLI   EZMODE,EZBATL       PROCESS BATCH END                            
         BE    VRH500                                                           
*                                                                               
         CLI   EZMODE,EZINVP       PROCESS INVOICE                              
         BNE   VRHX                 NO                                          
*                                                                               
         MVI   VALFLAG,C'Y'                                                     
*                                                                               
         ZIC   R5,SELLISTN         GET LINE NUMBER                              
         MH    R5,=AL2(L'INVENT)                                                
         LA    R5,INVLIST(R5)                                                   
         USING INVLISTD,R5                                                      
*                                                                               
* IF THIS INVOICE IS BEING CHANGED, CHECK IT LATER *                            
*                                                                               
         CLC   SVIHCNVS,EZIHCNVS                                                
         NOP   VRH100                                                           
         CLC   EZINVSEQ,INVRSEQ      ARE WE AT RIGHT INVOICE                    
         BE    VRH160                                                           
*                                                                               
* CHECK EACH INVOICE - CONVERTED OR DELETED FOR KEEP OR UNKEEP STATUS *         
*                                                                               
VRH100   TM    EZIHCVST,X'20'      RECONVERT                                    
         BO    VRH140               YES                                         
         CLI   RECONVSW,C'Y'       RECONVERT REQUEST                            
         BE    VRH140               YES                                         
         CLI   RESTORSW,C'Y'       RESTORE DELETED REQUEST                      
         BE    VRH140               YES                                         
         TM    EZIHCVST,X'48'      CONVERTED OR DELETED                         
         BNZ   VRH160                                                           
VRH140   MVI   KEEPSW,C'N'                                                      
*                                                                               
VRH160   CLC   EZINVSEQ,INVRSEQ      ARE WE AT RIGHT INVOICE                    
         BL    VRH360                                                           
         BH    VRH400                                                           
         DROP  R5                                                               
         MVI   FOUNDSW,C'Y'                                                     
*                                                                               
         L     R5,EZWKRREC         INVOICE HEADER RECORD                        
         LA    R5,7(,R5)           SKIP RECLEN(4),RECCODE(2),DELIM(1)           
*                                                                               
         NI    EZIHCVST-EZIHCNVS(R5),X'FF'-EZIHCOVR                             
*                                                                               
         OC    SVIHCVES,SVIHCVES   OVERRIDE ESTIMATE?                           
         BZ    VRH165                                                           
         L     R2,ATHISLST                                                      
         LA    R2,(LINCLTH-LIND)(R2)                                            
         GOTO1 DATCON,DMCB,(2,EZIHCMST),(0,SVIHCMST)                            
         GOTO1 (RF),(R1),(2,EZIHCMEN),(0,SVIHCMEN)                              
         CLC   SVESTSTR,SVIHCMEN   EST STR AFTER MOS END                        
         JH    ESTDATER                                                         
         CLC   SVESTEND,SVIHCMST   EST END BEFORE MOS START                     
         JL    ESTDATER                                                         
*                                                                               
VRH165   DS    0H                                                               
         OC    SVIHCVAD,SVIHCVAD   IS THERE AN OVERRIDE CLT?                    
         BZ    *+8                  NO                                          
         OI    EZIHCVST-EZIHCNVS(R5),EZIHCOVR   SET OVERRIDE SWITCH             
*                                                                               
         NI    EZIHCVST-EZIHCNVS(R5),X'FF'-EZIHRCVQ                             
*                                                                               
         CLI   RECONVSW,C'Y'       RECONVERT REQUEST                            
         BNE   VRH170                                                           
         OI    EZIHCVST-EZIHCNVS(R5),EZIHRCVQ                                   
         MVI   KEEPSW,C'N'                                                      
         B     VRH180                                                           
*                                                                               
VRH170   TM    EZIHCVST-EZIHCNVS(R5),EZIHCVQ    CONVERTED?                      
         BO    VRH180                            YES                            
*                                                                               
         CLI   DELETESW,C'Y'       DELETE REQUEST                               
         BE    VRH180               YES                                         
         MVI   KEEPSW,C'N'                                                      
*                                                                               
VRH180   NI    EZIHCVST-EZIHCNVS(R5),X'FF'-EZIHCDEL                             
*                                                                               
         CLI   DELETESW,C'Y'       DELETE REQUEST                               
         BNE   *+8                                                              
         OI    EZIHCVST-EZIHCNVS(R5),EZIHCDEL                                   
*                                                                               
         MVC   EZIHCVAD-EZIHCNVS(2,R5),SVIHCVAD                                 
         MVC   EZIHCVPR-EZIHCNVS(1,R5),SVIHCVPR                                 
         MVC   EZIHCVP2-EZIHCNVS(1,R5),SVIHCVP2                                 
         MVC   EZIHCVES-EZIHCNVS(1,R5),SVIHCVES                                 
         MVC   EZIHSPRT-EZIHCNVS(1,R5),SVIHSPRT                                 
         MVI   EZIHCVND-EZIHCNVS(R5),X'80' STOP DROP TRAILING BLANKS            
*                                                                               
         LHI   R0,36               COLUMN 36 - SECOND SAVE FIELD                
         L     R1,EZWKRREC         INVOICE HEADER RECORD                        
         BRAS  RE,FINDFLD                                                       
         BNE   VRH190                                                           
         MVC   EZIHSPRD-EZIHSAV(L'EZIHSPRD,R1),SVIHCPRD                         
         MVC   EZIHSPR2-EZIHSAV(L'EZIHSPR2,R1),SVIHCPR2                         
         B     VRH192                                                           
*                                                                               
VRH190   DS    0H                                                               
         L     R2,ATHISLST                                                      
         USING LIND,R2                                                          
         LA    R2,LINCLTH                                                       
         CLC   SVIHCVPR,SPACES                                                  
         BH    INVPRDER                                                         
         DROP  R2                                                               
*                                                                               
VRH192   DS    0H                                                               
         CLI   CHANGESW,C'Y'       WAS INVOICE CHANGED                          
         BNE   VRHX                 NO                                          
*                                                                               
         MVI   WRKIACTN,WRKIAPUT                                                
         GOTO1 EZWRKIO,(R4)                                                     
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                                                               
* IF RESTORE OR RECONVERT, MUST SET OFF BATCH CONVERT/KEEP IF NEEDED *          
*                                                                               
         CLI   RESTORSW,C'Y'       RESTORE DELETED REQUEST                      
         BE    VRH200               YES                                         
         CLI   RECONVSW,C'Y'       RECONVERT REQUEST                            
         BNE   VRHX                 NO                                          
*                                                                               
VRH200   DS    0H                                                               
         TM    WRKEZSTA,X'08'      THIS FILE ON KEEP                            
         BZ    VRHX                       NO                                    
*                                                                               
         MVI   WRKIACTN,WRKIAUKE                                                
         GOTO1 EZWRKIO,(R4)                                                     
         CLI   WRKIERRS,0                                                       
         BE    VRHX                                                             
         DC    H'0'                                                             
*                                                                               
VRH300   L     RD,SVR0             DON'T GO BACK TO EZMOD                       
         B     VRHX                                                             
*                                                                               
VRH360   MVI   EZMODE,EZINVL       SKIP TO NEXT INVOICE                         
         B     VRHX                                                             
*                                                                               
VRH400   CLI   FOUNDSW,C'Y'                                                     
         BE    VRHX                                                             
         DC    H'0'                                                             
*                                                                               
* AT BATCH END, SET OR RESET BOTH CONVERTED AND KEEP STATUS *                   
*                                                                               
* IF RESTORE, OR RECONVERT FOR BATCH IN KEEP STATUS, UNKEEP *                   
*                                                                               
* IF DELETE, AND ALL OTHER INVOICES IN BATCH CONVERTED OR                       
*    DELETED, SET BATCH CONVERT BIT ON AND STATUS TO KEEP                       
*                                                                               
VRH500   DS    0H                                                               
* 4/24/2014 BATCH IS MARKED "KEEP" BY SPEZF07                                   
* SKIPPING "KEEP" CODE IN SPEZF27 ALLTOGETHER                                   
         B     VRHX                                                             
*                                                                               
         CLI   KEEPSW,C'Y'         IF ALL INVOICES CONVERTED OR                 
         BE    VRH550               DELETED, SHOULD BE STATUS KEEP              
*                                                                               
VRH520   DS    0H                                                               
         TM    WRKEZSTA,X'08'      THIS FILE ON KEEP                            
         BZ    VRHX                       NO                                    
*                                                                               
         MVI   WRKIACTN,WRKIAUKE                                                
         GOTO1 EZWRKIO,(R4)                                                     
         CLI   WRKIERRS,0                                                       
         BE    VRHX                                                             
         DC    H'0'                                                             
*                                                                               
* ENTIRE BATCH DELETED OR CONVERTED, MUST SET TO CONVERTED & KEEP *             
*                                                                               
VRH550   DS    0H                                                               
         TM    WRKEZSTA,X'08'      THIS FILE ON KEEP                            
         BO    VRHX                       YES                                   
*                                                                               
         MVI   WRKIACTN,WRKIAKEE                                                
         GOTO1 EZWRKIO,(R4)                                                     
         CLI   WRKIERRS,0                                                       
         BE    VRHX                                                             
         DC    H'0'                                                             
*                                                                               
VRHX     J     XIT                                                              
*                                                                               
         DROP  R4                  WRKIOB,R4                                    
*                                                                               
***********************************************************************         
*   DKEY - DISPLAY KEY                                                *         
***********************************************************************         
*                                                                               
DKEY     DS    0H                                                               
         B     XIT                                                              
*                                                                               
*                                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
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
NOESTFND L     R1,=A(NOESTMS)                                                   
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
CVTCHGER L     R1,=A(CVTCHGMS)                                                  
         LA    R2,DIVCCDH                                                       
         B     ERREXIT                                                          
PFKERR   L     R1,=A(INVPFK)                                                    
         LA    R2,LUPFRSTH                                                      
         B     ERREXIT                                                          
INVERR   L     R1,=A(INVINV)                                                    
         LA    R2,LUPFRSTH                                                      
         B     ERREXIT                                                          
INVINPT  L     R1,=A(INPTERR)                                                   
         B     ERREXIT                                                          
NOUPDER  L     R1,=A(NOCHANGE)                                                  
         B     ERREXIT                                                          
INVPRDER L     R1,=A(INVPRMSG)                                                  
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
         MVC   CONHEAD(L'NOPRDMS),NOPRDMS                                       
         MVC   CONHEAD+23(3),QPRD                                               
         B     ERREXITA                                                         
INVEST   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVESTMS),INVESTMS                                     
         MVC   CONHEAD+19(3),QEST                                               
         B     ERREXITA                                                         
CNVDELER L     R1,=A(CNVDELMS)                                                  
         B     ERREXIT                                                          
SPRATERR L     R1,=A(SPRATMS)                                                   
         B     ERREXIT                                                          
ESTDATER L     R1,=A(ESTDATMS)                                                  
         B     ERREXIT                                                          
MAXIOSER L     R1,=A(MAXIOSMS)                                                  
         B     ERREXITC                                                         
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
         ZIC   RF,0(R1)                                                         
         EX    RF,ERREXITM                                                      
         MVC   CONHEAD(10),=C'* ERROR * '                                       
*                                                                               
ERREXITA DS    0H                                                               
         XC    ACURFORC,ACURFORC                                                
         GOTO1 ERREX2                                                           
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
*                                                                               
TRAPERR  DS    0H                                                               
         XC    ACURFORC,ACURFORC                                                
         GOTO1 ERREX                                                            
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         LTORG                                                                  
PARSEPTR DC    AL1(1),C'/'         PARSNIP SEPARATOR OVERRIDE LIST              
         DC    AL1(3),C' ,-'                                                    
         DC    AL1(1),C','                                                      
         DC    AL1(1),C','                                                      
*                                                                               
CHGOKMS  DC    C'* IF CHANGES OK, HIT ENTER TO RESUME *'                        
NOSPCLMS DC    C'* ERROR * CLIENT CODE XXX NOT ON SPOTPAK *'                    
         DC    AL1(L'NOPRDMS-1)                                                 
NOPRDMS  DC    C'* ERROR * PRODUCT CODE XXX NOT FOUND *'                        
         DC    AL1(L'INVESTMS-1)                                                
INVESTMS DC    C'* ERROR * ESTIMATE XXX INVALID *'                              
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
         DC    AL1(L'ESTENTMS-1)                                                
ESTENTMS DC    C'ESTIMATE MUST BE ENTERED LAST *'                               
         DC    AL1(L'MORPRDMS-1)                                                
MORPRDMS DC    C'ONLY PRD AND PTR ALLOWED *'                                    
         DC    AL1(L'PRDSIZMS-1)                                                
PRDSIZMS DC    C'PRD, PTR, EST MAX SIZE = 3 *'                                  
         DC    AL1(L'PRDENTMS-1)                                                
PRDENTMS DC    C'ENTER AS PRD OR PRD,EST OR PRD,PTR-EST *'                      
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
         DC    AL1(L'INPTERR-1)                                                 
INPTERR  DC    C'ENTER "CLT PRD-EST" OR "CLT PRD,PTR-EST" *'                    
         DC    AL1(L'INVPFK-1)                                                  
INVPFK   DC    C'TO USE PF KEYS PUT CURSOR ON LIST LINE'                        
         DC    AL1(L'INVINV-1)                                                  
INVINV   DC    C'NO INVOICE NUMBER - CANNOT SELECT RECORD'                      
         DC    AL1(L'NOCHANGE-1)                                                
NOCHANGE DC    C'NO UPDATES ALLOWED'                                            
         DC    AL1(L'INVPRMSG-1)                                                
INVPRMSG DC    C'INVALID PRODUCT CODE'                                          
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
         DROP  R7,RB                                                            
*                                                                               
*                                                                               
***********************************************************************         
*   PREZ - FIND RECORD, CALL EZMOD                                    *         
***********************************************************************         
         DS    0H                                                               
PREZ     NTR1  BASE=*,LABEL=*                                                   
         MVI   USEIO,C'Y'                                                       
*                                                                               
         BRAS  RE,INITEZB                                                       
         BRAS  RE,INITWKB                                                       
*                                                                               
         CLI   MODE,LVALREC        VALIDATE RECORD                              
         BNE   *+8                                                              
         MVI   USEIO,C'N'                                                       
*                                                                               
         LA    R5,INVLIST                                                       
         USING INVLISTD,R5                                                      
*                                                                               
         MVC   SVBUID,INVUID                                                    
         MVC   SVBWKFLN,INVBSEQ                                                 
         MVC   SVBINVSQ,INVRSEQ                                                 
*                                                                               
         ZIC   RF,SELLISTN         RELATIVE LINE NUMBER                         
         MH    RF,=AL2(L'INVENT)                                                
         LA    R5,INVLIST(RF)                                                   
         MVC   SRCESTA,INVSTA      STATION                                      
*                                                                               
         GOTO1 VEQVSTA             GO GET EQUIVALENT STATION                    
*                                                                               
* READ STATION MASTER RECORD TO OBTAIN SFLAG1                                   
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(L'STAKEY-1),KEY                                            
         LA    R4,KEY                                                           
         USING STAREC,R4                                                        
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,EQVMED                                                   
         MVC   STAKCALL,EQUISTA                                                 
         MVC   STAKAGY,AGENCY                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO1                     
         CLI   8(R1),0             ANY ERROR                                    
         BNE   PREZ10               NO                                          
         L     R4,AIO1                                                          
         TM    SFLAG1,STPG                                                      
         BZ    *+12                                                             
         TM    SVCOPT4,COP4PG                                                   
         BZ    PGERR                                                            
         DROP  R4                                                               
*                                                                               
PREZ10   DS    0H                                                               
         LAY   R4,EZWRKIOB                                                      
         USING WRKIOD,R4                                                        
*                                                                               
         XC    WRKEZKEY,WRKEZKEY                                                
         MVC   WRKEZUID,INVUID                                                  
         MVC   WRKEZSQN,INVBSEQ                                                 
*                                                                               
         DROP  R5                                                               
*                                                                               
         TM    FTRFLAG2,FTREXP                                                  
         BO    *+8                                                              
         OI    WRKINDS,WRKINOXQ                                                 
         OI    WRKINDS,WRKISEQQ                                                 
         MVI   WRKIACTN,WRKIANDX                                                
*                                                                               
         GOTO1 EZWRKIO,(R4)                                                     
         CLI   WRKIERRS,WRKIEEOF                                                
         BE    BATMOVER                                                         
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
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
*                                                                               
         MVC   EZSPTNET,SPOTNETS                                                
         MVC   EZEQVSTA,EQUISTA                                                 
         L     RE,=A(VRHOOK)       VALIDATE HOOK ROUTINE                        
         A     RE,RELO                                                          
         ST    RE,EZHOOK                                                        
         MVI   KEEPSW,C'Y'                                                      
         MVC   EZWKRIND,SVWEZIND                                                
*                                                                               
         TM    FTRFLAG,FTRTRACE                                                 
         BZ    *+8                                                              
         OI    EZTRACE,X'F0'                                                    
*                                                                               
         MVI   EZWRITE,C'N'                                                     
         MVI   FOUNDSW,C'N'                                                     
         L     RF,4(,RD)           BACK UP CHAIN 1                              
         L     R7,48(,RF)                                                       
         L     RB,64(,RF)                                                       
         GOTO1 VEZMOD,DMCB,(R6)                                                 
*                                                                               
         J     XIT             RET AT BATCH END-DONE ON RET FROM EZMOD          
*                                                                               
BATMOVER L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BATMOVMS),BATMOVMS                                     
         LA    R2,LUPSTAH                                                       
         GOTO1 ERREX2                                                           
*                                                                               
PGERR    L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PGERRMSG),PGERRMSG                                     
         LA    R2,LUPSTAH                                                       
         GOTO1 ERREX2                                                           
*                                                                               
BATMOVMS DC    C'* ERROR * BATCH MOVED OR SYSTEM ERROR-CALL DDS *'              
PGERRMSG DC    C'CLIENT CODE NOT AUTHORIZED FOR THIS STATION'                   
         DS    0H                                                               
         DROP  R4,RB                                                            
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*   DISLIN DISPLAY LINE ON LISTAR, OR PRINT LINE TO SPOOL             *         
***********************************************************************         
*                                                                               
         DS    0H                                                               
DISLIN   NTR1  BASE=*,LABEL=*                                                   
         LAY   R4,EZWRKIOB                                                      
         USING WRKIOD,R4                                                        
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
*                                                                               
DL100    DS    0H                                                               
         TM    FTRFLAG2,FTRIM      IM FILTER?                                   
         BZ    *+12                NO                                           
         TM    EZIHCVST,X'01'      CAME FROM IM?                                
         BZ    DL360               NO - SKIP IT                                 
*                                                                               
         TM    FTRFLAG2,FTRNONIM   NON-IM FILTER?                               
         BZ    *+12                NO                                           
         TM    EZIHCVST,X'01'      CAME FROM IM?                                
         BO    DL360               YES, SKIP IT                                 
*                                                                               
DL110    DS    0H                                                               
         CLI   FTRCLTN,0           TEST HAVE CLIENT NAME FILTER                 
         BNH   DL140                                                            
         ZIC   RF,FTRCLTNL         LENGTH - 1                                   
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
         ZIC   RF,FTRPRDNL         TEST PRODUCT FILTER                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZIHPRDN(0),FTRPRDN                                              
         BNE   DL360                                                            
*                                                                               
DL160    CLI   FTRINVNO,0          TEST HAVE INVOICE FILTER                     
         BNH   DL170                                                            
         ZIC   RF,FTRINVLN         GET INVNO LENGTH                             
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
DL200    DS    0H                                                               
*                                                                               
         L     R2,ATHISLST                                                      
         USING LIND,R2                                                          
*                                                                               
         LA    R0,LUPFRSTH            IF WE'RE AT LINE 1                        
         CR    R0,R2                                                            
         BNE   DL205                                                            
*                                     SAVE BATCH, INVOICE INFO                  
         MVC   DMDSKADD(4),=X'0000FFFF'                                         
         MVI   NLISTS,NUMLINS                                                   
*                                                                               
DL205    DS    0H                                                               
         MVC   LINFRST,SPACES                                                   
         MVC   LINCLT,SPACES                                                    
         MVC   LINSCND,SPACES                                                   
*                                                                               
         OI    LINFRSTH+6,X'80'                                                 
         OI    LINCLTH+6,X'80'                                                  
         OI    LINSCNDH+6,X'80'                                                 
         OI    LINFRSTH+4,X'20'                                                 
         OI    LINCLTH+4,X'20'                                                  
         OI    LINSCNDH+4,X'20'                                                 
*                                                                               
         OI    LINFRSTH+6,X'20'                                                 
         OI    LINSCNDH+6,X'20'                                                 
*                                                                               
* B-DTE                                                                         
         GOTO1 DATCON,DMCB,(2,SVWKDTEC),(4,LINBDAT)                             
* CONV DATE                                                                     
         OC    EZIHCVDT,EZIHCVDT                                                
         BZ    DL210                                                            
         GOTO1 DATCON,DMCB,(1,EZIHCVDT),(4,LINCDAT)                             
         TM    EZIHCVST,X'40'      THIS BETTER BE CONVERTED                     
         BO    *+6                                                              
         DC    H'0'                OOPS!                                        
*                                                                               
DL210    DS    0H                                                               
* SRCE                                                                          
         MVC   LINSRCE,SVWCSRCE                                                 
* MON                                                                           
         MVC   LINMON(3),EZIHDMOS                                               
         MVC   LINMON+3(2),EZIHDMOS+4                                           
         MVC   LINSTA(7),PRTSTA7C                                               
*                                                                               
         LA    R1,KEY                                                           
         USING STAKEY,R1                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVI   STAKTYPE,STAKTYPQ                                                
         MVC   STAKMED,EQVMED                                                   
         MVC   STAKCALL,EQUISTA                                                 
         MVC   STAKAGY,AGENCY                                                   
         MVI   STAKCLT,C'0'                                                     
         MVC   STAKCLT+1(L'STAKCLT+L'STAKFILL-1),STAKCLT                        
         DROP  R1                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO1,0                   
*                                                                               
         CLI   8(R1),0             ANY ERROR                                    
         BE    *+8                  NO                                          
         MVI   LINSTA+7,C'*'                                                    
*                                                                               
DL212    CLC   EZIHINST,=CL25'BILLING INVOICE REVERSAL'                         
         BNE   *+8                                                              
         MVI   LINSTAT,C'D'                                                     
* CLT                                                                           
         MVC   LINCLT,SPACES                                                    
         TM    EZIHCVST,EZIHCVQ    TEST CONVERTED                               
         BO    *+12                YES - MUST HAVE CLT CODE                     
*                                  PROCESS UNCONVERTED INVOICE HERE             
         TM    EZIHCVST,EZIHCOVR   TEST OVERRIDE CLT/PRD/EST                    
         BZ    DL250               NO - TRY AGENCY CLIENT CODE                  
*                                                                               
         OC    EZIHCVAD,EZIHCVAD   TEST HAVE OVERRIDE CLIENT CODE               
         BNZ   *+14                                                             
         MVC   LINCLT(3),=C'???'                                                
         B     DL270                                                            
*                                                                               
         LA    R1,EQVMED                                                        
         ICM   R1,8,=AL1(EZMTMEDQ) SEARCH BY MEDIA                              
         GOTO1 VGETMED                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    BAGYMD,X'F0'           TURN OFF MEDIA BITS                       
         OC    BAGYMD,EZMTHEX-EZMEDTBD(RF)                                      
*                                  SIMULATE CALL TO VALICLT                     
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),EZIHCVAD                                                
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     BIG BAD ERROR                                
         BE    *+14                CLIENT FOUND, PROCEED                        
         MVC   LINCLT(3),=C'???'   CLIENT NOT ON FILE - DISPLAY '???'           
         B     DL270               SKIP PRD,EST DISPLAY                         
*                                                                               
         L     R3,AIO1                                                          
         ST    R3,AIO                                                           
         MVC   FILENAME+3(3),=C'FIL'                                            
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
         LA    R0,CLIST-CLTHDR(R3)                                              
         LA    R1,880                                                           
         LA    RE,SVCLIST                                                       
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R0,CLIST2-CLTHDR(R3)                                             
         LHI   R1,140              LENGTH OF CLIST2                             
         LR    RF,R1                                                            
         CLC   CLEN-CLTHDR(2,R3),=H'1280'   TEST CLIST2 IN RECORD               
         BH    *+8                 YES                                          
         SR    R1,R1               ELSE JUST CLEAR REST OF LIST                 
         SR    RF,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   BYTE,CPROF+6-CLTHDR(R3)    PRINT CLT CODE AS AAN                 
         GOTO1 CLUNPK,DMCB,(BYTE,EZIHCVAD),QCLT                                 
*                                                                               
         MVC   LINCLT(3),QCLT                                                   
         OC    LINCLT(3),SPACES                                                 
* PRD                                                                           
         CLI   EZIHCVPR,0          TEST HAVE PRODUCT                            
         BE    DL223               NO - SKIP EST DISPLAY                        
*                                                                               
         LA    RE,255                                                           
         LA    RF,SVCLIST                                                       
DL222    CLC   EZIHCVPR,3(RF)                                                   
         BE    DL224                                                            
         LA    RF,4(,RF)                                                        
         CLI   0(RF),C' '                                                       
         BNH   *+8                                                              
         BCT   RE,DL222                                                         
*                                                                               
         MVC   LINPRD(3),=C'???'   NO PRODUCT FOUND                             
         B     DL270                                                            
*                                                                               
DL223    DS    0H                                                               
         CLI   SPOTNETS,C'N'                                                    
         BNE   DL270                                                            
         CLC   EZIHSPRD,SPACES                                                  
         BNH   DL270                                                            
         LA    RF,EZIHSPRD                                                      
*                                                                               
DL224    MVC   LINPRD(3),0(RF)                                                  
         MVI   LINPRD+3,C' '                                                    
*                                                                               
DL226    DS    0H                                                               
         LA    R3,LINPRD                                                        
         LHI   R0,3                                                             
         CLI   0(R3),C' '                                                       
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         BCT   R0,*-12                R2 POINTS TO NEXT SPACE                   
* PR2                                                                           
         CLI   EZIHCVP2,0          TEST HAVE PRODUCT 2                          
         BE    DL233               NO PRD2 - GO TO ESTIMATE                     
         MVI   0(R3),C','                                                       
         LA    RE,255                                                           
         LA    RF,SVCLIST                                                       
*                                                                               
DL230    CLC   EZIHCVP2,3(RF)                                                   
         BE    DL234                                                            
         LA    RF,4(,RF)                                                        
         CLI   0(RF),C' '                                                       
         BNH   *+8                                                              
         BCT   RE,DL230                                                         
*                                                                               
         MVC   1(3,R3),=C'???'   NO PRODUCT FOUND                               
         B     DL234                                                            
*                                                                               
DL233    DS   0H                                                                
         CLI   SPOTNETS,C'N'                                                    
         BNE   DL240                                                            
         CLC   EZIHSPR2,SPACES                                                  
         BNH   DL240                                                            
         LA    RF,EZIHSPR2                                                      
*                                                                               
DL234    DS    0H                                                               
         MVC   1(3,R3),0(RF)     OUTPUT PRODUCT 2                               
*                                                                               
DL236    DS    0H                                                               
         AHI   R3,1                TO ACCOMODATE ','                            
         LHI   R0,3                                                             
         CLI   0(R3),C' '                                                       
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         BCT   R0,*-12                R3 POINTS TO NEXT SPACE                   
* EST                                                                           
DL240    CLI   EZIHCVES,0                                                       
         BE    DL270                                                            
         MVI   0(R3),C'-'                                                       
         ZIC   RE,EZIHCVES                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(3,R3),DUB                                                      
         B     DL270                                                            
*                                                                               
DL250    DS    0H                  DISPLAY AGENCY CLIENT/PRD/EST                
         OC    EZIHAAID,EZIHAAID   TEST HAVE AGENCY ADVERTISER CODE             
         BZ    DL260               NO - TRY ADVERTISER NAME                     
*                                                                               
         MVC   LINCLT(L'EZIHAAID),EZIHAAID                                      
         OC    LINCLT,SPACES                                                    
         LA    R3,LINCLT                                                        
         ICM   R3,8,=AL1(L'EZIHAAID)                                            
         BRAS  RE,LASTCHAR                                                      
         BNE   DL260                                                            
         AHI   R3,1                                                             
***                                                                             
         OC    EZIHAPID,EZIHAPID   TEST HAVE AGENCY PRODUCT CODE                
         BZ    DL270                                                            
*                                                                               
         MVC   0(L'EZIHAPID,R3),EZIHAPID                                        
         LA    R3,LINPRD                                                        
         ICM   R3,8,=AL1(L'EZIHAPID)                                            
         BRAS  RE,LASTCHAR                                                      
         BNE   DL270                                                            
*                                                                               
         OC    EZIHEST,EZIHEST     TEST HAVE AGENCY ESTIMATE CODE               
         BZ    DL270                                                            
*                                                                               
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         LA    RF,LINCLT+L'LINCLT                                               
         SR    RF,R3                                                            
         CHI   RF,L'EZIHEST                                                     
         BL    *+8                                                              
         LHI   RF,L'EZIHEST                                                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     DL270                                                            
         MVC   0(0,R3),EZIHEST                                                  
*                                                                               
DL260    DS    0H                                                               
         MVC   LINCNAME(L'LINCNAME),EZIHADVN                                    
*                                                                               
         LA    R3,LINCNAME                                                      
         MVI   BYTE,L'LINCNAME                                                  
         ICM   R3,8,BYTE                                                        
         BRAS  RE,LASTCHAR                                                      
         BE    *+14                                                             
         MVC   LINCNAME(3),=C'???'                                              
         B     DL270                                                            
         MVI   0(R3),C'*'                                                       
         AHI   R3,1                         ADVANCE TO NEXT SPACE               
* * * * *                                                                       
         MVC   0(L'LINPNAME,R3),EZIHPRDN     PRODUCT NAME                       
*                                                                               
         MVI   BYTE,L'LINPNAME                                                  
         ICM   R3,8,BYTE                                                        
         BRAS  RE,LASTCHAR                                                      
         BE    *+14                                                             
         MVC   0(3,R3),=C'???'                                                  
         B     DL270                                                            
*                                                                               
         MVI   0(R3),C'*'                                                       
         AHI   R3,1                         ADVANCE TO NEXT SPACE               
* * * * *                                                                       
         MVC   0(3,R3),EZIHEST                                                  
         OC    LINCLT,SPACES                                                    
*                                                                               
DL270    DS    0H                                                               
         CLI   FTRQCLT,0           FILTER ON CLIENT CODE                        
         BE    *+14                                                             
         CLC   LINCLT(3),FTRQCLT                                                
         BNE   DL360                                                            
*                                                                               
         CLI   FTRQPRD,0           FILTER ON PRODUCT CODE                       
         BE    DL275                                                            
         ZIC   RF,FTRQPRDL                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   LINPRD(0),FTRQPRD                                                
         BNE   DL360                                                            
*                                                                               
* INV                                                                           
DL275    DS    0H                                                               
         MVC   LININV,EZIHINV                                                   
         OC    LININV,SPACES                                                    
         CLC   LININV,SPACES                                                    
         BNE   *+10                                                             
         MVC   LININV,=10C'*'                                                   
*                                                                               
         CLC   EZIHTSPN,=AL4(9999)                                              
         BNH   DL275A                                                           
*                                                                               
         MVC   LINSPTS,=C'>10K'                                                 
         NI    LINSPTS+3,X'BF'     LOWERCASE                                    
         B     DL275B                                                           
*                                                                               
DL275A   DS    0H                                                               
         EDIT  (B4,EZIHTSPN),LINSPTS,ZERO=NOBLANK                               
*                                                                               
DL275B   DS    0H                                                               
         MVI   LINSPTS+L'LINSPTS,C' '                                           
*                                                                               
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   DL277                                                            
*                                                                               
         TM    EZIHCVST,X'01'      CAME FROM IM?                                
         BZ    DL277                                                            
         MVI   LINIMFLG,C'I'                                                    
         NI    LINIMFLG,X'BF'                                                   
*                                                                               
DL277    DS    0H                                                               
         TM    EZIHCVST,EZIHCVQ    TEST CONVERTED                               
         BZ    *+8                  NO                                          
         MVI   LINSTAT,C'C'                                                     
*                                                                               
         TM    EZIHCVST,EZIHCDEL   TEST DELETED                                 
         BZ    *+8                  NO                                          
         MVI   LINSTAT,C'D'                                                     
*                                                                               
         TM    EZIHCVST,EZIHRCVQ   TEST RECONVERT                               
         BZ    *+8                  NO                                          
         MVI   LINSTAT+1,C'R'                                                   
*                                                                               
         TM    EZIHCVST,EZIHCOVR   TEST OVERRIDE CLT/PRD/EST                    
         BZ    *+8                  NO                                          
         MVI   LINSTAT+2,C'O'                                                   
*                                                                               
DL280    LM    RE,R0,BTOT                                                       
         LA    RE,1(,RE)                                                        
         A     RF,EZIHTSPN                                                      
         ICM   R1,15,EZITBDUE                                                   
         AR    R0,R1                                                            
         STM   RE,R0,BTOT                                                       
*                                                                               
         XC    LUPTL,LUPTL                                                      
         OI    LUPTLH+6,X'80'                                                   
*                                                                               
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   DL284                                                            
*                                                                               
         EDIT  (B4,IDXRDCT),(6,LUPTL),COMMAS=YES                                
         MVC   LUPTL+6(6),=C'=INDEX'                                            
         EDIT  (B4,RECRDCT),(6,LUPTL+13),COMMAS=YES                             
         MVC   LUPTL+19(5),=C'=GETS'                                            
*                                                                               
DL284    L     R5,INVLPTR          POINTER INTO SAVE LIST                       
         LA    R0,WRKFBUFR                                                      
         CR    R0,R5                                                            
         BH    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING INVLISTD,R5                                                      
*                                                                               
         MVC   INVUID,WRKEZUID                                                  
*                                                                               
         MVC   INVMEDIA,EQVMED                                                  
*                                                                               
         MVC   INVSTA,WRKEZSCL                                                  
         MVC   INVSTA+4(1),WRKEZMED                                             
         MVC   INVBDTE,WRKEZBDT                                                 
         MVC   INVBSEQ,WRKEZSQN                                                 
         MVC   INVRSEQ,EZINVSEQ                                                 
         MVC   INVNET,EZIHNET                                                   
         LA    R5,INVNEXT                                                       
         ST    R5,INVLPTR                                                       
         DROP  R5                  INVLISTD,R5                                  
*                                                                               
         MVC   DMDSKADD(4),=X'0000FFFF'   DUMMY DISK ADDRESS                    
         MVI   NLISTS,NUMLINS                                                   
*                                                                               
* SET SVLSTBAT (LAST BATCH INFORMATION)                                         
         MVC   SVBUID,WRKEZUID                                                  
         MVC   SVBWKFLN,WRKEZSQN                                                
         MVC   SVBINVSQ,EZINVSEQ                                                
*                                                                               
         CLI   LINSTAT,C'C'     CONVERTED?                                      
         BE    DL285                                                            
         CLI   LINSTAT,C'D'     DELETED?                                        
         BE    DL285                                                            
         CLC   LININV,=10C'*'     NO INVOICE NUMBER?                            
         BE    DL285                                                            
         CLI   LINSTAT+1,C'R'     RECONVERTED?                                  
         BNE   DL286                                                            
*                                                                               
DL285    DS    0H                                                               
         OI    LINCLTH+6,X'20'    IF YES-PROTECT, TO DISALLOW CHANGES           
*                                                                               
DL286    DS    0H                                                               
*                                                                               
         GOTO1 LISTMON                                                          
*                                                                               
         B     DLX                                                              
*                                                                               
DL360    XC    LISTAR,LISTAR                                                    
         MVI   EZMODE,EZINVL       SKIP TO NEXT INVOICE                         
*                                                                               
DLX      DS    0H                                                               
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         DROP  RB,R4                                                            
*                                                                               
*                                                                               
*********                                                                       
* SETUP *                                                                       
*********                                                                       
SETUP    NTR1  BASE=*,LABEL=*                                                   
         XC    DMCB,DMCB                                                        
*                                                                               
         MVC   DMCB+4(4),=X'D9000A2C' DDWRKIO                                   
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VWRKIO,0(R1)                                                     
*                                                                               
         GOTO1 CALLOV,DMCB,(X'10',0),ATWA                                       
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                WHERE IS T23010                              
         L     RF,DMCB                                                          
         ST    RF,VEZMOD                                                        
*                                                                               
         MVI   SVLINNUM,X'00'                                                   
*                                                                               
         L     R1,ACOMFACS                                                      
         MVC   VPARSNIP,CPARSNIP-COMFACSD(R1)                                   
*                                                                               
         OI    GENSTAT1,NOSETEFH                                                
         OI    GLSTSTAT,APPLCDSP+NOSELFLD+CHNGLIST                              
*                                                                               
         CLC   TWASCR,CALLSTCK                                                  
         BNE   *+12                                                             
         MVI   CALLSP,0                                                         
         MVI   CALLSTCK,0                                                       
*                                                                               
         XC    LUPPFKY,LUPPFKY                                                  
*                                                                               
         OI    CONSERVH+6,X'80'                                                 
         OI    CONSERVH+1,X'01'                                                 
*                                                                               
         MVC   LUPPFKY+14(12),=C'PF04=CHANGE'                                   
         MVC   LUPPFKY(12),=C'PF03=DISPLAY'                                     
         OI    LUPPFKYH+6,X'80'                                                 
*                                                                               
         OC    PFKEY,PFKEY                                                      
         BZ    SETUPX                                                           
*                                                                               
         XC    TWASVSTA,TWASVSTA                                                
         XC    TWASVSEQ,TWASVSEQ                                                
         XC    TWASVDAT,TWASVDAT                                                
*                                                                               
         L     R1,SYSPARMS         RF = A(TIOB)                                 
         L     RF,0(R1)                                                         
         USING TIOBD,RF                                                         
         SR    R1,R1                                                            
         ICM   R1,3,TIOBCURD         SAVE CURSOR DISPLACEMENT                   
         AR    R1,RA                 ABSOLUTE ADDRESS OF CURSOR                 
         LA    R0,LUPFRSTH                                                      
         DROP  RF                                                               
*                                                                               
         CR    R1,R0                                                            
         JL    NEQXIT                                                           
*                                                                               
         SR    R1,R0                                                            
         SR    R0,R0                                                            
         D     R0,=AL4(LUPSCNDH-LUPFRSTH)                                       
*                                                                               
         CHI   R1,NUMLINS-1                                                     
         JH    NEQXIT                                                           
*                                                                               
         CLI   PFKEY,3                                                          
         BE    *+12                                                             
         CLI   PFKEY,4                                                          
         BNE   SETUP10                                                          
*                                                                               
         LA    RF,INVLIST                                                       
         USING INVLISTD,RF                                                      
*                                                                               
         MVC   SVBUID,INVUID                                                    
         MVC   SVBWKFLN,INVBSEQ                                                 
         MVC   SVBINVSQ,INVRSEQ                                                 
*                                                                               
         DROP  RF                                                               
*                                                                               
         LR    R2,R1                                                            
         MHI   R2,LUPSCNDH-LUPFRSTH                                             
         LA    R0,LUPFRSTH                                                      
         AR    R2,R0                                                            
         CLC   77(10,R2),=10C'*'                                                
         JE    NEQXIT                                                           
*                                                                               
SETUP10  DS    0H                                                               
         LR    RF,R1                 LINE NUMBER - 1                            
         MHI   RF,L'INVENT           DISPLACEMENT INTO INVLIST                  
         LA    RF,INVLIST(RF)                                                   
         OC    0(L'INVENT,RF),0(RF)                                             
         JZ    NEQXIT                                                           
*                                                                               
         STC   R1,SELLISTN                                                      
         STC   R1,SVLINNM2                                                      
*                                                                               
         MHI   R1,L'INVENT                                                      
         LA    R2,INVLIST(R1)                                                   
         USING INVLISTD,R2                                                      
*                                                                               
* STATION                                                                       
         MVC   SRCESTA,INVSTA      STATION                                      
         CLI   SRCESTA+3,C'.'                                                   
         BNE   *+8                                                              
         MVI   SRCESTA+3,C' '                                                   
         GOTO1 VEQVSTA             GO GET EQUIVALENT STATION                    
         XC    WORK,WORK          BATCH STATION                                 
*                                                                               
SETUP130 MVC   WORK(L'PRTSTA7C),PRTSTA7C                                        
         OC    INVNET,INVNET     THIS A CABLE HEAD                              
         BZ    SETUP134                                                         
         MVI   WORK+4,C'/'                                                      
         MVC   WORK+5(3),INVNET                                                 
*                                                                               
SETUP134 CLC   TWASVSTA,WORK                                                    
         BE    *+10                                                             
         MVC   TWASVSTA,WORK                                                    
* B-DTE                                                                         
         GOTO1 DATCON,DMCB,(2,INVBDTE),(5,TWASVDAT)                             
* SEQ                                                                           
         SR    R3,R3                                                            
         ICM   R3,3,INVBSEQ                                                     
         EDIT  (R3),(5,TWASVSEQ),ALIGN=LEFT                                     
         DROP  R2                                                               
*                                                                               
         GOTO1 INITPFKY,DMCB,PFTABLE                                            
SETUPX   J     EQXIT                                                            
*                                                                               
*                                                                               
***********************************************************************         
*        PFKEYS TABLES                                                *         
***********************************************************************         
PFTABLE  DS    0H                                                               
*        EASI INVOICE DISPLAY SCREEN                                            
         DC   AL1(PF03X-*,03,PFTCPROG,(PF03X-PF03)/KEYLNQ,0)                    
         DC   CL3' '                                                            
         DC   CL8'CONLIST'                                                      
         DC   CL8'DISPLAY'                                                      
PF03     DC   AL1(KEYTYTWA,L'LINSTA-1),AL2(TWASVSTA-T230FFD)                    
         DC   AL1(KEYTYTWA,L'LINBDAT+2),AL2(TWASVDAT-T230FFD)                   
*        DC   AL1(KEYTYTWA,L'LINSEQ-1),AL2(TWASVSEQ-T230FFD)                    
         DC   AL1(KEYTYTWA,L'TWASVSEQ-2),AL2(TWASVSEQ-T230FFD)                  
PF03X    EQU  *                                                                 
*                                                                               
*        EASI INVOICE CHANGE SCREEN                                             
         DC   AL1(PF04X-*,04,PFTCPROG,(PF04X-PF04)/KEYLNQ,0)                    
         DC   CL3' '                                                            
         DC   CL8'CONLIST'                                                      
         DC   CL8'CHANGE'                                                       
PF04     DC   AL1(KEYTYTWA,L'LINSTA-1),AL2(TWASVSTA-T230FFD)                    
         DC   AL1(KEYTYTWA,L'LINBDAT+2),AL2(TWASVDAT-T230FFD)                   
*        DC   AL1(KEYTYTWA,L'LINSEQ-1),AL2(TWASVSEQ-T230FFD)                    
         DC   AL1(KEYTYTWA,L'TWASVSEQ-2),AL2(TWASVSEQ-T230FFD)                  
PF04X    EQU  *                                                                 
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
* FIND THE LAST >X'40' CHARACTER                                                
* R3 - ADDRESS OF THE TEXT STRING                                               
* R3 HIGH ORDER BYTE - LENGTH OF THE TEXT STRING                                
* ON EXIT - R3 POINTS PAST THE LAST NON-SPACE CHARACTER IN THE STRING           
* UNEQUAL CONDITION, IF STRING CONSISTS OF SPACES. EQUAL OTHERWISE              
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
LASTCHAR DS    0H                                                               
         LR    RF,R3                                                            
         SRL   RF,24               MAX NUMBER OF CHARS TO BACK UP THRU          
         AR    R3,RF               1 BYTE PAST END OF STRING                    
*                                                                               
LAST10   BCTR  RF,0                DECREMENT COUNTER                            
         CHI   RF,0                                                             
         JL    LASTNEQ             ZERO - STRING CONSISTS OF SPACES             
         BCTR  R3,0                BACK UP ONE CHARACTER                        
         CLI   0(R3),C' '                                                       
         JNH   LAST10              NONZERO, NON-SPACE CHARACTER                 
         J     LASTEQ                                                           
*                                                                               
LASTEQ   AHI   R3,1                ADVANCE TO NEXT CHARACTER                    
         CR    RB,RB                                                            
         BR    RE                                                               
LASTNEQ  LTR   RB,RB                                                            
         BR    RE                                                               
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *                 
* BYTE EXPECTED TO HAVE THE NEW MOS (X'BA' = OCT 2011)                          
* ON EXIT HALF WILL HAVE THE OLD FORMAT MOS (X'1110')                           
* DUB IS USED FOR CONVERSION                                                    
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *                 
NEW2OLD  NTR1  BASE=*,LABEL=*                                                   
         ZIC   RF,BYTE                                                          
         NILL  GRF,X'000F'         ZERO OUT YEAR (RF, GRAND REGISTER)           
         CVD   RF,DUB                                                           
         SR    RF,RF                                                            
         ICM   RF,3,DUB+6                                                       
         SRL   RF,4                                                             
         STC   RF,HALF+1                                                        
*                                                                               
         ZIC   RF,BYTE                                                          
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
*                                                                               
***********************************************************************         
* INITIALIZE EZBLOCK                                                            
***********************************************************************         
INITEZB  NTR1  BASE=*,LABEL=*                                                   
         XCEFL EZBLOCKD,'EZBLOCKL'                                              
*                                                                               
         MVC   EZWKRFIL,EASIWK                                                  
         LA    R1,WRKFBUFR                                                      
         ST    R1,EZWKRBUF                                                      
         LA    RE,WRKFREC                                                       
         ST    RE,EZWKRREC                                                      
         MVC   EZAREC,AIO1                                                      
         MVC   EZCOMFCS,ACOMFACS                                                
         MVI   EZLOOKSW,X'E2'                                                   
         MVI   EZTEST,C'Y'                                                      
         MVI   EZWRITE,C'N'                                                     
         TM    FTRFLAG,FTRTRACE                                                 
         BZ    *+8                                                              
         OI    EZTRACE,X'F0'                                                    
         MVC   EZPRINT,VPRINT                                                   
         MVC   EZWRKIO,VWRKIO                                                   
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
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
***********************************************************************         
* INITIALIZE WRKIO BLOCK                                                        
***********************************************************************         
INITWKB  NTR1  BASE=*,LABEL=*                                                   
         MVI   EZWRKIOF,C'Y'                                                    
*                                                                               
         LAY   R4,EZWRKIOB                                                      
         USING WRKIOD,R4                                                        
*                                                                               
         MVC   WRKEZUID,TWAORIG                                                 
         MVC   WRKIACOM,ACOMFACS                                                
         LA    RF,WRKFBUFR                                                      
         ST    RF,WRKIABUF                                                      
         LA    RF,WRKFREC                                                       
         ST    RF,WRKIAREC                                                      
         MVI   WRKIFTYP,WRKIFTEZ                                                
*                                                                               
         DROP  R4                  WRKIOD,R4                                    
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
         DROP  R6                  EZBLOCKD,R6                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* VALIDATE FILTERS (IF ANY)                                           *         
***********************************************************************         
*                                                                               
         DS    0H                                                               
VFTR     NTR1  BASE=*,LABEL=*                                                   
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
VFTR02   ZIC   R1,5(R2)                                                         
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
VFTR10   ZIC   R1,0(R4)            GET LENGTH                                   
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
         BNE   VFTR22                                                           
         CLI   1(R4),2                                                          
         BL    CCLENER                                                          
         CLI   1(R4),3                                                          
         BH    CCLENER                                                          
         MVC   FTRQCLT,22(R4)                                                   
         OC    FTRQCLT,SPACES                                                   
         B     VFTR90                                                           
*                                                                               
VFTR22   EX    R1,VFTRCLCC         CLIENT NAME (CN)                             
         BNE   VFTR24                                                           
         MVC   FTRCLTN,22(R4)                                                   
         ZIC   R0,1(R4)                                                         
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
         MVC   FTRQPRDL,1(R4)                                                   
         B     VFTR90                                                           
*                                                                               
VFTR26   EX    R1,VFTRCLCE         PRODUCT NAME (PN)                            
         BNE   VFTR30                                                           
         MVC   FTRPRDN,22(R4)                                                   
         ZIC   R0,1(R4)                                                         
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
         ZIC   R0,1(R4)                                                         
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
         ZIC   RE,1(R4)                                                         
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
         GOTO1 DATCON,(R1),(0,WORK),(6,FTRMOS)                                  
         GOTO1 (RF),(R1),(0,WORK),(X'20',WORK+6)                                
*                                                                               
         PACK  DUB,WORK+6(4)                                                    
         SR    R0,R0                                                            
         ICM   R0,7,DUB+5                                                       
         SRL   R0,4                                                             
         STCM  R0,3,FTRBMOS                                                     
         B     VFTR92                                                           
*                                                                               
VFTR90   DS   0H                                                                
         TM    WHEN,X'20'          ALLOWED IF SOON                              
         BO    VFTR92                                                           
*        OC    RQSTA,RQSTA         WAS STATION ENTERED                          
*        BNZ   VFTR92                                                           
*                                                                               
*        OC    RQBSEQ,RQBSEQ       WAS SEQUENCE # ENTERED                       
*        BZ    MISENTER                                                         
*                                                                               
VFTR92   ZIC   RE,BYTE             UP FIELD NUMBER CTR                          
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
*                                                                               
*                                                                               
VFNEXTX  LTR   RE,RE                                                            
         BR    RE                                                               
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
         LA    R2,LUPSTAH          STATION                                      
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
         ZIC   RF,0(R1)                                                         
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
         DROP  RB                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * *                                                     
* INCLUDES COME HERE                                                            
* * * * * * * * * * * * * *                                                     
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
       ++INCLUDE SPEZFFFD                                                       
*                                                                               
         ORG   CONTAGH                                                          
* SPEZFF8D                                                                      
       ++INCLUDE SPEZFF8D                                                       
*                                                                               
* SPEZFE1D                                                                      
         ORG   CONTAGH                                                          
T230E1D  DS    0X                                                               
       ++INCLUDE SPEZFE1D                                                       
*                                                                               
         DS    XL100                                                            
*                                                                               
TWASVSTA DS    XL8                                                              
TWASVDAT DS    XL10                                                             
TWASVSEQ DS    XL6                                                              
VPARSNIP DS    V                                                                
VWRKIO   DS    V                                                                
NUMCOMP  DS    X                                                                
SAVEBYTE DS    X                                                                
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
* CTGENFILE                                                                     
       ++INCLUDE CTGENFILE                                                      
*                                                                               
*SPEZFWORKD                                                                     
       ++INCLUDE SPEZFWORKD                                                     
*                                                                               
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAXTRAINF                                                      
       ++INCLUDE DDPARSNIPD                                                     
*                                                                               
*                                                                               
* DSECT FOR THIS PROGRAM *                                                      
*                                                                               
       ++INCLUDE SPEZFSYSD                                                      
*                                                                               
       ++INCLUDE SPEZDSCTS                                                      
*                                                                               
WRKIOD   DSECT                                                                  
       ++INCLUDE DDWRKIOD                                                       
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR        4                                                  
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
*                                                                               
*                                                                               
LIND     DSECT                 DEFINITION OF LIST LINE                          
*                                                                               
LINFRSTH DS    CL8                                                              
LINFRST  DS    CL31            FIRST FIELD - NON UPDATIVE                       
*                                                                               
LINCLTH  DS    CL8                                                              
LINCLT   DS    CL25            UPDATIVE FIELD - CLT, PRD, EST                   
*                                                                               
LINSCNDH DS    CL8                                                              
LINSCND  DS    CL20            SECOND NON-UPDATIVE FIELD                        
*                                                                               
         ORG   LINFRST                                                          
LINBDAT  DS    CL5                                                              
         DS    C                                                                
LINCDAT  DS    CL5                                                              
         DS    C                                                                
LINSRCE  DS    CL4                                                              
         DS    C                                                                
LINMON   DS    CL5                                                              
         DS    C                                                                
LINSTA   DS    CL8                                                              
*                                                                               
*                                                                               
         ORG   LINCLT+4                                                         
LINPRD   DS    CL3                                                              
         DS    C                                                                
LINPTR   DS    CL3                                                              
         DS    C                                                                
LINEST   DS    CL3                                                              
*                                                                               
*                                                                               
         ORG   LINCLT                                                           
LINCNAME DS    CL8                                                              
         DS    C                                                                
LINPNAME DS    CL12                                                             
         DS    C                                                                
LINESTNO DS    CL3                                                              
*                                                                               
*                                                                               
         ORG   LINSCND                                                          
LININV   DS    CL10                                                             
         DS    C                                                                
LINSPTS  DS    CL4                                                              
         DS    C                                                                
LINIMFLG DS    C                                                                
LINSTAT  DS    CL3                                                              
*                                                                               
LINNEXT  DS    0X                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'135SPEZF27   12/14/15'                                      
         END                                                                    
