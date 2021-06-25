*          DATA SET SPEZF15    AT LEVEL 079 AS OF 03/06/20                      
*PHASE T23015A                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE: T23015 - MOVE A BATCH FROM ONE AGENCY TO ANOTHER,           *         
*                  THEN DELETE FROM THE ORIGINAL AGENCY               *         
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
*                                                                     *         
* AIO USAGE - AIO1     - VALI RTNS IN CONTROLLER (SPEZF00-T23000)     *         
*                      - IN AIO FROM HYPER CONTROLLER (T00A30)        *         
*             AIO2     - WORKER INDEX                                 *         
*             WRKFBUFR - WORKER RECORD FOR UPDATE                     *         
*                                                                     *         
*                                                                     *         
* TSAR USES BLOCK AS AN I/O AREA                                      *         
*      0 - 1  LENGTH OF RECORD                                        *         
*      2 - 3  KEY - BINARY NUMBER 1 TO N                              *         
*      4 -    RECORD                                                  *         
*                                                                     *         
***********************************************************************         
*                                                                               
***********************************************************************         
*                                                                     *         
*  LEV  4    MAR14/94 FIX MOVE 1 INVOICE BUG - CKD FLAG 1ST           *         
*  LEV  5    MAR24/94 ADD MULTI INVOICE, SHOW REMAINING SEQ # IF ANY  *         
*  LEV  6    MAR31/94 TO MOVE ENTIRE BATCH, MUST ENTER ALL 1ST FIELD  *         
*  LEV  7    FEB08/95 ADD STATION EQUIVALENCY                         *         
*  LEV  8    FEB10/95 ADD CK ALL MEDIAS FOR NETWORK - N, C, S         *         
*  LEV  9    MAR06/96 ALLOW MOVES BETWEEN DF/TH/SF/LF                 *         
*  LEV 10    APR02/96 ALLOW MOVES BETWEEN BS/TH                       *         
*  LEV 11    MAY10/96 CK OLD USER TO BE THIS POWER CODE               *         
*  LEV 12    MAY13/96 CREATE NEW RECS TO KEEP TRACK OF BATCH MOVES    *         
*  LEV 13    MAY24/96 CONVERT TO WRKF FILES                           *         
*  LEV 14    AUG05/96 RIGHT JUSTIFY GROSS/NET BILLING FIELD IN ZM REC *         
*  LEV 15    AUG16/96 CHANGE DMGR COMMAND FROM KEEP TO DELETE         *         
*  LEV 16-17 NOV15/96 FIX BAD DATES AND LENGTH ZERO BUGS              *         
*  LEV 18    JUN04/97 FIX 5 DIGIT SEQ #                               *         
*  LEV 19    OCT21/97 ADD JW/FR TO ACROSS AGENCY OKAY TO TRANSFER     *         
*  LEV 20    APR03/98 ALLOW BS/TH/BT (BT IS NEW ) TRANSFER            *         
*  LEV 21    MAY06/98 FIX CANADIAN NET MOVES                          *         
*  LEV 22    MAY19/98 ALLOW BD & DM TRANSFER                          *         
*  LEV 23    JAN18/99 CHANGE WLAGEDD TO WLUDATA & PUT MOS IN IT       *         
*  LEV 24    FEB10/99 JUST CLEAR WLUDATA                              *         
*  LEV 25    FEB24/99 ALLOW MI & OU TRANSFER                          *         
*  LEV 26    JUL28/99 CK FOR STATION, IF NOT FOUND, CK EQUIV          *         
*  LEV 27    NOV24/99 ADD WI TO WR TO ACROSS AGENCY TRANSFER          *         
*  LEV 28    MAR24/00 ADD BN TO DM TO ACROSS AGENCY TRANSFER          *         
*  LEV 29    MAY16/00 ADD MI TO DN TO ACROSS AGENCY TRANSFER          *         
*  LEV 30    JUN16/00 FIX MAX INV TO 25                               *         
*  LEV 31    SEP15/00 ALLOW MOVES BETWEEN H7, JW, & OM                *         
*  LEV 32    DEC05/00 ALLOW MOVES BETWEEN OU & DN                     *         
*  LEV 33    APR06/01 FIX READING EQUIV STATIONS                      *         
*                     ALLOW MOVES BETWEEN TH & OH                     *         
*  LEV 34    SEP07/01 FIX READING EQUIV STATIONS                      *         
*  LEV 35    DEC07/01 FIX READING EQUIV STATIONS                      *         
*  LEV 36    MAY06/02 ALLOW MOVES BETWEEN AG AND FM                   *         
*                                                                     *         
***********************************************************************         
         TITLE 'T23015 - MOVE BATCH FROM ONE AGENCY TO ANOTHER'                 
T23015   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**3015**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         ST    R2,RELO                                                          
         MVC   AIO,AIO1                                                         
         MVI   QMED,C'T'                                                        
         MVI   IOOPT,C'Y'          USER DOING ALL I/O                           
*                                                                               
         CLC   =A(WRKFEND-SYSD),LSYSD                                           
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,WRKFBUFR                                                      
         LAY   RF,WRKFBFLQ                                                      
         XCEFL                                                                  
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    INVAL                                                            
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    INVAL                                                            
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    INVAL                                                            
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    INVAL                                                            
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    INVAL                                                            
         CLI   MODE,RECADD         ADD RECORD                                   
         BE    INVAL                                                            
         CLI   MODE,RECDEL         DEL RECORD                                   
         BE    INVAL                                                            
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
*                                                                               
***********************************************************************         
*   VKEY - VALIDATE KEY                                               *         
***********************************************************************         
*                                                                               
VKEY     CLI   ACTNUM,15          MUST BE ACTION MOVE                           
         BNE   INVAL                                                            
*                                                                               
         LA    RE,WRKIOB                                                        
         LHI   RF,WRKIOBL                                                       
         XCEFL                                                                  
*                                                                               
         MVI   WRKIFTYP,WRKIFTEZ                                                
         MVC   WRKEZUID,TWAORIG                                                 
         MVC   WRKIACOM,ACOMFACS                                                
         MVC   WRKIAREC,AIO2                                                    
         LA    RF,WRKFBUFR                                                      
         STCM  RF,15,WRKIABUF                                                   
*                                                                               
         XC    SPOTCNT,SPOTCNT     INIT SPOT COUNT                              
*                                                                               
         XC    LINNSQT,LINNSQT                                                  
         XC    LINNSQ2,LINNSQ2                                                  
         OI    LINNSQTH+6,X'80'                                                 
         OI    LINNSQ2H+6,X'80'                                                 
*                                                                               
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         GOTO1 VALIMED             VALIMED SETS TO SPOT OR NET                  
*                                                                               
         LA    R2,LINOAGYH         OLD AGENCY                                   
         XC    LKAGYBLK,LKAGYBLK                                                
         LLC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LKAGYUID(0),8(R2)                                                
         GOTO1 VLKAGY,LKAGYBLK                                                  
         BNE   USERIDER                                                         
*                                                                               
         MVC   OUID,LKAGYUID                                                    
         MVC   OUIDNUM,LKAGYBID    BINARY USER ID (ORIGIN)                      
         MVC   OUIDPC,LKAGYAGY     POWER CODE                                   
*                                                                               
         LA    R2,LINNAGYH         OLD AGENCY                                   
         XC    LKAGYBLK,LKAGYBLK                                                
         LLC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LKAGYUID(0),8(R2)                                                
         GOTO1 VLKAGY,LKAGYBLK                                                  
         BNE   USERIDER                                                         
*                                                                               
         MVC   NUID,LKAGYUID                                                    
         MVC   NUIDNUM,LKAGYBID    BINARY USER ID (ORIGIN)                      
         MVC   NUIDPC,LKAGYAGY     POWER CODE                                   
*                                                                               
         CLC   OUIDPC,NUIDPC       SAME POWER CODE                              
         BE    VK160                                                            
*                                                                               
         CLI   TWAOFFC,C'*'        OKAY FOR DDS TERMS                           
         BE    VK160                                                            
*                                                                               
         CLC   OUIDPC,AGENCY       OLD POWER CODE THIS AGENCY                   
         BNE   NONDDSER                                                         
*                                                                               
         BRAS  RE,CKTAB            GO SEE IF TRANSFER IS LEGAL                  
         BNE   NONDDSER                                                         
*                                                                               
VK160    DS    0H                                                               
         LA    R2,LINSTAH          STATION                                      
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         GOTO1 VREADSTA            READ STATION FROM TWA                        
         BNE   INVNEWST                                                         
         MVC   OLDSTA,FLDRDSTA                                                  
*                                                                               
         XC    NEWSTA,NEWSTA                                                    
         LA    R2,LINNSTAH                                                      
         CLI   5(R2),0                                                          
         BE    VK168                                                            
*                                                                               
         GOTO1 VREADSTA            READ STATION FROM TWA                        
         BNE   INVNEWST                                                         
         MVC   NEWSTA,FLDRDSTA                                                  
*                                                                               
* PREVENT DIGITAL/NON-DIGITAL TRANSFERS                                         
*                                                                               
VK165    DS    0H                                                               
         MVI   BYTE,X'00'                                                       
*                                                                               
         LA    R1,OLDSTA+4         "FROM" STATION'S BAND                        
         ICM   R1,8,=AL1(EZMTBNDQ)                                              
         GOTO1 VGETMED                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    EZMTFLAG-EZMEDTBD(RF),EZMTFDGQ                                   
         BZ    *+8                                                              
         OI    BYTE,X'01'                                                       
*                                                                               
         LA    R1,NEWSTA+4         "TO" STATION'S BAND                          
         ICM   R1,8,=AL1(EZMTBNDQ)                                              
         GOTO1 VGETMED                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    EZMTFLAG-EZMEDTBD(RF),EZMTFDGQ                                   
         BZ    *+8                                                              
         OI    BYTE,X'02'                                                       
*                                                                               
         TM    BYTE,X'03'                                                       
         BM    INVNEWST            MIXED = DIGITAL AND NON-DIGITAL              
*                                                                               
VK168    DS    0H                                                               
         LA    R2,LININVH          INVOICE                                      
         LA    R3,RQINV                                                         
         USING RQINVD,R3                                                        
*                                                                               
         LA    RE,RQINV                                                         
         LHI   RF,RQINVCT*RQINVDLQ                                              
         XCEFL                                                                  
         MVI   INVSW,0                                                          
*                                                                               
VK170    DS    0H                                                               
         LA    RF,LINNSQTH                                                      
         CR    R2,RF                                                            
         BNL   VK190               ALL INVOICE FIELDS PROCESSED                 
*                                                                               
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VK180               NO, PROCEED TO NEXT FIELD                    
*                                                                               
         CLI   5(R2),3                                                          
         BNE   VK175                                                            
         CLC   =C'ALL',8(R2)       THIS AN ALL INVOICES REQUEST                 
         BNE   VK175                                                            
*                                                                               
         OI    INVSW,INVSWALL      INDICATE ALL IVNOICE REQUEST                 
         B     VK180               GO TO NEXT FIELD                             
*                                                                               
VK175    DS    0H                                                               
         MVC   RQINVNUM,8(R2)      COPY INVOICE NUMBER INTO TABLE               
         OC    RQINVNUM,SPACES     COPY INVOICE NUMBER INTO TABLE               
         OI    INVSW,INVSWSP       INDICATE SPECIFIC INVOICE REQUEST            
*                                                                               
VK180    DS    0H                  ADVANCE TO NEXT FIELD                        
         LA    R3,RQINVDLQ(R3)     NEXT SLOT IN RQINV                           
         LLC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         B     VK170                                                            
*                                                                               
         DROP  R3                                                               
*                                                                               
* ALL INVOICE FIELDS PROCESSED                                                  
*                                                                               
VK190    DS    0H                                                               
         LA    R2,LININVH                                                       
*                                                                               
         TM    INVSW,INVSWSP+INVSWALL                                           
         BZ    MISSERRA                                                         
*                                                                               
         TM    INVSW,INVSWSP+INVSWALL                                           
         BO    BADALLER                                                         
*                                                                               
VK200    LA    R2,LINBDTEH         BATCH DATE                                   
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
*                                                                               
         GOTO1 DATVAL,DMCB,(0,FHDRLEN(R2)),WORK                                 
         OC    DMCB,DMCB                                                        
         BZ    BADATE                                                           
         GOTO1 DATCON,(R1),(0,WORK),(2,RQDTE)                                   
*                                                                               
         LA    R2,LINBSEQH         SEQ                                          
*                                                                               
         CLI   5(R2),0             MUST BE AN ENTRY                             
         BE    MISSERR                                                          
*                                                                               
         MVC   WORK(8),=8C'0'                                                   
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,VKMVN                                                         
         EX    R1,VKCLC                                                         
         BNE   NUMERR                                                           
         EX    R1,VKPK                                                          
         OI    DUB+7,X'0F'                                                      
         CVB   R0,DUB                                                           
         UNPK  RQSEQ,DUB                                                        
*                                                                               
         STCM  R0,15,RQBSEQ                                                     
         B     VK300                                                            
*                                                                               
VKMVN    MVN   WORK(0),8(R2)                                                    
VKCLC    CLC   WORK(0),8(R2)                                                    
VKPK     PACK  DUB,8(0,R2)                                                      
*                                                                               
*                                  FILTERS LINE                                 
VK300    DS   0H                                                                
         BAS   RE,TSTRO                                                         
*                                  FILTERS LINE                                 
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A2C'        DDWRKIO                            
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   EZWRKIO,0(R1)                                                    
*                                  FILTERS LINE                                 
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A2E'        EZPARSE                            
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   EZPARSE,0(R1)                                                    
*                                                                               
         MVC   WRKEZUID,OUIDNUM  BINARY USER ID (ORIGIN)                        
         MVI   WRKIACTN,WRKIANDX                                                
         MVC   WRKEZSQN,RQBSEQ                                                  
         OI    WRKINDS,WRKISEQQ                                                 
*                                                                               
         GOTO1 EZWRKIO,WRKIOB                                                   
         CLI   WRKIERRS,0                                                       
         BNE   NOBATFND                                                         
*                                                                               
         TM    WRKEZSTA,W_STDL   X'04' BATCH DELETED?                           
         BO    NOBATFND                                                         
*                                                                               
         CLC   WRKEZBDT,RQDTE    RETURNED BATCH DATE SAME AS REQUESTED?         
         BNE   BATDTERR                                                         
*                                                                               
         MVC   SRCESTA(L'SRCESTA-1),WRKEZSCL   CALL LETERS                      
         MVC   SRCESTA+4(1),WRKEZMED   MEDIA                                    
         CLI   SRCESTA+3,C'.'                                                   
         BNE   *+8                                                              
         MVI   SRCESTA+3,C' '                                                   
         GOTO1 VEQVSTA                                                          
*                                                                               
         CLI   EQUISTA+3,C'.'                                                   
         BNE   *+8                                                              
         MVI   EQUISTA+3,C' '                                                   
*                                                                               
         CLC   OLDSTA,EQUISTA5  RETURNED EQV STATION SAME AS REQUESTED?         
         BNE   NOBATFND                                                         
*                                                                               
         TM    WRKEZSTA,X'08' THIS BATCH IN KEEP STATUS W_STKE                  
         BO    CVTDELER                                                         
*                                                                               
* REQUESTED BATCH INDEX LOCATED SUCCESSFULLY                                    
*                                                                               
         MVC   SVEZKEY,WRKEZKEY                                                 
         MVC   SVEZUDT,WRKEZUDT    1-BYTE MOS                                   
         MVC   SVEZMOS,WRKEZMOS    2-BYTE MOS                                   
*                                                                               
         XC    SVEZDSC,SVEZDSC                                                  
         MVC   SVWKAGED,WRKEZBDT                                                
         MVC   SVWKAGET,WRKEZBTM                                                
*                                                                               
         LAY   RE,TSARBLK                                                       
         LHI   RF,TSPXTNL                                                       
         XCEFL                                                                  
*                                                                               
         LAY   R2,TSARBLK                                                       
         USING TSARD,R2                                                         
*                                                                               
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSKEYL,2                                                         
         OI    TSINDS,TSINODSK+TSIALLOC                                         
         OI    TSIND2,TSI2BIGN                                                  
         MVC   TSRECL,=H'372'                                                   
         OI    TSRECI,TSRVAR+TSRTSARB+TSRXTN                                    
         MVI   TSPAGN,28        NUMBER OF TEMPSTR PAGES TO BE USED              
*                                                                               
* TSAR CALLOV HERE                                                              
*                                                                               
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A5D')                                 
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ATSAR                                                         
*                                                                               
         MVI   TSACTN,TSAINI       SET 1ST TSAR FOR INIT                        
         GOTO1 ATSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TSAREC,AIO2         SAVING FROM AIO2 DIRECTLY                    
         LA    R3,1                RECORD COUNTER                               
*                                                                               
STR10    DS    0H                                                               
         MVI   WRKIACTN,WRKIAGET                                                
         GOTO1 EZWRKIO,WRKIOB                                                   
         CLI   WRKIERRS,WRKIEEOF                                                
         BE    STR30                                                            
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 EZPARSE,DMCB,AIO2,('FTMAXENT',FLDTAB)                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    SVEZDSC,SVEZDSC                                                  
         BNZ   *+10                                                             
         MVC   SVEZDSC,WRKEZDSC                                                 
*                                                                               
         L     R4,AIO2                                                          
*                                                                               
         CLC   =C'31',4(R4)        THIS AN INVOICE HEADER                       
         BNE   STR20               NO, JUST WRITE TO TSAR                       
*                                                                               
         TM    INVSW,INVSWSP       SPECIFIC INVOICE REQUESTED?                  
         BZ    STR16               NO, ENTIRE BATCH - DON'T CHECK RQINV         
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 =A(GETDATA),DMCB,10,WORK,RR=RELO                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    WORK(10),SPACES                                                  
         LA    R1,WORK                                                          
         BRAS  RE,CKINV            INVOICE IN RQINV?                            
         BNE   STR20                                                            
*                                                                               
         L     RF,FULL                                                          
         OI    RQINVFLG-RQINVD(RF),RQFFOUNQ   INDICATE INVOICE FOUND            
*                                                                               
STR16    TM    7(R4),X'58'         IS CONVERT/OVERRIDE/DELETE ON                
         BNZ   CVTDELER                                                         
*                                                                               
STR20    DS    0H                                                               
         STCM  R3,3,2(R4)          SAVE KEY (RECORD COUNT)                      
         AHI   R3,1                                                             
*                                                                               
         MVI   TSACTN,TSAADD                                                    
         GOTO1 ATSAR,TSARD                                                      
         CLI   TSERRS,TSEEOF                                                    
         BE    SIZERR                                                           
         CLI   TSERRS,0                                                         
         BE    STR10               READ NEXT RECORD                             
*                                                                               
         DC    H'0'                                                             
*                                                                               
*                                                                               
* EOF ON WORKER FILE READ                                                       
* SEE IF ALL REQUESTED INVOICES WERE FOUND *                                    
*                                                                               
STR30    TM    INVSW,INVSWSP       WAS SPECIFIC INVOICE REQUESTED               
         BZ    STR34                NO                                          
*                                                                               
         OI    INVSW,INVSWFND                                                   
*                                                                               
         LA    R0,RQINVCT                                                       
         LA    R1,RQINV                                                         
         LA    R2,LININVH                                                       
         USING RQINVD,R1                                                        
*                                                                               
STR32    OC    RQINVNUM,RQINVNUM   THIS AN INVOICE                              
         BZ    STR33                                                            
         TM    RQINVFLG,RQFFOUNQ   WAS THIS INVOICE FOUND                       
         BO    STR33                YES                                         
         DROP  R1                                                               
*                                                                               
         NI    INVSW,X'FF'-INVSWFND                                             
*                                                                               
         LA    R3,17(,R2)                                                       
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         MVC   1(2,R3),=C'**'                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
STR33    LA    R1,RQINVDLQ(R1)                                                  
         LLC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R0,STR32                                                         
*                                                                               
         TM    INVSW,INVSWFND                                                   
         BZ    INVFNDER                                                         
*                                                                               
* BUILD NEW WORKER FILE ON NEW UID *                                            
*                                                                               
STR34    DS    0H                                                               
         XC    WRKEZSQN,WRKEZSQN                                                
         MVC   WRKEZUID,NUIDNUM                                                 
         OC    NEWSTA,NEWSTA             NEW STATION FIELD USED?                
         BZ    *+16                      NO - DO NOT OVERRIDE                   
         MVC   WRKEZSCL,NEWSTA           MOVE IN STATION                        
         MVC   WRKEZMED,NEWSTA+4         MOVE IN BAND                           
*                                                                               
         MVC   WRKEZDSC,SVEZDSC    ORIGINAL DESCRIPTION                         
         MVC   WRKEZBDT,SVWKAGED                                                
         MVC   WRKEZBTM,SVWKAGET                                                
*                                                                               
         MVC   SVEZKSTA(4),WRKEZSCL      SAVE STATION                           
         MVC   SVEZKSTA+4(1),WRKEZMED    AND MEDIA                              
*                                                                               
         MVI   WRKIACTN,WRKIANEW                                                
         OI    WRKINDS,WRKICOPQ                                                 
         GOTO1 EZWRKIO,WRKIOB                                                   
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         EDIT  WRKEZSQN,(6,LINNSQ1),ALIGN=LEFT,COMMAS=YES                       
*                                                                               
         LH    R2,=AL2(TSARBLK-SYSD)  GET ADDR OF TSAR TABLE                    
         AR    R2,R9                                                            
         USING TSARD,R2                                                         
*                                                                               
         L     R3,AIO2                                                          
*                                                                               
         L     RE,AIO2                                                          
         LHI   RF,AIODLQ                                                        
         XCEFL                                                                  
*                                                                               
         NI    INVSW,X'FF'-INVSWWR    TURN OFF "NEW BATCH WRITTEN" FLAG         
*                                                                               
STR39    NI    INVSW,X'FF'-INVSWEQ-INVSWNE-INVSWOT                              
         NI    INVSW,X'FF'-INVSWEND                                             
*                                                                               
         XC    SVRECTYP,SVRECTYP                                                
*                                                                               
* READ FROM TSAR TO BUILD NEW BATCH                                             
* IF INVOICE ONLY, 1ST WRITE OUT BATCH OF 1 INVOICE,                            
* THEN WRITE NEW BATCH WITHOUT THAT INVOICE,                                    
* THEN DELETE ORIGINAL BATCH                                                    
*                                                                               
         MVC   2(2,R3),=X'0001'      FIRST RECORD                               
*                                                                               
         MVI   TSACTN,TSARDH                                                    
         GOTO1 ATSAR,TSARD                                                      
         B     STR42                                                            
*                                                                               
STR40    DS    0H                                                               
         MVC   SVRECTYP,4(R3)                                                   
*                                                                               
         MVI   TSACTN,TSANXT                                                    
         GOTO1 ATSAR,TSARD                                                      
*                                                                               
STR42    DS    0H                                                               
         TM    TSERRS,X'80'        END OF FILE                                  
         BO    STR50                                                            
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 EZPARSE,DMCB,AIO2,('FTMAXENT',FLDTAB)                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    2(2,R3),2(R3)       XC RECORD NUMBER                             
         LA    R1,7(R3)            POINT TO THE ACTUAL DATA                     
*                                                                               
         BRAS  RE,ISEND            END OF CURRENT INVOICE?                      
         BNE   STR43                                                            
*                                                                               
         TM    INVSW,INVSWWR       HAS NEW BATCH BEEN WRITTEN?                  
         BO    STR42A                                                           
*                                                                               
         TM    INVSW,INVSWSP       SPECIFIC INVOICE REQUEST?                    
         BZ    *+12                NO, MOVING ALL IVNOICES                      
         TM    INVSW,INVSWEQ       REQUESTED INVOICE?                           
         BZ    STR42A                                                           
*                                                                               
         BRAS  RE,BLDEZMOV         CREATE MOV REC FOR PREV INV                  
*                                                                               
STR42A   DS    0H                                                               
         NI    INVSW,X'FF'-INVSWEQ-INVSWNE                                      
         XC    SPOTCNT,SPOTCNT     INIT SPOT COUNT                              
*                                                                               
STR43    DS    0H                                                               
         BRAS  RE,PROCREC          SAVE IMPORTANT DATA FROM RECORDS             
*                                                                               
* FOR ALL-INVOICE REQUESTS, FLAG EVERY INVOICE IN BATCH                         
* AS "REQUESTED INVOICE FOUND", SO BATCH MOVE RECORDS CAN BE BUILT              
* IF THE INVOICE IS NEITHER "REQUESTED" NOR "NOT REQUESTED"                     
* THEN MOVE RECORDS ARE NOT CREATED                                             
*                                                                               
         CLC   =C'31',4(R3)        THIS AN INVOICE HEADER                       
         BNE   STR43A                                                           
         TM    INVSW,INVSWSP       SPECIFIC INVOICE REQUEST?                    
         BO    STR43A              YES                                          
         OI    INVSW,INVSWEQ                                                    
*                                                                               
STR43A   DS    0H                                                               
         TM    INVSW,INVSWSP       SPECIFIC INVOICE REQUEST?                    
         BZ    STR46               NO, WRITE ALL RECORDS TO NEW BATCH           
*                                                                               
* PROCESSING SPECIFIC INVOICE BATCH MOVE HERE...                                
*                                                                               
         BRAS  RE,SAVEREC          BUFFER RECORDS, IF NECESSARY                 
         BE    STR40               NO, GET NEXT RECORD                          
*                                                                               
         CLC   =C'31',4(R3)        THIS AN INVOICE HEADER                       
         BNE   STR44                                                            
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 =A(GETDATA),DMCB,10,WORK,,RR=RELO                                
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    WORK(10),SPACES                                                  
*                                                                               
         LA    R1,WORK                                                          
         BRAS  RE,CKINV            INVOICE IN RQINV?                            
         BNE   *+12                                                             
         OI    INVSW,INVSWEQ        REQUESTED INVOICE FOUND                     
         B     *+8                                                              
         OI    INVSW,INVSWNE+INVSWOT OTHER IVNOICES FOUND                       
*                                                                               
         BRAS  RE,TOPROC           PROCESSING THIS IVNOICE?                     
         BNE   STR40               NO, GET NEXT RECORD                          
*                                                                               
         BRAS  RE,WRTREC           FLUSH THE SAVED RECORDS' BUFFER              
         B     STR46               ADD CURRENT RECORD TO THE BATCH              
*                                                                               
STR44    DS    0H                                                               
         BRAS  RE,TOPROC           PROCESSING THIS IVNOICE?                     
         BNE   STR40               NO, GET NEXT RECORD                          
*                                                                               
         CLC   =C'34',4(R3)        THIS AN INVOICE TOTAL                        
         BE    STR46                                                            
         CLC   =C'32',4(R3)        THIS AN INVOICE COMMENT                      
         BE    STR46                                                            
         CLC   =C'33',4(R3)        THIS AN INVOICE COMMENT (BOTTOM)             
         BE    STR46                                                            
         CLC   =C'41',4(R3)        THIS A SCHEDULE LINE                         
         BE    STR46                                                            
         CLC   =C'52',4(R3)        THIS A BROADCAST DETAIL                      
         BE    STR46                                                            
         CLC   =C'42',4(R3)        THIS A SCHEDULE COMMENT                      
         BE    STR46                                                            
         CLC   =C'51',4(R3)        THIS A BROADCAST DETAIL                      
         BE    STR46                                                            
*                                                                               
STR45    DS    0H                                                               
         DC    H'0'                UNKNOWN RECORD                               
*                                                                               
* ADD RECORD TO WORKER FILE                                                     
*                                                                               
STR46    DS    0H                                                               
         MVI   WRKIACTN,WRKIAADD                                                
         GOTO1 EZWRKIO,WRKIOB                                                   
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     STR40               NEXT RECORD FROM TSAR                        
*                                                                               
* ALL TSAR RECORDS PROCESSED                                                    
*                                                                               
STR50    DS    0H                                                               
         MVI   WRKIACTN,WRKIACLO                                                
         GOTO1 EZWRKIO,WRKIOB                                                   
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* WRITE ORIGINAL DESCRIPTION                                                    
*                                                                               
         XC    WRKEZDSC,WRKEZDSC                                                
         MVC   WRKEZDSC,SVEZDSC                                                 
*                                                                               
         MVI   WRKIACTN,WRKIADSC                                                
         GOTO1 EZWRKIO,WRKIOB                                                   
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    INVSW,INVSWWR       HAS NEW BATCH BEEN WRITTEN?                  
         BO    STR55                                                            
*                                                                               
         TM    INVSW,INVSWSP       SPECIFIC INVOICE REQUEST?                    
         BZ    *+12                NO, MOVING ALL IVNOICES                      
         TM    INVSW,INVSWEQ       REQUESTED INVOICE?                           
         BZ    STR55                                                            
*                                                                               
         BRAS  RE,BLDEZMOV         MOVE RECORD FOR PREV INVOICE                 
*                                                                               
STR55    DS    0H                                                               
         OI    LINNSQ1H+6,X'80'                                                 
*                                                                               
         TM    INVSW,INVSWSP       WAS SPECIFIC INVOICE REQUESTED               
         BZ    STR60               NO - DISPLAY INFORMATION AND EXIT            
         TM    INVSW,INVSWOT       WERE OTHER INVOICES FOUND                    
         BZ    STR60               NO                                           
         TM    INVSW,INVSWWR       WAS REQUESTED INV NEW BATCH WRITTEN          
         BO    STR60               YES                                          
*                                                                               
         OI    INVSW,INVSWWR       REQUESTED INVOICE NEW BATCH WRITTEN          
*                                                                               
         MVC   WRKEZKEY,SVEZKEY    ORIGINAL BATCH INDEX                         
         XC    WRKEZSQN,WRKEZSQN                                                
         MVC   WRKEZDSC,SVEZDSC    ORIGINAL DESCRIPTION                         
         MVC   WRKEZUDT,SVEZUDT    ORIGINAL USER DATA (MOS, 1-BYTE)             
         MVC   WRKEZMOS,SVEZMOS    2-BYTE MOS                                   
         MVC   WRKEZBDT,SVWKAGED                                                
         MVC   WRKEZBTM,SVWKAGET                                                
*                                                                               
         MVI   WRKIACTN,WRKIANEW                                                
         OI    WRKINDS,WRKICOPQ                                                 
         GOTO1 EZWRKIO,WRKIOB                                                   
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         EDIT  WRKEZSQN,(6,LINNSQ2),ALIGN=LEFT,COMMAS=YES                       
*                                                                               
         MVC   LINNSQT(19),=C'REMAINING BATCH SEQ'                              
         OI    LINNSQTH+6,X'80'                                                 
         OI    LINNSQ2H+6,X'80'                                                 
*                                                                               
         B     STR39                                                            
*                                                                               
* NEW, REMAINING BATCHES CREATED                                                
*                                                                               
STR60    DS    0H                                                               
         MVC   WRKEZKEY,SVEZKEY    ORIGINAL BATCH INDEX                         
         MVI   WRKIACTN,WRKIANDX                                                
         MVC   WRKEZSQN,RQBSEQ                                                  
         OI    WRKINDS,WRKISEQQ                                                 
         GOTO1 EZWRKIO,WRKIOB                                                   
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   WRKIACTN,WRKIADEL                                                
         MVC   WRKEZSQN,RQBSEQ                                                  
         OI    WRKINDS,WRKISEQQ                                                 
         GOTO1 EZWRKIO,WRKIOB                                                   
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   WRKIACTN,WRKIARET                                                
         MVC   WRKEZINF,=X'0030'   48 HOURS                                     
         GOTO1 EZWRKIO,WRKIOB                                                   
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     MVEOKMSG                                                         
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
WKRFULER L     R1,=A(WKRFULMS)                                                  
         B     ERREXIT                                                          
*                                                                               
SIZERR   L     R1,=A(SIZERMS)                                                   
         B     ERREXIT                                                          
*                                                                               
NOBATFND L     R1,=A(NOBATFMS)                                                  
         B     ERREXIT                                                          
*                                                                               
NONDDSER L     R1,=A(NONDDSMS)                                                  
         LA    R2,CONRECH                                                       
         B     ERREXIT                                                          
*                                                                               
UIDVRERR L     R1,=A(UIDVRMS)                                                   
         B     ERREXIT                                                          
*                                                                               
MVEOKMSG XC    CONHEAD,CONHEAD                                                  
         LA    R2,LINSTAH                                                       
         TM    INVSW,INVSWSP                                                    
         BO    *+14                                                             
         MVC   CONHEAD(L'MVEOKMS),MVEOKMS                                       
         B     ERREXITA                                                         
*                                                                               
         MVC   CONHEAD(L'MVEIOKMS),MVEIOKMS                                     
         B     ERREXITA                                                         
*                                                                               
USERIDER L     R1,=A(INVUIDMS)                                                  
         B     ERREXIT                                                          
*                                                                               
BADALLER L     R1,=A(BADALLMS)                                                  
         B     ERREXIT                                                          
*                                                                               
CVTDELER L     R1,=A(CVTDELMS)                                                  
         LA    R2,LINBSEQH                                                      
         B     ERREXIT                                                          
*                                                                               
BATDTERR L     R1,=A(BATDTEMS)                                                  
         LA    R2,LINBDTEH                                                      
         B     ERREXIT                                                          
*                                                                               
SAMUSRER L     R1,=A(SAMUSRMS)                                                  
         B     ERREXIT                                                          
*                                                                               
*                                                                               
INVFNDER L     R1,=A(INVFNDMS)                                                  
         B     ERREXIT                                                          
*                                                                               
INVNEWST L     R1,=A(INVNEWMS)                                                  
         B     ERREXIT                                                          
*                                                                               
NOUPDER  L     R1,=A(NOCHANGE)                                                  
         B     ERREXIT                                                          
*                                                                               
USIDLNER L     R1,=A(UIDLENMS)                                                  
*                                                                               
ERREXIT  XC    CONHEAD,CONHEAD                                                  
         A     R1,RELO                                                          
         BCTR  R1,0                                                             
         CLI   0(R1),L'CONHEAD-1-10                                             
         BNH   *+6                                                              
         DC    H'0'                                                             
         ZIC   RF,0(R1)                                                         
         EX    RF,ERREXITM                                                      
         MVC   CONHEAD(9),=C'* ERROR *'                                         
*                                                                               
ERREXITA GOTO1 ERREX2                                                           
ERREXITM MVC   CONHEAD+10(0),1(R1)                                              
*                                                                               
MISSERRA LA    R2,LININVH          INVOICE                                      
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
BADATE   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
INVAL    MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         DC    AL1(L'SIZERMS-1)                                                 
SIZERMS  DC    C'* NOTE * BATCH TOO LARGE TO MOVE INVOICE *'                    
         DC    AL1(L'MVEOKMS-1)                                                 
MVEOKMS  DC    C'* NOTE * BATCH MOVED TO NEW ID, OLD BATCH DELETED *'           
         DC    AL1(L'MVEIOKMS-1)                                                
MVEIOKMS DC    C'* NOTE * INVOICE MOVED TO NEW ID, ENTER TO RESUME *'           
         DC    AL1(L'SAMUSRMS-1)                                                
SAMUSRMS DC    C'CAN''T MOVE BATCH TO SAME USER *'                              
         DC    AL1(L'CVTDELMS-1)                                                
CVTDELMS DC    C'CAN''T MOVE CONVERTED/DELETED/OVERRIDDEN *'                    
         DC    AL1(L'NOBATFMS-1)                                                
NOBATFMS DC    C'BATCH NOT FOUND *'                                             
         DC    AL1(L'BATDTEMS-1)                                                
BATDTEMS DC    C'BATCH NOT FOUND FOR THIS DATE *'                               
         DC    AL1(L'INVUIDMS-1)                                                
INVUIDMS DC    C'INVALID USER ID *'                                             
         DC    AL1(L'BADALLMS-1)                                                
BADALLMS DC    C'IF ALL INVOICES, CAN''T HAVE OTHER ENTRIES *'                  
         DC    AL1(L'USIDLNMS-1)                                                
USIDLNMS DC    C'USER ID CAN''T BE MORE THAN 8 CHARACTERS *'                    
         DC    AL1(L'UIDVRMS-1)                                                 
UIDVRMS  DC    C'USER ID CAN''T DO CHANGE *'                                    
         DC    AL1(L'NONDDSMS-1)                                                
NONDDSMS DC    C'DDS ONLY FUNCTION *'                                           
         DC    AL1(L'UIDLENMS-1)                                                
UIDLENMS DC    C'USERID CAN''T BE MORE THAN 8 CHARACTERS *'                     
         DC    AL1(L'WKRFULMS-1)                                                
WKRFULMS DC    C'WORKER FILE FULL, BATCH NOT MOVED *'                           
         DC    AL1(L'INVFNDMS-1)                                                
INVFNDMS DC    C'REQUESTED INVOICE(S) NOT FOUND IN BATCH *'                     
         DC    AL1(L'INVNEWMS-1)                                                
INVNEWMS DC    C'INVALID STATION *'                                             
         DC    AL1(L'NOCHANGE-1)                                                
NOCHANGE DC    C'NO UPDATES ALLOWED *'                                          
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* SEE IF CROSS AGENCY MOVE IS LEGAL                                             
*                                                                               
CKTAB    NTR1  BASE=*,LABEL=*                                                   
         LA    R1,PCTABLE                                                       
*                                                                               
CKTAB10  LA    R0,OUIDPC           OLD POWER CODE                               
         BRAS  RE,CKINTAB                                                       
         BNE   CKTAB20                                                          
*                                                                               
         LA    R0,NUIDPC           NEW POWER CODE                               
         BRAS  RE,CKINTAB                                                       
         JE    EQXIT                                                            
*                                                                               
* ADVANCE TO NEXT LINE (PAST X'FF')                                             
CKTAB20  LA    R1,2(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   CKTAB20                                                          
*                                                                               
         LA    R1,1(R1)            ADVANCE PAST X'FF'                           
*                                                                               
         CLI   0(R1),X'FF'         EOT?                                         
         JE    NEQXIT                                                           
         B     CKTAB10                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
* AGENCY CODE IN TABLE?                                                         
CKINTAB  NTR1  BASE=*,LABEL=*                                                   
         LR    R2,R0               AGY POWER CODE                               
*                                                                               
CKINT10  CLC   0(2,R2),0(R1)       AGENCY CODE MATCHES?                         
         JE    EQXIT               YES                                          
         LA    R1,2(R1)            ADVANCE TO NEXT CODE                         
         CLI   0(R1),X'FF'         EOT?                                         
         JE    NEQXIT              YES - AGY NOT IN THIS TABLE                  
         B     CKINT10                                                          
         LTORG                                                                  
*                                                                               
*                                                                               
PCTABLE  DC    C'DFDTLFTH'                                                      
         DC    X'FF'                                                            
*                                                                               
         DC    C'BSBTTH'                                                        
         DC    X'FF'                                                            
*                                                                               
         DC    C'JWFR'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'BDDM'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'MIOU'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'WIWR'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'BNDM'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'MIDN'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'H7JWOM'                                                        
         DC    X'FF'                                                            
*                                                                               
         DC    C'OUDN'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'THOH'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'THSO'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'WWYN'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'AGFM'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'OOBDBNDN'                                                      
         DC    X'FF'                                                            
*                                                                               
         DC    C'H9DUGZ'                                                        
         DC    X'FF'                                                            
*                                                                               
         DC    C'H7FR'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'YNH7'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'YNFR'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'WIM1'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'M2H7FRYN'                                                      
         DC    X'FF'                                                            
*                                                                               
         DC    C'THPC'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'THTB'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'DFTB'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'DFPC'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'TRT$'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'TBPC'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'DFSO'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'YRHY'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'H0HY'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'O0TB'                                                          
         DC    X'FF'                                                            
*                                                                               
         DC    C'OOOU'             OMDTOA/OMDCAN SPEC-39341                     
         DC    X'FF'                                                            
*                                                                               
         DC    X'FF'               END OF TABLES                                
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* USAGE: GOTO1 GETFDISP,N   WHERE N=FIELD NUMBER                                
* OR                                                                            
* R1 EXPECTED TO HAVE FIELD NUMBER                                              
* ON EXIT R1 CONTAINS A(FIELD) IN RECORD                                        
*         RF CONTAINS FIELD LENGTH                                              
* ZERO CC SET IF FIELD NOT ON THIS RECORD                                       
***********************************************************************         
GETFDISP LR    RF,R1               FIELD NUMBER                                 
         BCTR  RF,0                DEC FOR INDEXING (FIELD 1 = INDEX 0)         
         MHI   RF,FTLENQ           TIMES TABLE LENGTH                           
         LA    RF,FLDTAB(RF)       INDEX INTO THE TABLE                         
         XR    R1,R1                                                            
         ICM   R1,3,FTFLDDSP-FLDTABD(RF) FLD DISPLACEMENT                       
         BZR   RE                  EXIT IF ZERO                                 
         A     R1,AIO2             FIELD ADDRESS                                
         LLC   RF,FTFLDLEN-FLDTABD(RF)                                          
         LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* GETDATA - COPIES INPUT DATA FROM FLDTAB                                       
* UNEQUAL CC EXIT, IF FIELD NOT ON THE RECORD                                   
***********************************************************************         
GETDATA  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            FIELD NUMBER                                 
         L     R3,4(R1)            DESTINATION ADDRESS                          
         LR    R1,R2               FIELD NUMBER                                 
         BRAS  RE,GETFDISP                                                      
         JZ    NEQXIT                                                           
         LTR   RF,RF                                                            
         JZ    EQXIT                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         J     EQXIT                                                            
         MVC   0(0,R3),0(R1)                                                    
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* SEE IF THIS IS REQUESTED INVOICE                                              
* R1 EXPECTED TO ADDRESS THE IVNOICE NUMBER                                     
* EQUAL CONDITION CODE, IF INVOICE IS IN RQINV                                  
* FULL CONTAINS ADDRESS OF INVOICE IN RQINV                                     
* UNEQUAL CC OTHERWISE                                                          
CKINV    NTR1  BASE=*,LABEL=*                                                   
         LA    R4,RQINV                                                         
         LA    R6,RQINVCT                                                       
         USING RQINVD,R4                                                        
*                                                                               
CKINV44  DS    0H                                                               
         CLC   RQINVNUM,0(R1)                                                   
         BNE   *+12                                                             
         ST    R4,FULL                                                          
         J     EQXIT                                                            
*                                                                               
         LA    R4,RQINVDLQ(R4)                                                  
         BCT   R6,CKINV44                                                       
         J     NEQXIT                                                           
         LTORG                                                                  
         DROP  R4                                                               
*                                                                               
*                                                                               
*                                                                               
WRTREC   NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIO2                                                          
         USING AIOD,R2                                                          
*                                                                               
* SAVE RECORD CURRENTLY IN AIOTSREC                                             
*                                                                               
         LAY   R0,AIOSVCUR                                                      
         LHI   R1,AIORECLQ                                                      
         LA    RE,AIOTSREC                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         DROP  R2                                                               
*                                                                               
* FLUSH THE ENTIRE BUFFER NOW                                                   
*                                                                               
         L     R2,AIO2                                                          
         LA    R2,(AIOSVST-AIOD)(R2)                                            
         LHI   R3,AIORECNQ                                                      
*                                                                               
WRTR10   DS    0H                                                               
         OC    0(2,R2),0(R2)       ANYTHING THERE?                              
         BZ    WRTR20              GO TO NEXT SLOT                              
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,0(R2)          REC LEN                                      
         BCTR  RF,0                                                             
         L     RE,AIO2                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R2)                                                    
*                                                                               
*                                                                               
         MVI   WRKIACTN,WRKIAADD                                                
         GOTO1 EZWRKIO,WRKIOB                                                   
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* CLEAR THE SLOT THAT HAS JUST BEEN WRITTEN TO WKR FILE                         
         LR    RE,R2                                                            
         LHI   RF,AIORECLQ                                                      
         XCEFL                                                                  
*                                                                               
WRTR20   DS    0H                                                               
         LA    R2,AIORECLQ(R2)                                                  
         BCT   R3,WRTR10                                                        
*                                                                               
* RESTORE CURRENT RECORD BACK TO AIOTSREC                                       
*                                                                               
         L     R2,AIO2                                                          
         USING AIOD,R2                                                          
*                                                                               
         LAY   R0,AIOSVCUR                                                      
         LHI   R1,AIORECLQ                                                      
         LA    RE,AIOTSREC                                                      
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         DROP  R2                                                               
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
SAVEREC  NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIO2                                                          
         USING AIOD,R3                                                          
*                                                                               
         CLC   =C'76',4(R3)                                                     
         BNE   *+12                                                             
         LA    R1,AIOSV76                                                       
         B     SAVER100                                                         
*                                                                               
         CLC   =C'21',4(R3)                                                     
         BNE   *+12                                                             
         LA    R1,AIOSV21                                                       
         B     SAVER100                                                         
*                                                                               
         CLC   =C'22',4(R3)                                                     
         BNE   *+12                                                             
         LA    R1,AIOSV22                                                       
         B     SAVER100                                                         
*                                                                               
         CLC   =C'23',4(R3)                                                     
         BNE   *+12                                                             
         LA    R1,AIOSV23                                                       
         B     SAVER100                                                         
*                                                                               
* COMMENT RECORDS ARE HANDLED DIFFERENTLY.  5 OF EACH ARE SAVED                 
*                                                                               
         CLC   =C'24',4(R3)                                                     
         BNE   *+12                                                             
         LA    R1,AIOSV24                                                       
         B     SAVER200                                                         
*                                                                               
         CLC   =C'25',4(R3)                                                     
         BNE   *+12                                                             
         LA    R1,AIOSV25                                                       
         B     SAVER200                                                         
*                                                                               
         J     NEQXIT                                                           
*                                                                               
SAVER100 DS    0H                                                               
         BRAS  RE,MVREC                                                         
         J     EQXIT                                                            
*                                                                               
SAVER200 DS    0H                                                               
         LR    R2,R1               ADDR OF SAVE BUFFER                          
*                                                                               
         CLC   SVRECTYP,4(R3)      PREV RECORD OF SAME TYPE?                    
         BE    SAVER220            YES, DO NOT CLEAR THE BUFFER                 
*                                                                               
* NO - NEW SET OF COMMENTS, CLEAR THE SAVE AREA                                 
*                                                                               
         LR    RE,R1                                                            
         LHI   RF,AIONCOMQ*AIORECLQ                                             
         XCEFL                                                                  
*                                                                               
SAVER220 DS    0H                                                               
         LHI   R0,AIONCOMQ                                                      
*                                                                               
         OC    0(2,R2),0(R2)       ANYTHING IN THIS SLOT?                       
         BZ    SAVER240                                                         
         LA    R2,AIORECLQ(R2)     ADVANCE TO NEXT SLOT                         
         BCT   R0,*-14                                                          
*                                                                               
         J     EQXIT                                                            
*                                                                               
SAVER240 DS    0H                                                               
         LR    R1,R2               PUT ADDR OF EMPTY COMMENT SLOT IN R1         
         BRAS  RE,MVREC                                                         
         J     EQXIT                                                            
*                                                                               
         LTORG                                                                  
         DROP  R3                                                               
*                                                                               
*                                                                               
*                                                                               
* SAVE RECORD CURRENTLY IN AIO2(AIOTSREC)IN BUFFER AREA                         
* R1 EXPECTED TO ADDRESS THE BUFFER AREA                                        
MVREC    NTR1  BASE=*,LABEL=*                                                   
         LR    R2,R1               SAVE ADDR OF RECORD'S SAVE AREA              
*                                                                               
* CLEAR RECORD SAVE AREA IN BUFFER                                              
         LR    RE,R1                                                            
         LHI   RF,AIORECLQ                                                      
         XCEFL                                                                  
*                                                                               
* MOVE RECORD AT AIOTSREC INTO DESIGNATED SAVE AREA                             
         L     RE,AIO2                                                          
         XR    RF,RF                                                            
         ICM   RF,3,0(R2)          RECORD LENGTH                                
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         J     EQXIT                                                            
         MVC   0(0,R2),0(RE)                                                    
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
TOPROC   TM    INVSW,INVSWWR+INVSWNE BATCH WRITTEN AND "OTHER" INVOICE?         
         JO    TOPROCQX              YES, PROCESS                               
         TM    INVSW,INVSWWR+INVSWNE OPPOSITE?                                  
         JZ    TOPROCQX              YES, PROCESS                               
         LTR   RB,RB               UNEQ CC OTHERWISE                            
         BR    RE                                                               
TOPROCQX CR    RB,RB               EQ CC EXIT                                   
         BR    RE                                                               
                                                                                
*                                                                               
*                                                                               
*                                                                               
ISEND    NTR1  BASE=*,LABEL=*                                                   
         TM    INVSW,INVSWEQ+INVSWNE         PROCESSING AN INVOICE NOW?         
         JZ    NEQXIT              NO - DON'T DO END OF INVOICE                 
*                                                                               
         L     R3,AIO2                                                          
*                                                                               
* SEE IF NEED TO SET END OF INOVICE FLAG                                        
*                                                                               
         CLC   =C'21',4(R3)                                                     
         BE    IE40                                                             
         CLC   =C'22',4(R3)                                                     
         BE    IE40                                                             
         CLC   =C'23',4(R3)                                                     
         BE    IE40                                                             
         CLC   =C'12',4(R3)                                                     
         BE    IE40                                                             
         CLC   =C'31',4(R3)                                                     
         JNE   NEQXIT                                                           
*                                                                               
IE40     DS    0H                                                               
         OI    INVSW,INVSWEND      SET END OF INVOICE                           
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
PROCREC  NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIO2                                                          
*                                                                               
* SAVE ALL KINDS OF DATA FROM RECORDS                                           
*                                                                               
         CLC   =C'31',4(R3)                                                     
         BNE   PR100                                                            
*                                                                               
         XC    SVADVNAM,SVADVNAM                                                
         XC    SVPRDNAM,SVPRDNAM                                                
         XC    SVBRDMOS,SVBRDMOS                                                
         XC    SVINVNUM,SVINVNUM                                                
         XC    SVNET,SVNET                                                      
*                                                                               
         GOTO1 =A(GETDATA),DMCB,5,SVADVNAM,RR=RELO                              
         GOTO1 =A(GETDATA),DMCB,6,SVPRDNAM,RR=RELO                              
         GOTO1 =A(GETDATA),DMCB,11,SVBRDMOS,RR=RELO                             
         GOTO1 =A(GETDATA),DMCB,10,SVINVNUM,RR=RELO                             
         GOTO1 =A(GETDATA),DMCB,32,SVNET,RR=RELO                                
*                                                                               
         OC    SVADVNAM,SPACES                                                  
         OC    SVPRDNAM,SPACES                                                  
         OC    SVBRDMOS,SPACES                                                  
         OC    SVINVNUM,SPACES                                                  
         OC    SVNET,SPACES                                                     
*                                                                               
         J     EQXIT                                                            
*                                                                               
PR100    DS    0H                                                               
         CLC   =C'21',4(R3)        THIS AN AGENCY REC                           
         BNE   PR200                                                            
*                                                                               
         XC    SVAGYNAM,SVAGYNAM                                                
*                                                                               
         GOTO1 =A(GETDATA),DMCB,3,SVAGYNAM,RR=RELO                              
         OC    SVAGYNAM,SPACES                                                  
*                                                                               
         J     EQXIT                                                            
*                                                                               
PR200    DS    0H                                                               
         CLC   =C'22',4(R3)        THIS A STATION REC                           
         BNE   PR300                                                            
*                                                                               
         XC    SVCALL,SVCALL                                                    
         XC    SVMEDIA,SVMEDIA                                                  
*                                                                               
         GOTO1 =A(GETDATA),DMCB,2,SVCALL,RR=RELO                                
         OC    SVCALL,SPACES                                                    
*                                                                               
         GOTO1 =A(GETDATA),DMCB,3,SVMEDIA,RR=RELO                               
         OC    SVCALL,SPACES                                                    
*                                                                               
         J     EQXIT                                                            
*                                                                               
PR300    DS    0H                                                               
         CLC   =C'34',4(R3)        INVOICE TOTATL ?                             
         BNE   PR400                NO                                          
*                                                                               
         XC    SVGBILL,SVGBILL                                                  
         XC    SVNBILL,SVNBILL                                                  
         XC    SVSPOTS,SVSPOTS                                                  
*                                                                               
         LHI   R1,3                GROSS DOLLARS                                
         BRAS  RE,GETFDISP                                                      
         BZ    PR310                                                            
         LTR   RF,RF                                                            
         BZ    PR310                                                            
*                                                                               
         LHI   RE,L'SVGBILL                                                     
         SR    RE,RF                                                            
         LA    RE,SVGBILL(RE)                                                   
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R1)                                                    
         OC    SVGBILL,SPACES                                                   
*                                                                               
PR310    DS    0H                                                               
         LHI   R1,5                NET DUE                                      
         BRAS  RE,GETFDISP                                                      
         BZ    PR320                                                            
         LTR   RF,RF                                                            
         BZ    PR320                                                            
*                                                                               
         LHI   RE,L'SVNBILL                                                     
         SR    RE,RF                                                            
         LA    RE,SVNBILL(RE)                                                   
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R1)                                                    
         OC    SVNBILL,SPACES                                                   
*                                                                               
PR320    DS    0H                                                               
         OC    SPOTCNT,SPOTCNT                                                  
         BZ    PR400                                                            
         EDIT  (B3,SPOTCNT),(5,SVSPOTS)                                         
         OC    SVSPOTS,SPACES                                                   
*                                                                               
         J     EQXIT                                                            
*                                                                               
PR400    DS    0H                                                               
         CLC   =C'51',4(R3)        THIS A BROADCAST DETAIL                      
         BNE   PR500                                                            
*                                                                               
         ZICM  RE,SPOTCNT,3        COUNT NUMBER OF SPOTS                        
         LA    RE,1(RE)                                                         
         STCM  RE,7,SPOTCNT                                                     
*                                                                               
PR500    DS    0H                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
BLDEZMOV NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   DATADISP+1,42                                                    
         MVI   LKEY+1,32                                                        
         MVC   SYSDIR(6),=C'GENDIR'                                             
         MVC   SYSFIL(6),=C'GENFIL'                                             
*                                                                               
         MVI   CURSYST,C'C'        SWITCH TO CONTROL FILE                       
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
*                                                                               
         USING EZMKEY,R4                                                        
*                                                                               
         MVC   EZMKID,=C'ZM'       RECORD ID                                    
         MVC   EZMOUID,OUIDNUM     OLD USER ID                                  
         MVC   EZMNUID,NUIDNUM     NEW USER ID                                  
*                                                                               
         MVC   EZMKSTA,SVEZKSTA    STATION                                      
         OC    NEWSTA,NEWSTA                                                    
         BZ    *+10                                                             
         MVC   EZMKSTA,NEWSTA      STATION                                      
*                                                                               
         MVC   EZMKINV,SVINVNUM    INVOICE NUMBER                               
         MVC   EZMKDTP,RQDTE       DATE (COMPRESSED)                            
         XC    EZMKDTP,=X'FFFF'    PACKED BATCH DATE (INVERTED)                 
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         GOTO1 HIGH                OLD KEY IS IN KEYSAVE                        
*                                                                               
         CLC   KEY(23),KEYSAVE     WAS THE REC FOUND                            
         BE    *+20                                                             
         MVC   KEY(23),KEYSAVE     RESTORE THE KEY                              
         MVC   EZMKSEQ,=X'FFFFFF'  SEQUENCE NUMBER                              
         B     STR47               GO BUILD MOVE RECORD                         
*                                                                               
         ZICM  RE,KEY+23,3         GET LAST SEQ NUMBER USED                     
         BCTR  RE,0                DECREMENT IT BY ONE                          
         STCM  RE,7,EZMKSEQ        PUT SEQ NUMBER TO KEY                        
*                                                                               
         DROP  R4                                                               
*                                                                               
* BUILD NEW MOVE RECORD                                                         
*                                                                               
STR47    DS    0H                                                               
         L     RE,AIO1                                                          
         LA    RF,2000(RE)                                                      
         SR    RF,RE                                                            
         XCEFL                                                                  
*                                                                               
         MVC   0(32,R6),KEY                                                     
         MVI   33(R6),39           LENGTH                                       
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING EZMDTAEL,R6                                                      
*                                                                               
         MVI   EZMDTAEL,X'10'                                                   
         MVI   EZMDTALN,EZMDTAX-EZMDTAEL                                        
         MVC   EZMMEDIA,SVMEDIA    MEDIA                                        
         MVC   EZMCALL,SVCALL      CALL LETTERS                                 
*                                                                               
         OC    NEWSTA,NEWSTA                                                    
         BZ    *+10                                                             
         MVC   EZMOLDST,OLDSTA                                                  
*                                                                               
         MVC   EZMNET,SVNET        NETWORK                                      
         MVC   EZMMOS,SVBRDMOS     BROADCAST MONTH                              
         MVC   EZMADVNM,SVADVNAM   ADVERTISER NAME                              
         MVC   EZMPRDNM,SVPRDNAM   PRODUCT NAME                                 
         MVC   EZMNSPTS,SVSPOTS    NUMBER OF SPOTS                              
         MVC   EZMGDOL,SVGBILL     GROSS BILLING                                
         MVC   EZMNDOL,SVNBILL     NET BILLING                                  
         MVC   EZMAGYNM,SVAGYNAM   AGENCY NAME                                  
         MVC   EZMFAGY,OUID        FROM AGENCY                                  
         MVC   EZMTAGY,NUID        TO AGENCY                                    
         MVC   EZMSRCE,SVEZDSC+EZWCSRCE-EZWKRCMD                                
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,EZMDATE) COMPRESSED                         
*                                                                               
         THMS  DDSTIME=YES                                                      
         STCM  R1,15,PACK1                                                      
         STCM  R0,15,PACK2                                                      
         AP    PACK2,PACK1         DDS TIME IS OFFSET BY 6AM                    
*                                                                               
         CP    PACK2,=P'240000'    PAST MIDNIGHT?                               
         BL    STR47A                                                           
         SP    PACK2,=P'240000'    YUP, BUMP TO NEXT DAY AND ADJUST             
         ZICM  R1,EZMDATE,2                                                     
         LA    R1,1(R1)            ADD ONE DAY                                  
         STCM  R1,3,EZMDATE                                                     
*                                                                               
STR47A   DS    0H                                                               
         ICM   R1,15,PACK2                                                      
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STCM  R1,3,EZMTIME        CURRENT TIME PWOS                            
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'       IS PASSWORD PROTECT ACTIVE                   
         BZ    *+10                                                             
         MVC   EZMPID,FAPASSWD    YES SO USE THIS ID                            
         DROP  R1                                                               
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         GOTO1 ADDREC                                                           
*                                                                               
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* SPEZFFFD                                                                      
* CTGENFILE                                                                     
       ++INCLUDE SPGENEZ                                                        
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE SPEZMOV                                                        
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
*                                                                               
       ++INCLUDE SPEZFFFD                                                       
*                                                                               
         ORG   CONTAGH                                                          
* SPEZFE7D                                                                      
       ++INCLUDE SPEZFE7D                                                       
*                                                                               
*                                                                               
* DDGENTWA                                                                      
       ++INCLUDE DDGENTWA                                                       
*                                                                               
* DMWRKFD                                                                       
       ++INCLUDE DMWRKFD                                                        
*                                                                               
* DMWRKFK                                                                       
       ++INCLUDE DMWRKFK                                                        
*                                                                               
* DMWRKFL                                                                       
       ++INCLUDE DMWRKFL                                                        
*                                                                               
* CTGENFILE                                                                     
       ++INCLUDE CTGENFILE                                                      
*                                                                               
*SPEZFWORKD                                                                     
       ++INCLUDE SPEZFWORKD                                                     
*XTRAINFD                                                                       
       ++INCLUDE FAXTRAINF                                                      
*COMFACSD                                                                       
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
       ++INCLUDE FAFACTS                                                        
*                                                                               
       ++INCLUDE SPEZDSCTS                                                      
       ++INCLUDE EZFLDTAB                                                       
*                                                                               
*                                                                               
*                                                                               
RQINVD   DSECT                                                                  
RQINVNUM DS    CL10                                                             
RQINVFLG DS    X                                                                
RQFFOUNQ EQU   X'80'               INVOICE FOUND                                
RQINVDLQ EQU   *-RQINVD                                                         
*                                                                               
*                                                                               
*                                                                               
AIOD     DSECT                                                                  
*                                                                               
AIORECLQ EQU   372+4               LENGTH OF SAVED RECORD                       
*                                                                               
AIOTSREC DS    XL(AIORECLQ)                                                     
*                                                                               
AIOSVST  DS    0X                  START OF RECORD SAVE BUFFER                  
AIOSV76  DS    XL(AIORECLQ)                                                     
AIOSV21  DS    XL(AIORECLQ)                                                     
AIOSV22  DS    XL(AIORECLQ)                                                     
AIOSV23  DS    XL(AIORECLQ)                                                     
*                                                                               
AIONCOMQ EQU   5                   NUMBER OF COMMENT LINES SAVED                
*                                                                               
AIOSV24  DS    XL(AIORECLQ*AIONCOMQ)                                            
AIOSV25  DS    XL(AIORECLQ*AIONCOMQ)                                            
*                                                                               
AIORECNQ EQU   (*-AIOSVST)/AIORECLQ                                             
*                                                                               
AIOSVCUR DS    XL(AIORECLQ)        SAVED CURRENT RECORD                         
*                                                                               
AIODLQ   EQU   *-AIOD                                                           
*                                                                               
*                                                                               
*                                                                               
*                                                                               
* DSECT FOR THIS PROGRAM *                                                      
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
RELO     DS    A                                                                
ATSAR    DS    A                                                                
EZWRKIO  DS    A                                                                
EZPARSE  DS    A                                                                
TSARLAST DS    H                   LAST TSAR RECORD NUMBER                      
*                                                                               
SVEZKEY  DS    XL(L'WRKEZKEY)                                                   
SVEZDSC  DS    XL(L'WRKEZDSC)                                                   
SVEZUDT  DS    XL(L'WRKEZUDT)                                                   
SVEZMOS  DS    XL(L'WRKEZMOS)                                                   
SVWKAGED DS    XL(L'WRKEZBDT)                                                   
SVWKAGET DS    XL(L'WRKEZBTM)                                                   
*                                                                               
       ++INCLUDE EZLKAGYBLK                                                     
*                                                                               
RQINVCT  EQU   25                                                               
RQINV    DS    (RQINVCT*RQINVDLQ)C                                              
*                                                                               
SVINVNUM DS    CL10                INVOICE NUMBER                               
SVRECTYP DS    CL2                                                              
*                                                                               
SVAGYNAM DS    CL30                AGENCY NAME                                  
*                                                                               
SVCALL   DS    CL5                 4 CALL LETTERS+BAND                          
SVMEDIA  DS    CL2                 T/R                                          
*                                                                               
SVADVNAM DS    CL25                ADV NAME                                     
SVPRDNAM DS    CL25                PROD NAME                                    
SVBRDMOS DS    CL4                 BROADCAST MONTH                              
SVNET    DS    CL4                 NETWORK FOR LOCAL CABLE                      
SVNBILL  DS    CL11                NET BILLING                                  
SVGBILL  DS    CL11                GROSS BILLING                                
SVSPOTS  DS    CL5                 SAVE NUMBER OF SPOTS                         
SPOTCNT  DS    CL3                 NUMBER OF SPOTS COUNTER                      
*                                                                               
SVEZKSTA DS    CL5                 STATION /MEDIA                               
*                                                                               
PACK1    DS    PL4                                                              
PACK2    DS    PL4                                                              
*                                                                               
SVWCPTIM DS    XL2                 PROCESSED TIME-FORCED 0100                   
SVWCICNT DS    XL2                 INV CT - NOT USED YET                        
SVWCPCNT DS    XL2                 PROCESSED INV CT - NOT USED YET              
SVWCSRCE DS    CL4                                                              
         DS    CL2                 SPARE                                        
*                                                                               
OUID     DS    CL8                 OLD AGENCY                                   
OUIDNUM  DS    H                                                                
OUIDPC   DS    H                   POWER CODE                                   
NUID     DS    CL8                 NEW AGENCY                                   
NUIDNUM  DS    H                                                                
NUIDPC   DS    H                   POWER CODE                                   
NWKFIL   DS    CL8                                                              
*                                                                               
RQSTA    DS    CL5                 REQUESTED STATION                            
OLDSTA   DS    CL5                 REQUESTED STATION                            
NEWSTA   DS    CL5                 EQUIVALENT  STATION                          
RQDTE    DS    XL2                           BATCH DATE                         
RQSEQ    DS    CL6                           BATCH SEQ                          
RQBSEQ   DS    XL4                                                              
*                                                                               
EQUIVSW  DS    CL1                                                              
*                                                                               
INVSW    DS    XL1                                                              
INVSWSP  EQU   X'80'               SPECIFIC INVOICE REQUEST                     
INVSWFND EQU   X'40'               SPECIFIC INVOICE FOUND                       
INVSWOT  EQU   X'20'               OTHER INVOICES IN BATCH                      
INVSWWR  EQU   X'10'               REQUESTED INVOICE NEW BATCH WRITTEN          
INVSWEQ  EQU   X'08'               THIS IS REQUESTED INVOICE                    
INVSWNE  EQU   X'04'               THIS IS NOT REQUESTED INVOICE                
INVSWEND EQU   X'02'               END OF INVOICE                               
INVSWALL EQU   X'01'               ALL-INVOICE REQUEST                          
*                                                                               
FTMAXENT EQU   50                                                               
FLDTAB   DS    0X                                                               
         DS    (FTMAXENT*FTLENQ)X                                               
*                                                                               
*                                                                               
       ++INCLUDE DDWRKIOD                                                       
*                                                                               
TSARBLK  DS    CL(TSPXTNL)                                                      
*                                                                               
WRKFBUFR DS    0D                                                               
         DS    14336X                                                           
WRKFBFLQ EQU   *-WRKFBUFR                                                       
WRKFEND  EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'079SPEZF15   03/06/20'                                      
         END                                                                    
