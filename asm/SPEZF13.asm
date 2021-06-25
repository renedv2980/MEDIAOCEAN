*          DATA SET SPEZF13    AT LEVEL 049 AS OF 05/17/07                      
*PHASE T23013A                                                                  
*INCLUDE QSORT                                                                  
***********************************************************************         
*  TITLE: T23013 - EASI INVOICE LISTS INVOICE FIGURES BY AGENCY       *         
*                                                                     *         
*  OUTPUTS: NOW REPORT OR SCREEN LIST                                 *         
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
*             AIO2 - EZBLOCK                                          *         
*             AIO3 - WORKER RECORD - 1ST 1024                         *         
*                  - EZMOD REC AREA NEXT 2000                         *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'T23013 - INVOICES PER AGENCY LIST'                              
*                                                                               
T23013   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T23013**,R7,RR=R2                                              
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
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    INVAL                                                            
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    INVAL                                                            
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    INVAL                                                            
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
         J     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
*                                                                               
***********************************************************************         
*   VKEY - VALIDATE KEY                                               *         
***********************************************************************         
*                                                                               
VKEY     CLI   ACTNUM,14          THIS HAD BETTER BE COUNTS                     
         BE    *+12                                                             
         CLI   ACTNUM,ACTSEL                                                    
         BNE   INVAL                                                            
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
*                                                                               
         MVC   RQSTA,QSTA                                                       
*                                                                               
VK100    OI    4(R2),X'20'                                                      
         LA    R2,LINBDTH          BATCH DATE                                   
         XC    RQDTES,RQDTES                                                    
         CLI   5(R2),0             IF NO DATE                                   
         BE    VK200                                                            
*                                                                               
         GOTO1 DATVAL,DMCB,(0,FHDRLEN(R2)),WORK                                 
         ICM   R3,15,DMCB                                                       
         BZ    BADATE                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(2,RQDTSTR)                                 
*                                                                               
         CLM   R3,1,5(R2)          ONLY 1 DATE ENTERED                          
         BE    VK160                YES                                         
         LA    R3,1+8(R2,R3)                                                    
*                                                                               
         GOTO1 DATVAL,DMCB,(0,(R3)),WORK                                        
         OC    DMCB,DMCB                                                        
         BZ    BADATE                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(2,RQDTEND)                                 
*                                                                               
         CLC   RQDTSTR,RQDTEND                                                  
         BH    BADATE                                                           
         B     VK200                                                            
*                                                                               
VK160    MVC   RQDTEND,RQDTSTR                                                  
*                                                                               
VK200    OI    4(R2),X'20'                                                      
         LA    R2,LINBSQH          SEQ                                          
         XC    RQSEQ,RQSEQ                                                      
         XC    RQBSEQ,RQBSEQ                                                    
         CLI   5(R2),0             ANY INPUT                                    
         BE    VK300                                                            
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
         UNPK  FULL,DUB                                                         
*                                                                               
         MVC   RQSEQ,FULL                                                       
         STCM  R0,3,RQBSEQ                                                      
         B     VK300                                                            
*                                                                               
VKMVN    MVN   WORK(0),8(R2)                                                    
VKCLC    CLC   WORK(0),8(R2)                                                    
VKPK     PACK  DUB,8(0,R2)                                                      
*                                  FILTERS LINE                                 
VK300    OI    4(R2),X'20'                                                      
         LA    R2,LINFTRH          FILTERS                                      
         GOTO1 =A(VFTR),RR=RELO                                                 
         OI    4(R2),X'20'                                                      
*                                                                               
VKXIT    XC    KEY,KEY                                                          
         B     EXIT                                                             
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
         L     R6,AIO2             SET EZBLOCK                                  
         USING EZBLOCKD,R6                                                      
         LR    RE,R6               CLEAR EZBLOCK                                
         LHI   RF,EZBLOCKL                                                      
         XCEFL                                                                  
*                                                                               
         LA    RE,EZCOUNT          COUNT IT ALL ROUTINE                         
         ST    RE,EZHOOK                                                        
*                                                                               
         MVC   EZWKRFIL,EASIWK                                                  
         MVC   EZPRINT,VPRINT                                                   
         LA    RE,WRKFBUFR                                                      
         ST    RE,EZWKRBUF                                                      
         L     RE,AIO3                                                          
         ST    RE,EZWKRREC                                                      
         LA    RE,1024(,RE)                                                     
         ST    RE,EZAREC                                                        
         L     RE,ACOMFACS                                                      
         ST    RE,EZCOMFCS                                                      
         MVI   EZLOOKSW,X'E0'                                                   
         MVC   EZTRACE,FTRTRACE    TRACE FIELDS                                 
         MVI   EZTEST,C'Y'                                                      
         MVI   EZWRITE,C'N'                                                     
         DROP  R6                                                               
*                                                                               
         MVI   USEIO,C'Y'          SET USER WILL DO ALL I/O                     
         LM    R0,R1,=A(HEADING,HDHK)                                           
         A     R0,RELO                                                          
         ST    R0,SPECS                                                         
         A     R1,RELO                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         L     R0,AIO1                                                          
         ST    R0,AAGYTAB                                                       
         LA    R0,2000/AGYENTL                                                  
         ST    R0,AAGYTABS                                                      
         XC    ASRCTAB,ASRCTAB                                                  
         XC    ASRCTABE,ASRCTABE                                                
         XC    AMEDTAB,AMEDTAB                                                  
         XC    AMEDTABE,AMEDTABE                                                
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   LS050                                                            
*                                                                               
         L     R1,VADUMMY                                                       
         MVC   0(8,R1),=C'*INVTAB*'                                             
         LA    R1,8(,R1)                                                        
         ST    R1,AAGYTAB                                                       
         MVC   AAGYTABS,=A(52000/AGYENTL)                                       
         A     R1,=F'52000'                                                     
*                                                                               
         MVC   0(8,R1),=C'*SRCTAB*'                                             
         LA    R1,8(,R1)                                                        
         ST    R1,ASRCTAB                                                       
*        LA    R1,85*SRCENTL(,R1)                                               
         LHI   RF,150*SRCENTL                                                   
         AR    R1,RF                                                            
         ST    R1,ASRCTABE                                                      
*                                                                               
         MVC   0(8,R1),=C'*MEDTAB*'                                             
         LA    R1,8(,R1)                                                        
         ST    R1,AMEDTAB                                                       
         LA    R1,10*MEDENTL(,R1)                                               
         ST    R1,AMEDTABE                                                      
*                                                                               
         L     RE,ASRCTAB                                                       
         L     RF,ASRCTABE                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         L     RE,AMEDTAB                                                       
         L     RF,AMEDTABE                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
LS050    MVI   NLISTS,NUMLINS                                                   
         L     RE,AAGYTAB                                                       
         L     RF,AAGYTABS                                                      
         MH    RF,=AL2(AGYENTL)                                                 
         XCEF                                                                   
*                                                                               
         LA    R4,SVWEZIND                                                      
         USING EZWKRIXD,R4                                                      
*                                                                               
         XC    SVWEZIND,SVWEZIND                                                
         LA    R1,JTOT                                                          
         SR    R2,R2                                                            
         BRAS  RE,UPDTOT                                                        
         ZAP   JBYPASSC,=P'0'                                                   
         ZAP   BATSRD,=P'0'                                                     
         MVC   SVSRCE,SPACES                                                    
*                                                                               
* LOOP THRU ALL BATCHES - BUILDING TABLE OF INVOICES BY AGENCY  *               
*                                                                               
LS100    DS    0H                                                               
         LA    R4,SVWEZIND                                                      
         GOTO1 DATAMGR,DMCB,(X'00',=C'INDEX'),                         X        
               EASIWK,SVWEZIND,AIO3,WRKFBUFR                                    
*                                                                               
         TM    DMCB+8,X'80'        TEST EOF                                     
         BZ    LS102                                                            
*                                                                               
LS101A   DS    0H                                                               
         CLI   FTRWKR,X'00'                                                     
         BNE   LS200                                                            
*                                                                               
         CLI   FUIDNUM,X'FF'       TEST 'ALL' IDS                               
         BNE   LS200                                                            
         CLI   EASIWK+4,C'F'                                                    
         BE    LS200                                                            
         CLI   EASIWK+4,C'9'                                                    
         BNE   *+12                                                             
*                                                                               
         MVI   EASIWK+4,C'A'                                                    
         B     LS101C                                                           
*                                                                               
         ZIC   RF,EASIWK+4                                                      
         LA    RF,1(,RF)                                                        
         STC   RF,EASIWK+4                                                      
*                                                                               
LS101C   DS    0H                                                               
         L     RF,AIO2                                                          
         MVC   EZWKRFIL-EZBLOCKD(,RF),EASIWK                                    
         XC    SVWEZIND,SVWEZIND                                                
         B     LS100                                                            
*                                                                               
LS102    DS    0H                                                               
         OC    SVWEZIND(2),SVWEZIND                                             
         BZ    LS101A                                                           
*                                                                               
         AP    BATSRD,=P'1'                                                     
*                                                                               
         TM    FTRFLAG,FTRTEST     TEST - ONLY READ 500 BATCHES                 
         BZ    LS103                                                            
         CP    BATSRD,=P'499'      STOP AT 500                                  
         BH    LS200                                                            
*                                                                               
LS103    DS    0H                                                               
         CLI   FTRKEY,C'Y'         PRINT TRACE OF INDEX                         
         BNE   LS104                                                            
*                                                                               
* PRINT HEXADECIMAL TRACE OF INDEXES *                                          
*                                                                               
         LA    R0,4                                                             
         LA    R6,P+3                                                           
         LA    R5,SVWEZIND                                                      
LS103C   GOTO1 HEXOUT,DMCB,(R5),(R6),4                                          
         LA    R6,9(,R6)                                                        
         LA    R5,4(,R5)                                                        
         BCT   R0,LS103C                                                        
*                                                                               
         MVC   P+40(40),SVWEZIND                                                
         MVC   P+85(5),EASIWK                                                   
         EDIT  (P8,BATSRD),(9,P+91),COMMAS=YES,ZERO=NOBLANK                     
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         USING EZWKRIXD,R4                                                      
LS104    CLI   EZWIDAY,X'99'       MUST BE DAY 99                               
         BNE   LS100                                                            
*                                                                               
         OC    RQDTES,RQDTES       DATE FILTER                                  
         BZ    LS106                                                            
*                                                                               
         CLC   UKAGELD-UKRECD+SVWEZIND,RQDTSTR                                  
         BL    LS100               LOW, SKIP                                    
         CLC   UKAGELD-UKRECD+SVWEZIND,RQDTEND                                  
         BH    LS100               HIGH, SKIP                                   
*                                                                               
LS106    DS    0H                                                               
         CLI   FUIDNUM,X'FF'       TEST 'ALL' IDS                               
         BE    LS110                                                            
         OC    FUIDNUM,FUIDNUM     DIFFERENT USER ID                            
         BZ    LS108                                                            
         CLC   EZWIUID,FUIDNUM     ELSE, TEST RIGHT ID                          
         BNE   LS100                                                            
         B     LS110                                                            
LS108    CLC   EZWIUID,TWAORIG     ELSE, TEST RIGHT ID                          
         BNE   LS100                                                            
*                                                                               
LS110    MVC   SVUID,EZWIUID                                                    
*                                                                               
         TM    FTRFLAG,FTRALL      INCLUDE ALL?                                 
         BO    LS111                                                            
*                                                                               
         CLC   SVUID,=H'0011'      BYPASS SJR                                   
         BE    LS100                                                            
         CLC   SVUID,=H'2894'      BYPASS NFNY                                  
         BE    LS100                                                            
*                                                                               
LS111    MVC   SRCESTA(4),EZWISTN  STATION                                      
         MVC   SRCESTA+4(1),EZWIMED                                             
         MVC   ORIGSTA,SRCESTA                                                  
*                                                                               
         CLI   SRCESTA+3,C' '                                                   
         BH    *+8                                                              
         MVI   SRCESTA+3,C' '                                                   
*                                                                               
LS114    MVC   EQUISTA,SRCESTA                                                  
*                                                                               
         CLI   FUIDNUM,X'FF'       ALL USERS                                    
         BE    LS115                                                            
*                                                                               
         GOTO1 VEQVSTA             GO GET EQUIVALENT STATION                    
*                                                                               
LS115    OC    RQSTA,RQSTA         STATION FILTER                               
         BZ    LS116                                                            
*                                                                               
         CLC   EQUISTA(5),RQSTA                                                 
         BNE   LS100                                                            
*                                                                               
LS116    OC    RQBSEQ,RQBSEQ       BATCH SEQ FILTER                             
         BZ    *+14                                                             
         CLC   SVWEZIND+8(2),RQBSEQ                                             
         BNE   LS100                                                            
*                                                                               
         MVC   SVSNMED,EQUISTA+4                                                
         CLI   SVSNMED,C'T'                                                     
         BE    LS126                                                            
         CLI   SVSNMED,C'C'                                                     
         BE    LS126                                                            
         CLI   SVSNMED,C'S'                                                     
         BE    LS126                                                            
         CLI   SVSNMED,C'N'                                                     
         BE    LS126                                                            
         CLI   SVSNMED,C'X'                                                     
         BE    LS126                                                            
         CLI   EQUISTA+4,C'A'                                                   
         BE    LS118                                                            
         CLI   EQUISTA+4,C'F'                                                   
         BNE   LS120                                                            
LS118    MVI   SVSNMED,C'R'                                                     
         B     LS126                                                            
*                                                                               
LS120    MVI   SVSNMED,C'?'        MARK UNKNOWN MEDIA                           
         MVI   EQUISTA+4,C'?'                                                   
*                                                                               
LS126    CLI   FTRMEDIA,0                                                       
         BE    LS128                                                            
         CLC   FTRMEDIA,SVSNMED                                                 
         BNE   LS100                                                            
*                                                                               
LS128    DS    0H                                                               
         MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   SET ON RETURN                                
         GOTO1 CATCHIOS            SEE IF 90% OF MAX                            
         CLI   ERROR,0             IF ERROR, OVER MAX                           
         BNE   MAXIOSER                                                         
*                                                                               
         LA    R3,WRKFBUFR                                                      
         GOTO1 DATAMGR,DMCB,(X'08',=C'READ'),EASIWK,SVWEZIND,AIO3,(R3)          
         TM    DMCB+8,X'80'        TEST EOF ON FIRST READ                       
         BNZ   LS100                YES, SKIP                                   
*                                                                               
         USING W_RECD,R3                                                        
         MVC   SVWCMNT,W_DESC                                                   
*                                                                               
         OC    RQDTES,RQDTES       DATE FILTER                                  
         BZ    LS140                                                            
         CLC   W_AGELD,RQDTSTR                                                  
         BL    LS100               LOW, SKIP                                    
         BE    LS140               EQUAL, OK                                    
*                                                                               
         CLC   W_AGELD,RQDTEND                                                  
         BH    LS100               HIGH, SKIP                                   
*                                                                               
LS140    OC    FTRBSDT,FTRBSDT     FILTERING ON BATCH DATE                      
         BZ    LS142                NO                                          
         CLC   W_AGELD,FTRBSDT                                                  
         BL    LS100               LOW, SKIP                                    
         CLC   W_AGELD,FTRBEDT                                                  
         BH    LS100               HIGH, SKIP                                   
*                                                                               
*                                  DISPLAY FOUND DATE AND BATCH SEQ             
*                                                                               
LS142    MVC   SVWKDTEC,W_AGELD       SAVE DATE AND BATCH SEQ                   
         MVC   SVWKFILN,W_FILENO                                                
         MVC   SVWKSTAT,W_STAT                                                  
*                                                                               
         LA    R2,W_DESC                                                        
         USING EZWKRCMD,R2                                                      
*                                                                               
         OC    FTRSRCE,FTRSRCE     FILTERING ON SOURCE                          
         BZ    LS144                                                            
         CLC   FTRSRCE,EZWCSRCE                                                 
         BE    LS144                                                            
*                                                                               
         CLC   =C'SDI',FTRSRCE     THIS SDI                                     
         BNE   LS100                                                            
         CLC   =C'JW',AGENCY       AND JWT                                      
         BNE   LS100                                                            
         CLI   FTRSRCE+3,C' '      AND 3 CHAR                                   
         BH    LS100                                                            
         CLC   =C'SDI',EZWCSRCE                                                 
         BNE   LS100                                                            
*                                                                               
LS144    L     R6,AIO2             SET EZBLOCK                                  
         USING EZBLOCKD,R6                                                      
*                                                                               
         OC    EZWCSRCE,SPACES                                                  
         MVC   EZSRCE,EZWCSRCE                                                  
         MVC   SVSRCE,EZWCSRCE                                                  
         DROP  R2,R3                                                            
*                                                                               
         TM    FTRFLAG,FTRALL      INCLUDE ALL?                                 
         BO    LS146                                                            
*                                                                               
         CLC   SVSRCE,=C'DDS '     BYPASS ANY SOURCE = DDS                      
         BE    LS100                                                            
         CLC   SVSRCE,=C'IRNY'     BYPASS IRNY TRANSFERS                        
         BE    LS100                                                            
*                                                                               
LS146    DS    0H                                                               
         LA    R1,TINVCTS                                                       
         SR    R2,R2                                                            
         BRAS  RE,UPDTOT                                                        
         AP    TBAT,=P'1'                                                       
*                                                                               
         MVC   EZWKRIND,SVWEZIND                                                
*                                                                               
         GOTO1 VEZMOD,DMCB,(R6)                                                 
         DROP  R6                                                               
*                                                                               
* RETURN HERE ONLY AT END OF BATCH, THEN ADD TO TABLE *                         
*                                                                               
         CP    TINV,=P'0'                                                       
         BNH   LS180                                                            
*                                                                               
         L     R0,AAGYTABS                                                      
         L     R3,AAGYTAB                                                       
         USING AGYENTD,R3                                                       
*                                                                               
LS150    OC    AGYSRT,AGYSRT       EMPTY ENTRY                                  
         BZ    LS152                                                            
*                                                                               
         CLC   AGYMED,SVSNMED                                                   
         BNE   LS151                                                            
         CLC   AGYUID,SVUID                                                     
         BE    LS153                                                            
*                                                                               
LS151    LA    R3,AGYNEXT                                                       
         BCT   R0,LS150                                                         
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   SIZERR                                                           
         DC    H'0'                                                             
*                                                                               
* ADD NEW ENTRY TO THE AGY TABLE                                                
*                                                                               
LS152    MVC   AGYUID,SVUID                                                     
         MVC   AGYMED,SVSNMED                                                   
*                                                                               
         LA    R1,AGYCTS                                                        
         SR    R2,R2                                                            
         BRAS  RE,UPDTOT                                                        
*                                                                               
LS153    DS    0H                                                               
         LA    R1,AGYCTS                                                        
         LA    R2,TINVCTS                                                       
         BRAS  RE,UPDTOT                                                        
*                                                                               
         DROP  R3                                                               
*                                                                               
         ICM   R3,15,ASRCTAB                                                    
         BZ    LS156                                                            
         USING SRCENTD,R3                                                       
*                                                                               
LS155    OC    SRCENT,SRCENT       EMPTY ENTRY                                  
         BZ    LS155C                                                           
         CLC   SRCSRC,SVSRCE                                                    
         BE    LS155E                                                           
         LA    R3,SRCNEXT                                                       
         C     R3,ASRCTABE                                                      
         BL    LS155                                                            
*                                                                               
         DC    H'0'                                                             
*                                                                               
LS155C   MVC   SRCSRC,SVSRCE                                                    
         LA    R1,SRCCTS                                                        
         SR    R2,R2                                                            
         BRAS  RE,UPDTOT                                                        
*                                                                               
LS155E   DS    0H                                                               
         LA    R1,SRCCTS                                                        
         LA    R2,TINVCTS                                                       
         BRAS  RE,UPDTOT                                                        
*                                                                               
         DROP  R3                                                               
*                                                                               
LS156    ICM   R3,15,AMEDTAB                                                    
         BZ    LS158                                                            
         USING MEDENTD,R3                                                       
LS157    OC    MEDENT,MEDENT       EMPTY ENTRY                                  
         BZ    LS157C                                                           
         CLC   MEDMED,SVSNMED                                                   
         BE    LS157E                                                           
         LA    R3,MEDNEXT                                                       
         C     R3,AMEDTABE                                                      
         BL    LS157                                                            
*                                                                               
         DC    H'0'                                                             
*                                                                               
LS157C   MVC   MEDMED,SVSNMED                                                   
         LA    R1,MEDCTS                                                        
         SR    R2,R2                                                            
         BRAS  RE,UPDTOT                                                        
*                                                                               
LS157E   SR    RE,RE                                                            
         LA    R1,MEDCTS                                                        
         LA    R2,TINVCTS                                                       
         BRAS  RE,UPDTOT                                                        
*                                                                               
         DROP  R3                                                               
*                                                                               
* ADD TO JOB TOTALS                                                             
*                                                                               
LS158    DS    0H                                                               
         LA    R1,JTOT                                                          
         LA    R2,TINVCTS                                                       
         BRAS  RE,UPDTOT                                                        
*                                                                               
LS180    OC    RQBSEQ,RQBSEQ       BATCH SEQ FILTER                             
         BNZ   LS200                YES, ALL DONE                               
*                                                                               
         B     LS100                                                            
*                                                                               
* READ ENTIRE FILE, NOW SORT AND PRINT TABLE *                                  
*                                                                               
LS200    DS   0H                                                                
*                                                                               
         CLI   MODE,PRINTREP       UNLESS PRINTING REPORT                       
         BE    *+6                  ALL DONE                                    
         DC    H'0'                                                             
*                                                                               
* PRINT JOB TOTALS *                                                            
*                                                                               
         L     R2,AAGYTABS                                                      
         L     R3,AAGYTAB                                                       
         SR    R5,R5                                                            
         USING AGYENTD,R3                                                       
LS210    OC    AGYSRT,AGYSRT                                                    
         BZ    LS214                                                            
         LA    R5,1(,R5)            COUNT OF ENTRIES                            
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4           BUILD ID RECORD KEY                          
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),AGYUID  FROM TABLE                                   
         L     R4,AIO2                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,(R4)                    
*                                                                               
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'02'        AGY ID                                       
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTDSCD,R6                                                        
         MVC   AGYSIG,CTDSC                                                     
*                                                                               
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'06'        AGY ID                                       
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTAGYEL,R6                                                       
         MVC   AGYPC,CTAGYID                                                    
*                                                                               
         LA    R3,AGYNEXT                                                       
         BCT   R2,LS210                                                         
         DROP  R4,R6                                                            
*                                                                               
LS214    DS   0H                                                                
         L     RF,=V(QSORT)                                                     
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,AAGYTAB,(R5),AGYENTL,L'AGYSRT,0                        
*                                                                               
         L     R4,AAGYTABS                                                      
         L     R3,AAGYTAB                                                       
*                                                                               
         LA    R1,ATOT                                                          
         SR    R2,R2                                                            
         BRAS  RE,UPDTOT                                                        
*                                                                               
         LA    R1,UTOT                                                          
         SR    R2,R2                                                            
         BRAS  RE,UPDTOT                                                        
                                                                                
         USING AGYENTD,R3                                                       
LS220    OC    AGYSRT,AGYSRT       AT END OF ENTRIES                            
         BZ    LS260                YEP                                         
*                                                                               
         MVC   SVAGYPC,AGYPC                                                    
         MVC   SVAGYSIG,AGYSIG                                                  
*                                                                               
         MVC   PAGYPC,AGYPC                                                     
         MVC   PAGYSIG,AGYSIG                                                   
         MVC   PAGYMED+1(1),AGYMED                                              
*                                                                               
         LA    R1,ATOT                                                          
         LA    R2,AGYCTS                                                        
         BRAS  RE,UPDTOT                                                        
*                                                                               
         LA    R1,UTOT                                                          
         LA    R2,AGYCTS                                                        
         BRAS  RE,UPDTOT                                                        
*                                                                               
         LA    R1,AGYCTS                                                        
         BRAS  RE,EDTTOT                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,AGYNEXT                                                       
*                                                                               
         TM    FTRFLAG2,FTRTODAY                                                
         BZ    LS242                                                            
*                                                                               
         CLC   SVAGYSIG,AGYSIG                                                  
         BE    LS242                                                            
*                                                                               
         MVC   PAGYSIG,SVAGYSIG                                                 
         MVC   PAGYMED(3),=C'TOT'                                               
*                                                                               
         LA    R1,UTOT                                                          
         BRAS  RE,EDTTOT                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R1,UTOT                                                          
         SR    R2,R2                                                            
         BRAS  RE,UPDTOT                                                        
*                                                                               
LS242    DS    0H                                                               
         CLC   SVAGYPC,AGYPC                                                    
         BE    LS250                                                            
*                                                                               
         MVC   PAGYPC,SVAGYPC                                                   
         MVC   PAGYSIG(6),=C'TOTALS'                                            
*                                                                               
         LA    R1,ATOT                                                          
         BRAS  RE,EDTTOT                                                        
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)     FORCE BLANK LINE TO PRINT                    
         B     LS246                                                            
*                                                                               
LS245    MVI   P,0                 FORCE BLANK LINE TO PRINT                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LS246    LA    R1,ATOT                                                          
         SR    R2,R2                                                            
         BRAS  RE,UPDTOT                                                        
*                                                                               
LS250    BCT   R4,LS220                                                         
*                                                                               
LS260    MVC   P+2(5),=C'TOTAL'                                                 
*                                                                               
         LA    R1,JTOT                                                          
         BRAS  RE,EDTTOT                                                        
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   PNDOL(20),=C'TOTAL BATCHES READ ='                               
         EDIT  (P8,BATSRD),(9,PNDOL+20),COMMAS=YES,ZERO=NOBLANK                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         CP    JBYPASSC,=P'0'      ANY BYPASSED CABLE STA                       
         BE    LS276                                                            
*                                                                               
         LA    R2,CABNOBCT                                                      
         L     R3,=A(CABNOBIL)                                                  
         MVC   P+3(33),=C'BYPASSED CABLE (NO BILL) STATIONS'                    
LS272    CP    5(3,R3),=P'0'                                                    
         BE    LS274                                                            
         MVC   P+40(5),0(R3)                                                    
         EDIT  (P3,5(R3)),(6,P+50),COMMAS=YES                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LS274    LA    R3,8(,R3)                                                        
*                                                                               
         BCT   R2,LS272                                                         
*                                                                               
         MVC   P+40(7),=CL7'*TOTAL*'                                            
         EDIT  (P8,JBYPASSC),(7,P+50),COMMAS=YES                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LS276    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         OC    ASRCTAB,ASRCTAB     IS THERE A RECAP                             
         BZ    LS288                                                            
*                                                                               
         L     R3,ASRCTAB                                                       
         USING SRCENTD,R3                                                       
         OC    SRCENT,SRCENT                                                    
         BZ    LS288                                                            
*                                                                               
         SR    R5,R5                                                            
LS280    OC    SRCENT,SRCENT                                                    
         BZ    LS282                                                            
         LA    R5,1(,R5)                                                        
         LA    R3,SRCNEXT                                                       
         C     R3,ASRCTABE                                                      
         BL    LS280                                                            
*                                                                               
         DC    H'0'                                                             
*                                                                               
LS282    DS   0H                                                                
         L     R3,ASRCTAB                                                       
*                                                                               
         L     RF,=V(QSORT)                                                     
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(R3),(R5),SRCENTL,4,0                                  
*                                                                               
         MVC   P+2(12),=C'SOURCE RECAP'                                         
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LS284    MVC   P+2(4),SRCSRC                                                    
         LA    R1,SRCCTS                                                        
         BRAS  RE,EDTTOT                                                        
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,SRCNEXT                                                       
         OC    SRCENT,SRCENT                                                    
         BNZ   LS284                                                            
         DROP  R3                                                               
*                                                                               
LS288    OC    AMEDTAB,AMEDTAB     IS THERE A RECAP                             
         BZ    LS296                                                            
         L     R3,AMEDTAB                                                       
         USING MEDENTD,R3                                                       
         OC    MEDENT,MEDENT                                                    
         BZ    LS296                                                            
*                                                                               
         MVC   P+2(11),=C'MEDIA RECAP'                                          
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LS290    MVC   P+2(1),MEDMED                                                    
         LA    R1,MEDCTS                                                        
         BRAS  RE,EDTTOT                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,MEDNEXT                                                       
         OC    MEDENT,MEDENT                                                    
         BNZ   LS290                                                            
         DROP  R3                                                               
LS296    B     EXIT                                                             
*                                                                               
***********************************************************************         
*   COUNT INVOICES (CONV, UNCONV, DEL) SPOTS, DOLLARS IN BATCH        *         
***********************************************************************         
*                                                                               
EZCOUNT  NTR1                                                                   
         L     R6,AIO2             SET EZBLOCK                                  
         USING EZBLOCKD,R6                                                      
         CLI   EZMODE,EZINVL       END OF INVOICE                               
         BE    EZC200                                                           
*                                                                               
         CLI   EZMODE,EZINVP       PROCESS INVOICE                              
         BNE   EZCX                                                             
*                                                                               
         TM    FTRFLAG2,FTRCAB     INCLUDE ALL CABLE                            
         BO    EZC120                                                           
*                                                                               
         LA    R0,CABNOBCT                                                      
         L     R1,=A(CABNOBIL)                                                  
         A     R1,RELO                                                          
         CLI   SRCESTA+3,C'.'                                                   
         BNE   *+8                                                              
         MVI   SRCESTA+3,C' '                                                   
*                                                                               
EZC110   CLC   0(5,R1),SRCESTA     THIS EXEMPT CALL LETTERS                     
         BE    EZC114                                                           
         LA    R1,L'CABNOBIL(,R1)                                               
         BCT   R0,EZC110                                                        
         B     EZC120                                                           
*                                                                               
EZC114   AP    JBYPASSC,=P'1'                                                   
         AP    5(3,R1),=P'1'                                                    
         B     EZC360                                                           
*                                                                               
EZC120   CLI   FTRCLTN,0           TEST HAVE CLIENT NAME FILTER                 
         BNH   EZC140                                                           
         ZIC   RF,FTRCLTNL         LENGTH - 1                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZIHADVN(0),FTRCLTN                                              
         BNE   EZC360                                                           
*                                                                               
EZC140   CLI   FTRMOS,0            TEST HAVE MONTH OF SERVICE FILTER            
         BNH   EZC150                                                           
         CLC   EZIHDMOS,FTRMOS                                                  
         BNE   EZC360                                                           
*                                                                               
EZC150   CLI   FTRPRDN,0           TEST HAVE PROD FILTER                        
         BNH   EZC160                                                           
         ZIC   RF,FTRPRDNL         TEST PRODUCT FILTER                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZIHPRDN(0),FTRPRDN                                              
         BNE   EZC360                                                           
*                                                                               
EZC160   CLI   FTRINVNO,0          TEST HAVE INVOICE FILTER                     
         BNH   EZC170                                                           
         ZIC   RF,FTRINVLN         GET INVNO LENGTH                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZIHINV(0),FTRINVNO                                              
         BNE   EZC360                                                           
*                                                                               
EZC170   CLI   FTRDATE,0           FILTER ON CONVERTED DATE                     
         BNH   EZC180                                                           
*                                                                               
         OC    EZIHCVDT,EZIHCVDT   IS THIS CONVERTED                            
         BZ    EZC360               NO                                          
*                                                                               
         CLI   FTRDATES,0          FILTER ON CONVERTED DATE RANGE               
         BNE   EZC174                                                           
*                                                                               
         CLC   EZIHCVDT,FTRDATE    FILTER ON EXACT DATE                         
         BNE   EZC360                                                           
         B     EZC180                                                           
EZC174   CLI   FTRDATES,C'+'       PLUS                                         
         BE    EZC176                                                           
         CLI   FTRDATES,C'-'       MINUS                                        
         BE    EZC178                                                           
         DC    H'0'                                                             
EZC176   CLC   EZIHCVDT,FTRDATE                                                 
         BL    EZC360                                                           
         B     EZC180                                                           
EZC178   CLC   EZIHCVDT,FTRDATE                                                 
         BH    EZC360                                                           
*                                                                               
EZC180   TM    FTRFLAG,FTRDONE     DONE ONLY                                    
         BZ    EZC181                                                           
*                                                                               
         TM    EZIHCVST,EZIHRCVQ   RECONVERT                                    
         BO    EZC360                                                           
         TM    EZIHCVST,EZIHCVQ    CONVERTED                                    
         BO    EZC190               YES                                         
         TM    EZIHCVST,EZIHCDEL   DELETED                                      
         BO    EZC190               YES                                         
         B     EZC360                                                           
*                                                                               
EZC181   TM    FTRFLAG,FTRUCVQ     UNCONVERTED ONLY                             
         BZ    EZC182               YES                                         
*                                                                               
* IF CONVERTED, DELETED, OR EVEN A RECONVERT (HAS CONVERT ON) BYPASS *          
*                                                                               
         TM    EZIHCVST,EZIHCVQ+EZIHCDEL   CONVERTED OR DELETED                 
         BNZ   EZC360                       YES, BYPASS                         
         B     EZC190                                                           
*                                                                               
EZC182   TM    FTRFLAG,FTRRCVQ     RECONVERT ONLY                               
         BZ    EZC184                                                           
         TM    EZIHCVST,EZIHRCVQ   RECONVERT                                    
         BO    EZC190               YES                                         
         B     EZC360                                                           
*                                                                               
EZC184   TM    FTRFLAG,FTRCVQ      CONVERTED ONLY                               
         BZ    EZC186                                                           
         TM    EZIHCVST,EZIHRCVQ   RECONVERT                                    
         BO    EZC360                                                           
         TM    EZIHCVST,EZIHCVQ    CONVERTED                                    
         BO    EZC190               YES                                         
         B     EZC360                                                           
*                                                                               
EZC186   TM    FTRFLAG,FTROVR      OVERRIDES ONLY                               
         BZ    EZC188                                                           
         TM    EZIHCVST,EZIHCOVR   OVERRIDE                                     
         BO    EZC190               YES                                         
         B     EZC360                                                           
*                                                                               
EZC188   TM    FTRFLAG,FTRDEL      DELETES ONLY                                 
         BZ    EZC190                                                           
         TM    EZIHCVST,EZIHCDEL   DELETED                                      
         BZ    EZC360               NO, BYPASS                                  
*                                                                               
EZC190   MVI   PROCESS,C'Y'                                                     
         B     EZCX                                                             
*                                                                               
* HAVE FILTERED AT INVOICE HEADER, NOW HAVE HEADER AND TOTAL *                  
*                                                                               
EZC200   DS   0H                                                                
         CLI   PROCESS,C'Y'                                                     
         BNE   EZCX                                                             
*                                                                               
         MVC   SVINVSEQ,EZRECSEQ     SAVE INVOICE HEADER SEQ                    
         MVC   SVSNMED,EZSNMED       SAVE MEDIA                                 
         MVC   SVSNBND,EZSNBND         BAND                                     
*                                                                               
         TM    EZIHCVST,EZIHCVQ    TEST CONVERTED                               
         BZ    *+14                 NO                                          
         AP    TINVC,=P'1'                                                      
         B     *+10                                                             
         AP    TINVU,=P'1'                                                      
*                                                                               
         TM    EZIHCVST,EZIHCOVR   TEST OVERRIDE CLT/PRD/EST                    
         BZ    *+10                                                             
         AP    TINVO,=P'1'                                                      
*                                                                               
         TM    EZIHCVST,EZIHCDEL   DELETED                                      
         BZ    *+10                                                             
         AP    TINVD,=P'1'                                                      
*                                                                               
         ICM   R1,15,EZIHTSPN                                                   
         CVD   R1,DUB                                                           
         AP    TINVSPT,DUB                                                      
*                                                                               
         ICM   R1,15,EZITBDUE                                                   
         CVD   R1,DUB                                                           
         AP    TINVNDOL,DUB                                                     
*                                                                               
         ICM   R1,15,EZITBACT                                                   
         CVD   R1,DUB                                                           
         AP    TINVGDOL,DUB                                                     
*                                                                               
         AP    TINV,=P'1'                                                       
*                                                                               
         B     EZCX                                                             
*                                                                               
* THIS IS USED TO BYPASS THIS INVOICE - NOT IN TOTALS                           
*                                                                               
EZC360   MVI   EZMODE,EZINVL       SKIP TO NEXT INVOICE                         
         MVI   PROCESS,C'N'                                                     
*                                                                               
EZCX     B     EXIT                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
SIZERR   L     R1,=A(SIZMS)                                                     
         B     ERREXIT                                                          
MAXIOSER L     R1,=A(MAXIOSMS)                                                  
         B     ERREXIT                                                          
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
         DC    AL1(L'MAXIOSMS-1)                                                
MAXIOSMS DC    C'BE MORE SPECIFIC, TOO MUCH DATA FOR ONLINE *'                  
         DC    AL1(L'INVUIDMS-1)                                                
INVUIDMS DC    C'INVALID USER ID *'                                             
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
         DC    AL1(L'SIZMS-1)                                                   
SIZMS    DC    C'TOO BIG TO RUN ONLINE *'                                       
*                                                                               
HEADING  SSPEC H1,3,REQUESTOR                                                   
         SSPEC H2,3,C'EASI'                                                     
         SSPEC H1,48,C'INVOICES BY AGENCY'                                      
         SSPEC H2,48,C'------------------'                                      
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         SSPEC H8,1,C'AGENCY'                                                   
         SSPEC H9,1,C'------'                                                   
         SSPEC H8,13,C'MED'                                                     
         SSPEC H9,13,C'---'                                                     
         SSPEC H8,18,C'INVOICES'                                                
         SSPEC H9,18,C'--------'                                                
         SSPEC H8,29,C'CONVERTD'                                                
         SSPEC H9,29,C'--------'                                                
         SSPEC H8,39,C'OVERRIDES'                                               
         SSPEC H9,39,C'---------'                                               
         SSPEC H8,53,C'UNCONV'                                                  
         SSPEC H9,53,C'------'                                                  
         SSPEC H8,63,C'DELETED'                                                 
         SSPEC H9,63,C'-------'                                                 
         SSPEC H8,76,C'SPOTS'                                                   
         SSPEC H9,76,C'-----'                                                   
         SSPEC H8,91,C'NET DOLLARS'                                             
         SSPEC H9,91,C'-----------'                                             
         SSPEC H8,109,C'GROSS DOLLARS'                                          
         SSPEC H9,109,C'-------------'                                          
         SSPEC H8,126,C'BATCHES'                                                
         SSPEC H9,126,C'-------'                                                
         DC    X'00'                                                            
*                                                                               
* TABLE OF BYPASSED CABLE STATIONS                                              
*                                                                               
CABNOBIL DS   0CL8                                                              
         DC    CL5'AEN C',PL3'0'                                                
         DC    CL5'BET C',PL3'0'                                                
         DC    CL5'CMT C',PL3'0'                                                
         DC    CL5'CNBCC',PL3'0'                                                
         DC    CL5'ENT C',PL3'0'                                                
         DC    CL5'EOP C',PL3'0'                                                
         DC    CL5'ESPAC',PL3'0'                                                
         DC    CL5'ESPCC',PL3'0'                                                
         DC    CL5'ESPEC',PL3'0'                                                
         DC    CL5'ESPGC',PL3'0'                                                
         DC    CL5'ESPHC',PL3'0'                                                
         DC    CL5'ESPNC',PL3'0'                                                
         DC    CL5'FAM C',PL3'0'                                                
         DC    CL5'FIT C',PL3'0'                                                
         DC    CL5'HGTVC',PL3'0'                                                
         DC    CL5'MSNBC',PL3'0'                                                
         DC    CL5'MTV C',PL3'0'                                                
         DC    CL5'NAN C',PL3'0'                                                
         DC    CL5'NIC C',PL3'0'                                                
         DC    CL5'SCI C',PL3'0'                                                
         DC    CL5'THC C',PL3'0'                                                
         DC    CL5'TNN C',PL3'0'                                                
         DC    CL5'TVFNC',PL3'0'                                                
         DC    CL5'USANC',PL3'0'                                                
         DC    CL5'VH1 C',PL3'0'                                                
         DC    CL5'WGN C',PL3'0'                                                
CABNOBCT EQU   (*-CABNOBIL)/8                                                   
*                                                                               
         DROP  R7,RB,RC                                                         
*                                                                               
***********************************************************************         
*   HEADHOOK ROUTINE FOR REPORT                                       *         
***********************************************************************         
*                                                                               
         DS    0H                                                               
HDHK     NMOD1 0,**HDHK**                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
         GOTO1 DATCON,DMCB,(2,RQDTSTR),(5,H3+48)                                
         MVI   H3+56,C'-'                                                       
         GOTO1 (RF),(R1),(2,RQDTEND),(5,H3+57)                                  
         CLI   FTRMOS,0            TEST HAVE MONTH OF SERVICE FILTER            
         BNH   HDHK10                                                           
         MVC   H4+30(4),=C'MOS='                                                
         MVC   H4+34(6),FTRMOS                                                  
HDHK10   CLI   FTRSRCE,0           TEST HAVE SOURCE FILTER                      
         BNH   HDHK20                                                           
         MVC   H4+45(7),=C'SOURCE='                                             
         MVC   H4+52(4),FTRSRCE                                                 
HDHK20   CLI   FTRMEDIA,0          TEST HAVE SOURCE FILTER                      
         BNH   HDHKX                                                            
         MVC   H4+58(6),=C'MEDIA='                                              
         MVC   H4+64(1),FTRMEDIA                                                
HDHKX    XIT1                                                                   
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
         CLI   8(R2),C'?'          HELP                                         
         BE    VFTRHLP             YES                                          
         CLI   5(R2),4                                                          
         BNH   VFTR02                                                           
         LA    R1,3                                                             
         B     VFTR04                                                           
*                                                                               
VFTR02   ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
*                                                                               
VFTR04   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL4'HELP'                                               
         BE    VFTRHLP                                                          
*                                                                               
         LA    R0,25               NON-STANDARD LENGTH                          
         MVI   BYTE,1                                                           
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
         GOTO1 SCANNER,DMCB,((R0),(R2)),(9,(R4)),0                              
         CLI   4(R1),0                                                          
         BE    MISSERRA             SCANNER DIDN'T FIND ANYTHING                
         SR    R3,R3                                                            
         ICM   R3,1,DMCB+4         GET NUMBER OF BLOCKS                         
*                                                                               
VFTR10   ZIC   R1,0(R4)            GET LENGTH                                   
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
         BNE   VFTR20                                                           
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),WORK                                        
         OC    DMCB(4),DMCB        WAS DATE VALID                               
         BZ    BADATEA              NO                                          
         GOTO1 DATCON,DMCB,(0,WORK),(1,FTRDATE)                                 
         MVC   FTRDATES,HOLDSIGN                                                
         B     VFTR500                                                          
*                                                                               
VFTR20   EX    R1,VFTRCLCB         CLIENT CODE (CC)                             
         BNE   VFTR22                                                           
         CLI   1(R4),2                                                          
         BL    CCLENER                                                          
         CLI   1(R4),3                                                          
         BH    CCLENER                                                          
         MVC   FTRQCLT,22(R4)                                                   
         B     VFTR500                                                          
*                                                                               
VFTR22   EX    R1,VFTRCLCC         CLIENT NAME (CN)                             
         BNE   VFTR24                                                           
         MVC   FTRCLTN,22(R4)                                                   
         ZIC   R0,1(R4)                                                         
         BCTR  R0,0                                                             
         STC   R0,FTRCLTNL                                                      
         B     VFTR500                                                          
*                                                                               
VFTR24   EX    R1,VFTRCLCD         PRODUCT CODE (PC)                            
         BNE   VFTR26                                                           
         CLI   1(R4),2                                                          
         BL    PCLENER                                                          
         CLI   1(R4),3                                                          
         BH    PCLENER                                                          
         MVC   FTRQPRD,22(R4)                                                   
         B     VFTR500                                                          
*                                                                               
VFTR26   EX    R1,VFTRCLCE         PRODUCT NAME (PN)                            
         BNE   VFTR30                                                           
         MVC   FTRPRDN,22(R4)                                                   
         ZIC   R0,1(R4)                                                         
         BCTR  R0,0                                                             
         STC   R0,FTRPRDNL                                                      
         B     VFTR500                                                          
*                                                                               
VFTR30   EX    R1,VFTRCLCF         CONVERTED (CONV)                             
         BNE   VFTR32                                                           
         TM    FTRFLAG,FTRUCVQ                                                  
         BNZ   VFTRERRC                                                         
         OI    FTRFLAG,FTRCVQ                                                   
         B     VFTR500                                                          
*                                                                               
VFTR32   EX    R1,VFTRCLCG         RECONVERTS (RECONV)                          
         BNE   VFTR34                                                           
         OI    FTRFLAG,FTRRCVQ                                                  
         B     VFTR500                                                          
*                                                                               
VFTR34   EX    R1,VFTRCLCH         UNCONVERTED (UNCONV)                         
         BNE   VFTR36                                                           
         TM    FTRFLAG,FTRCVQ                                                   
         BNZ   VFTRERRC                                                         
         OI    FTRFLAG,FTRUCVQ                                                  
         B     VFTR500                                                          
*                                                                               
VFTR36   EX    R1,VFTRCLCO         OVERRIDES                                    
         BNE   VFTR40                                                           
         OI    FTRFLAG,FTROVR                                                   
         B     VFTR500                                                          
*                                                                               
VFTR40   EX    R1,VFTRCLCI         INVOICE                                      
         BNE   VFTR50                                                           
         CLI   1(R4),10                                                         
         BH    INVLENER                                                         
         MVC   FTRINVNO,22(R4)                                                  
         ZIC   R0,1(R4)                                                         
         BCTR  R0,0                                                             
         STC   R0,FTRINVLN                                                      
         B     VFTR500                                                          
*                                                                               
VFTR50   EX    R1,VFTRCLCJ         SOURCE                                       
         BNE   VFTR54                                                           
         CLI   1(R4),4                                                          
         BH    SRCLENER                                                         
         MVC   FTRSRCE,22(R4)                                                   
         B     VFTR500                                                          
*                                                                               
VFTR54   EX    R1,VFTRCLCK         DONE (MEANS CONV/DEL INC KEEP)               
         BNE   VFTR60                                                           
         OI    FTRFLAG,FTRDONE                                                  
         B     VFTR500                                                          
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
VFTR64   GOTO1 DATCON,DMCB,(0,WORK),(1,FTRBSDT)                                 
         GOTO1 (RF),(R1),(0,WORK+6),(1,FTRBEDT)                                 
         B     VFTR500                                                          
*                                                                               
VFTR66   EX    R1,VFTRCLCM         MEDIA                                        
         BNE   VFTR69                                                           
         LA    R5,22(,R4)                                                       
         CLI   0(R5),C'T'          TV                                           
         BE    VFTR68                                                           
         CLI   0(R5),C'R'          RADIO                                        
         BE    VFTR68                                                           
         CLI   0(R5),C'C'          CABLE                                        
         BE    VFTR68                                                           
         CLI   0(R5),C'S'          SYNDICATION                                  
         BE    VFTR68                                                           
         CLI   0(R5),C'N'          NETWORK                                      
         BE    VFTR68                                                           
         CLI   0(R5),C'X'          NETWORK RADIO                                
         BNE   VFTRMDER                                                         
VFTR68   MVC   FTRMEDIA,0(R5)                                                   
         B     VFTR500                                                          
*                                                                               
VFTR69   EX    R1,VFTRCLCN         ALL (INCLUDE SJR/IRNY/WWNY)                  
         BNE   VFTR70                                                           
         OI    FTRFLAG,FTRALL                                                   
         B     VFTR500                                                          
*                                                                               
VFTR70   EX    R1,VFTRCLCT         DELETE                                       
         BNE   VFTR72                                                           
         OI    FTRFLAG,FTRDEL                                                   
         B     VFTR500                                                          
*                                                                               
VFTR72   EX    R1,VFTRCLCQ         CAB - INCLUDE BYPASSED CABLE STA             
         BNE   VFTR74                                                           
         OI    FTRFLAG2,FTRCAB                                                  
         B     VFTR500                                                          
*                                                                               
VFTR74   EX    R1,VFTRCLCU         USER ID                                      
         BNE   VFTR80                                                           
         CLI   OFFLINE,C'Y'                                                     
         BE    *+12                                                             
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   VFTRHLP                                                          
         CLI   1(R4),8                                                          
         BH    USIDLNER                                                         
         MVC   FUID,22(R4)                                                      
*                                                                               
         MVI   FUIDNUM,X'FF'       ALL IDS                                      
         CLC   FUID(3),=C'ALL'                                                  
         BE    VFTR500                                                          
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
         USING CTIKEY,R6           BUILD ID RECORD KEY                          
*                                                                               
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,VFNEXT2                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FUIDNUM,2(R6)       BINARY USER ID (ORIGIN)                      
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(2),2(R6)                                                    
*                                                                               
         LH    R5,=AL2(WRKFBUFR-SYSD)                                           
         AR    R5,R9                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GFILE'),=C'WRKFIL',WORK,AIO3,(R5)             
*                                                                               
         LA    R1,WORK                                                          
         MVC   EASIWK,UKUSRINF-UKRECD(R1)                                       
*                                                                               
         B     VFTR500                                                          
         DROP  R6                                                               
*                                                                               
VFTR80   EX    R1,VFTRCLCP         MOS - MONTH OF SERVICE                       
         BNE   VFTR82                                                           
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),WORK                                        
         ICM   RE,15,DMCB          WAS MO/DA/YR DATE VALID                      
         BNZ   MOSERR               YES, ERROR                                  
*                                                                               
         GOTO1 (RF),(R1),(2,(R5)),WORK                                          
         ICM   RE,15,DMCB          WAS MO/YR DATE VALID                         
         BZ    MOSERR               NO, ERROR                                   
         GOTO1 DATCON,DMCB,(0,WORK),(6,FTRMOS)                                  
         B     VFTR500                                                          
*                                                                               
VFTR82   EX    R1,VFTRCLCX         TEST - ONLY READ 500 BATCHES                 
         BNE   VFTR84                                                           
         OI    FTRFLAG,FTRTEST                                                  
         B     VFTR500                                                          
*                                                                               
VFTR84   EX    R1,VFTRCLCV         EZMOD TRACE                                  
         BNE   VFTR89                                                           
         CLI   1(R4),1             MUST HAVE 1 CHARACTER ENTERED                
         BNE   VFTRERR                                                          
         LA    R0,6                                                             
         LA    R1,=C'AEFMNR'                                                    
         LA    RF,=X'FF20E0600070'                                              
*                                                                               
* A=EVERYTHING, E=ERRORS, M=MODES  F=FIELDS, R=ADDED RECORDS, N=NONE            
*                           ERRORS   ERRORS    FIELDS                           
*                                    MODES     ERRORS                           
*                                              MODES                            
*                                                                               
VFTR86   CLC   0(1,R1),22(R4)                                                   
         BE    VFTR88                                                           
         LA    R1,1(,R1)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,VFTR86                                                        
         B     VFTRERR                                                          
*                                                                               
VFTR88   MVC   FTRTRACE,0(RF)                                                   
         B     VFTR500                                                          
*                                                                               
VFTR89   EX    R1,VFTRCLCW         PRINT KEYS READ                              
         BNE   VFTR100                                                          
         MVI   FTRKEY,C'Y'         PRINT KEYS READ                              
         B     VFTR500                                                          
*                                                                               
VFTR100  DS    0H                                                               
         EX    R1,VFTRCLCR         TODAY                                        
         BNE   VFTR120                                                          
         GOTO1 DATCON,DMCB,(3,BTODAY),(2,RQDTSTR)                               
         MVC   RQDTEND,RQDTSTR                                                  
         OI    FTRFLAG2,FTRTODAY                                                
         B     VFTR500                                                          
*                                                                               
VFTR120  DS    0H                                                               
         EX    R1,VFTRCLCS         WORKER FILE                                  
         BNE   VFTRERR                                                          
         MVI   FTRWKR,X'00'                                                     
         CLI   22(R4),C'A'                                                      
         BL    VFTRERR                                                          
         CLI   22(R4),C'F'                                                      
         BNH   VFTR122                                                          
         CLI   22(R4),C'0'                                                      
         BL    VFTRERR                                                          
         CLI   22(R4),C'9'                                                      
         BH    VFTRERR                                                          
*                                                                               
VFTR122  DS    0H                                                               
         MVC   FTRWKR,22(R4)                                                    
         B     VFTR500                                                          
*                                                                               
VFTR500  ZIC   RE,BYTE             UP FIELD NUMBER CTR                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         LA    R4,47(R4)           NOTE- NON-STANDARD LENGTH                    
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
*                                                                               
         CLI   FUIDNUM,X'FF'       ALL IDS                                      
         BNE   *+8                                                              
         MVI   EASIWK+4,C'1'                WRKF1                               
*                                                                               
         CLI   FTRWKR,X'00'                                                     
         BE    *+10                                                             
         MVC   EASIWK+4(1),FTRWKR                                               
*                                                                               
VFTRX    XIT1                                                                   
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
VFTRCLCN CLC   12(0,R4),=CL4'ALL'                                               
VFTRCLCO CLC   12(0,R4),=CL9'OVERRIDE'                                          
VFTRCLCP CLC   12(0,R4),=CL4'MOS'                                               
VFTRCLCQ CLC   12(0,R4),=CL5'CABLE'                                             
VFTRCLCR CLC   12(0,R4),=CL5'TODAY'                                             
VFTRCLCS CLC   12(0,R4),=CL5'WRKF'                                              
VFTRCLCT CLC   12(0,R4),=CL7'DELETE'                                            
VFTRCLCU CLC   12(0,R4),=CL5'USER'                                              
VFTRCLCV CLC   12(0,R4),=CL7'TRACE '                                            
VFTRCLCW CLC   12(0,R4),=CL3'KEY'                                               
VFTRCLCX CLC   12(0,R4),=CL5'TEST '                                             
*                                                                               
VFGETEL  LA    R6,24(R6)           POINT TO FIRST ELEMENT                       
         B     VFNEXT2                                                          
*                                                                               
VFNEXTEL CLI   0(R6),0                                                          
         BE    VFNEXTX                                                          
         SR    RF,RF                                                            
         ICM   RF,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
VFNEXT2  CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
         B     VFNEXTEL                                                         
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
VFTRHLP  L     R1,=A(FTRHELP)                                                   
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
         DC    AL1(L'FTRTIMS-1)                                                 
FTRTIMS  DC    CL60'** ERROR * FILTERS NOT ALLOWED FOR REPORT *'                
         DC    AL1(L'UIDLENMS-1)                                                
UIDLENMS DC    C'* ERROR * USERID CAN''T BE MORE THAN 8 CHARACTERS *'           
         DC    AL1(L'SRCLENMS-1)                                                
SRCLENMS DC    C'* ERROR * SOURCE CAN''T BE MORE THAN 4 CHARACTERS *'           
         DC    AL1(L'MOSERMS-1)                                                 
MOSERMS  DC    C'* ERROR * ENTER MOS MO/YR OR MOMYR *'                          
         LTORG                                                                  
NUMLINS  EQU   16                                                               
*                                                                               
*                                                                               
* R1 EXPECTED TO ADDRESS LIST OF COUNTERS TO BE UPDATED                         
* R2 EXPECTED TO ADDRESS LIST OF COUNTERS TO BE ADDED TO COUNTERS IN R1         
* IF R2 IS ZERO, COUNTERS IN R1 ARE ZAPPED                                      
UPDTOT   NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,CTRNQ                                                         
*                                                                               
UPDT10   LTR   R2,R2                                                            
         BZ    *+14                                                             
         AP    0(8,R1),0(8,R2)                                                  
         B     *+10                                                             
         ZAP   0(8,R1),=P'0'                                                    
*                                                                               
         LA    R1,8(R1)                                                         
         LTR   R2,R2                                                            
         BZ    *+8                                                              
         LA    R2,8(R2)                                                         
         BCT   R0,UPDT10                                                        
*                                                                               
UPDTX    J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
* R1 EXPECTED TO ADDRESS LIST OF COUNTERS TO BE EDITED                          
EDTTOT   NTR1  BASE=*,LABEL=*                                                   
         LHI   R4,CTRNQ-3                                                       
         LR    R5,R1                                                            
         LA    R6,PINVS                                                         
*                                                                               
         MVC   WORK(21),=X'40202020206B2020206B2020206B2020214B202060'          
         ZAP   DUB,CTINNDOL-CTRSD(8,R5)                                         
         ED    WORK(21),DUB                                                     
         MVC   PNDOL,WORK+3     PRINT AMOUNT                                    
*                                                                               
         MVC   WORK(21),=X'40202020206B2020206B2020206B2020214B202060'          
         ZAP   DUB,CTINGDOL-CTRSD(8,R5)                                         
         ED    WORK(21),DUB                                                     
         MVC   PGDOL,WORK+3     PRINT AMOUNT                                    
*                                                                               
*&&DO                                                                           
         EDIT  (P8,CTINNDOL-CTRSD(R5)),(16,PNDOL),2,                   X        
               COMMAS=YES,MINUS=YES                                             
         EDIT  (P8,CTINGDOL-CTRSD(R5)),(16,PGDOL),2,                   X        
               COMMAS=YES,MINUS=YES                                             
*&&                                                                             
         EDIT  (P8,CTBAT-CTRSD(R5)),(8,PBAT),COMMAS=YES                         
*                                                                               
EDTT10   EDIT  (P8,0(R5)),(10,0(R6)),COMMAS=YES                                 
         LA    R5,8(,R5)                                                        
         LA    R6,11(,R6)                                                       
         BCT   R4,EDTT10                                                        
*                                                                               
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* SPEZFFFD                                                                      
* EZBLOCK                                                                       
* CTGENFILE                                                                     
       ++INCLUDE SPGENEZ                                                        
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE EZBLOCK                                                        
       ++INCLUDE SPEZFFFD                                                       
         PRINT ON                                                               
*                                                                               
         ORG   CONTAGH                                                          
* SPEZFE8D                                                                      
       ++INCLUDE SPEZFE4D                                                       
         PRINT OFF                                                              
*                                                                               
* DDGENTWA                                                                      
       ++INCLUDE DDGENTWA                                                       
*                                                                               
* DMWRKFD                                                                       
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
* CTGENFILE                                                                     
       ++INCLUDE CTGENFILE                                                      
*                                                                               
*SPEZFWORKD                                                                     
       ++INCLUDE SPEZFWORKD                                                     
         PRINT ON                                                               
* DSECT FOR THIS PROGRAM *                                                      
*                                                                               
CTRSD    DSECT                                                                  
CTINV    DS    PL8                 INVOICES                                     
CTINVC   DS    PL8                 CONVERTED                                    
CTINVO   DS    PL8                 OVERRIDE                                     
CTINVU   DS    PL8                 UNCONV                                       
CTINVD   DS    PL8                 DEL                                          
CTINVSPT DS    PL8                 SPOTS                                        
CTINNDOL DS    PL8                                                              
CTINGDOL DS    PL8                                                              
CTBAT    DS    PL8                 BATCHES                                      
CTRNQ    EQU   (*-CTRSD)/8                                                      
*                                                                               
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
DUB2     DS    D                                                                
RELO     DS    A                                                                
SVRC     DS    A                                                                
AAGYTAB  DS    A                                                                
AAGYTABS DS    F                                                                
ASRCTAB  DS    F                   ADDRESS OF SOURCE RECAP TABLE                
ASRCTABE DS    F                    END                                         
AMEDTAB  DS    F                   ADDRESS OF MEDIA RECAP TABLE                 
AMEDTABE DS    F                    END                                         
SVITBACT DS    F                                                                
SVITBDUE DS    F                                                                
*                                                                               
*                                                                               
* COUNTS OF INDEX READS AND WORKER REC READS  *                                 
* ONLY A COUNT OF CALLS, DOESN'T INCLUDE REAL *                                 
* INDEX READS OR SECOND RECORD GETS IN EZMOD  *                                 
*                                                                               
BATSRD   DS    PL8                                                              
UTOT     DS    (8*CTRNQ)X             UID TOTALS                                
ATOT     DS    (8*CTRNQ)X             AGENCY TOTALS                             
JTOT     DS    (8*CTRNQ)X             JOB TOTALS                                
JBYPASSC DS    PL8                                                              
*                                                                               
CURWKIXD DS    CL42                CURRENT WORKER INDEX                         
*                                                                               
SVWCMNT  DS    0CL16               SAVED WORKER RECORD COMMENT AREA             
SVWCSTAT DS    XL1                 STATUS                                       
*              X'40'               ALL INV CONVERTED  OR DELETED                
SVWCPDAT DS    XL3                 PROCESSED DATE                               
SVWCPTIM DS    XL2                 PROCESSED TIME-FORCED 0100                   
SVWCICNT DS    XL2                 INV CT - NOT USED YET                        
SVWCPCNT DS    XL2                 PROCESSED INV CT - NOT USED YET              
SVWCSRCE DS    CL4                                                              
         DS    CL2                 SPARE                                        
*                                                                               
RQSTA    DS    CL5                 REQUESTED STATION                            
RQDTES   DS   0XL4                           BATCH DATES                        
RQDTSTR  DS    XL2                           BATCH DATE START                   
RQDTEND  DS    XL2                           BATCH DATE END                     
RQSEQ    DS    CL4                           BATCH SEQ                          
RQBSEQ   DS    XL2                                                              
*                                                                               
* INFO ON LAST BATCH PROCESSED - *                                              
*                                                                               
*SVCIADDR DS    XL2                                                             
*                                                                               
* INFO ON LAST INVOICE PROCESSED - *                                            
*                                                                               
SVWKFILN DS    XL2                         SEQUENCE NUMBER                      
SVWKSTAT DS    XL1                         STATUS                               
SVWKDTEC DS    XL3                         DATE                                 
SVINVSEQ DS    XL2                         INVOICE SEQ WITHIN BATCH             
*                                                                               
SVWEZIND DS    CL42                                                             
PRNTSTA  DS    CL7                 CURRENT BATCH STATION                        
*                                                                               
SVSNMED  DS    CL1                 SAVED EZSNMED FROM EZBLOCK                   
SVSNBND  DS    CL1                 SAVED EZSNBND FROM EZBLOCK                   
SVUID    DS    XL2                                                              
SVAGYPC  DS    CL2                                                              
SVAGYSIG DS    CL8                                                              
SVSRCE   DS    CL4                                                              
PROCESS  DS    CL1                                                              
*                                                                               
ORIGSTA  DS    CL5                 CURRENT BATCH BEFORE CHANGES                 
*                                                                               
FILTERS  DS    0CL(FILTERND-FUID)                                               
FUID     DS    CL8                                                              
FUIDNUM  DS    XL2                 USER ID (DDS TERMS ONLY)                     
FTRQCLT  DS    CL3                 CLIENT CODE                                  
FTRCLTN  DS    CL25                EASI CLIENT NAME                             
FTRCLTNL DS    XL1                                  LENGTH                      
FTRQPRD  DS    CL3                 PRODUCT CODE                                 
FTRPRDN  DS    CL25                PRODUCT NAME                                 
FTRPRDNL DS    XL1                                  LENGTH                      
FTRINVNO DS    CL10                INVOICE NO                                   
FTRINVLN DS    XL1                            LENGTH                            
FTRSRCE  DS    CL4                                                              
FTRDATE  DS    XL3                 ACTIVITY DATE                                
FTRDATES DS    CL1                                                              
FTRBSDT  DS    XL3                 BATCH DATE START                             
FTRBEDT  DS    XL3                 BATCH DATE END                               
FTRMOS   DS    CL6                 MONTH OF SERVICE DATE                        
FTRMEDIA DS    CL1                 MEDIA                                        
FTRTRACE DS    CL1                 TRACE EZMOD                                  
FTRKEY   DS    CL1                 PRINT KEYS READ                              
FTRWKR   DS    X                                                                
FTRFLAG  DS    XL1                                                              
FTRUCVQ  EQU   X'80'               FILTER ON UNCONVERTED                        
FTRCVQ   EQU   X'40'                         CONVERTED                          
FTRRCVQ  EQU   X'20'                         RECONVERTS                         
FTROVR   EQU   X'10'                         OVERRIDES                          
FTRDEL   EQU   X'08'                         DELETES                            
FTRDONE  EQU   X'04'                         DONE - KEEP STATUS FILES           
FTRTEST  EQU   X'02'               ONLY READ 500 BATCHES                        
FTRALL   EQU   X'01'               RD ALL (SJR/IRNY/WWNY NOT EXCLUDED)          
FTRFLAG2 DS    XL1                                                              
FTRCAB   EQU   X'80'               COUNT CABLE TOO!                             
FTRTODAY EQU   X'40'               TODAY                                        
HOLDSIGN DS    CL1                                                              
FILTERND EQU   *                                                                
*                                                                               
TINVCTS  DS    0F                                                               
TINV     DS    PL8                                                              
TINVC    DS    PL8                                                              
TINVO    DS    PL8                                                              
TINVU    DS    PL8                                                              
TINVD    DS    PL8                                                              
TINVSPT  DS    PL8                                                              
TINVNDOL DS    PL8                                                              
TINVGDOL DS    PL8                                                              
TBAT     DS    PL8                                                              
*                                                                               
WRKFBUFR DS    0D                                                               
         DS    XL14336                                                          
WRKFEND  DS    XL1                                                              
*                                                                               
*                                                                               
AGYENTD  DSECT                                                                  
AGYSRT   DS    0CL13                                                            
AGYPC    DS    CL2                                                              
AGYSIG   DS    CL8                                                              
AGYMED   DS    CL1                                                              
AGYUID   DS    XL2                                                              
AGYCTS   DS    (8*CTRNQ)X                                                       
AGYNEXT  EQU   *                                                                
AGYENTL  EQU   *-AGYSRT                                                         
*                                                                               
*                                                                               
*                                                                               
SRCENTD  DSECT                                                                  
SRCENT   DS   0CL(SRCNEXT-SRCSRC)                                               
SRCSRC   DS    CL4                                                              
SRCCTS   DS    (8*CTRNQ)X                                                       
SRCNEXT  EQU   *                                                                
SRCENTL  EQU   *-SRCENTD                                                        
*                                                                               
*                                                                               
*                                                                               
MEDENTD  DSECT                                                                  
MEDENT   DS   0CL(MEDNEXT-MEDMED)                                               
MEDMED   DS    CL1                                                              
MEDCTS   DS    (8*CTRNQ)X                                                       
MEDNEXT  EQU   *                                                                
MEDENTL  EQU   *-MEDENTD                                                        
*                                                                               
*                                                                               
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
PAGYPC   DS    CL2                                                              
         DS    CL1                                                              
PAGYSIG  DS    CL8                                                              
         DS    CL1                                                              
PAGYMED  DS    CL3                                                              
         DS    CL2                                                              
PINVS    DS    CL10                TOTAL INVOICES                               
         DS    CL1                                                              
PINVC    DS    CL10                CONVERTED                                    
         DS    CL1                                                              
PINVO    DS    CL10                OVERRIDES                                    
         DS    CL1                                                              
PINVU    DS    CL10                UNCONVERTED                                  
         DS    CL1                                                              
PINVD    DS    CL10                DELETED                                      
         DS    CL1                                                              
PSPTS    DS    CL10                                                             
         DS    CL2                                                              
PNDOL    DS    CL18                                                             
         DS    CL2                                                              
PGDOL    DS    CL18                                                             
         DS    CL2                                                              
PBAT     DS    CL8                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049SPEZF13   05/17/07'                                      
         END                                                                    
