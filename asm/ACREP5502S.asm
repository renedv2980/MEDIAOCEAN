*          DATA SET ACREP5502S AT LEVEL 097 AS OF 10/26/00                      
*PHASE AC5502A,+0                                                               
*INCLUDE ACLIST                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE NUMTOLET                                                               
*INCLUDE PUBEDIT                                                                
*INCLUDE POWWOW                                                                 
*INCLUDE ADFORM                                                                 
CHECKS   TITLE 'ACREP5502 - AC5502'                                             
         PRINT NOGEN                                                            
AC5502   CSECT                                                                  
         NMOD1 0,*AC5502*,R9,R8,R7,R6                                           
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         L     RC,=A(LWS)                                                       
         USING AC55D,RC                                                         
*                                                                               
         MVI   FCRESET,C'Y'                                                     
         CLI   MODE,RUNFRST                                                     
         BE    RNF00                                                            
         CLI   MODE,REQFRST                                                     
         BE    RQF00                                                            
         CLI   MODE,LEDGFRST                                                    
         BE    LGF00                                                            
         CLI   MODE,PROCACC                                                     
         BE    PAC00                                                            
         CLI   MODE,PROCTRND                                                    
         BE    PTD00                                                            
         CLI   MODE,PROCTRNS                                                    
         BE    PTN00                                                            
         CLI   MODE,ACCLAST                                                     
         BE    ACL00                                                            
         CLI   MODE,REQLAST                                                     
         BE    RQL00                                                            
         CLI   MODE,RUNLAST                                                     
         BE    RNL00                                                            
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
         SPACE 1                                                                
RNF00    L     RE,=A(RELOTAB)      RELOCATE A AND V TYPES                       
         LA    R1,ATYPES                                                        
*                                                                               
RNF02    L     RF,0(RE)                                                         
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RNF02                                                            
*                                                                               
         SR    R0,R0               GET MAIN STORAGE                             
         LA    R0,100                                                           
         L     R4,AMAINTAB                                                      
*                                                                               
RNF03    CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    RNF05                                                            
         A     R0,8(R4)            ADD THE LENGTH OF EACH TABLE                 
         LA    R4,L'MAINTAB(R4)                                                 
         B     RNF03                                                            
*                                                                               
RNF05    ST    R0,MAINLEN          SAVE LENGTH OF TABLE                         
         GETMAIN  R,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,MAINBGN          START OF AREA                                
         LR    R2,R1               CLEAR STORAGE                                
         L     R3,MAINLEN                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R2,RE                                                            
         L     R4,AMAINTAB                                                      
*                                                                               
RNF07    MVC   0(8,R1),0(R4)       EYE CATCHER                                  
         LA    R1,8(R1)            R1 TO DATA AREA                              
         ICM   R3,15,12(R4)        DISPLACEMENT OR ADDRESS                      
         BP    RNF09                                                            
         LA    R3,AC55D            GET START OF DSECT                           
         SR    RF,RF                                                            
         ICM   RF,3,14(R4)         GET DISPLACEMENT INTO DSECT                  
         AR    R3,RF                                                            
*                                                                               
RNF09    ST    R1,0(R3)            A(START OF THIS TABLE)                       
         L     R0,8(R4)            LENGTH OF THIS TABLE                         
         AR    R1,R0               R1 TO NEXT AREA                              
         LA    R4,L'MAINTAB(R4)                                                 
         CLI   0(R4),X'FF'                                                      
         BNE   RNF07                                                            
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         MVC   VSSB,MCSSB          V(SSB)                                       
         MVC   PQID,MCREMPQK       PQ ID                                        
         MVC   MCUSRDMP(4),MAINBGN START OF STORAGE AREA                        
         STCM  R1,15,MCUSRDMP+4    SET END OF STORGE AREA                       
         MVC   XSPRM1,APTNTAB      A(TRANSACTION TABLE)                         
         MVI   RNSW,0                                                           
         MVI   RNSW2,0                                                          
         CLI   RCRERUN,C'Y'                                                     
         BNE   *+8                                                              
         OI    RNSW,RERNY          TEST A RERUN=Y                               
         CLI   RCRERUN,C'F'                                                     
         BNE   *+8                                                              
         OI    RNSW,RERNY+RERNF    OR RERUN=F                                   
         CLI   RCWRITE,C'N'        TEST WRITE=NO                                
         BNE   *+8                                                              
         OI    RNSW2,WRTNO                                                      
         CLI   MCTSTRUN,X'FF'      TEST RUN=TEST                                
         BNE   *+8                                                              
         OI    RNSW2,RUNTST                                                     
         CLI   MCRECOVR,C'W'                                                    
         BNE   *+8                                                              
         OI    RNSW,SOON                                                        
         TM    RNSW,RERNY          TEST RERUN                                   
         BO    *+8                                                              
         MVI   FCFLTUSD,C'Y'       FILTER OUT USED ITEMS                        
         OC    MCREMPQK,MCREMPQK   TEST SOON RUN                                
         BZ    RNF30                                                            
         OI    RNSW,SOON                                                        
         NI    RNSW2,ALL-RUNTST    SOON CAN'T BE TEST                           
         DROP  RF                                                               
         L     RF,REMOTEC          SET UP REPORT TITLE                          
         USING REMOTED,RF                                                       
         MVI   REMOTCLS,C'E'                                                    
         DROP  RF                                                               
*                                                                               
RNF30    MVC   DIR,ACCFIL          ASSUME ACCOUNT FILE                          
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         LA    R0,DICI                                                          
         LA    R1,DICO                                                          
         STM   R0,R1,ACMAUCDI                                                   
         TM    ACMINDS,ACMIEMUD    IS FILE EMULATED                             
         BZ    *+10                                                             
         MVC   DIR,ACCDIR          SET FOR DIRECTORY                            
         DROP  RF                                                               
         GOTO1 DATAMGR,DMCB,DMDTFA,DIR                                          
         MVC   ACDCB,12(R1)        SAVE A(ACCDIR/ACCOUNT) DCB                   
*                                                                               
         LA    R0,NRNTOT                                                        
         LA    R1,RNTOTS                                                        
         ZAP   0(L'RNTOTS,R1),=P'0' CLEAR SOME ACCUMS                           
         LA    R1,L'RNTOTS(R1)                                                  
         BCT   R0,*-10                                                          
         ZAP   CHNUM,=P'1'         CHECK NUMBER                                 
         XC    REQTAB,REQTAB                                                    
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,TODAY2)                                
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,TODAY1)                                
         GOTO1 DATCON,DMCB,(4,RCDATE),(3,TODAY3)                                
         OC    ORIGINUM,ORIGINUM                                                
         BNZ   *+6                                                              
         DC    H'0'                NO ORIGIN NUMBER                             
         MVC   BANKEY+1(14),=CL14'BANK'                                         
         MVC   BANKNAME,SPACES                                                  
*                                                                               
         LA    R1,SRKLQ            KEY FOR SORT                                 
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+6(2)                                                  
         L     RF,ASORTCRD                                                      
         MVC   16(3,RF),DUB        KEY LENGTH FOR SORT RECORD                   
*                                                                               
         LA    R1,SRLNQ            RECORD LENGTH                                
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+6(2)                                                  
         L     RF,ARECCRD                                                       
         MVC   22(3,RF),DUB        FOR RECCARD                                  
         GOTO1 ADSORTER,DMCB,ASORTCRD,ARECCRD,0                                 
         MVI   SCS,0               INIT SORT CONTROL SWITCH                     
         ZAP   SRREQ,=P'1'                                                      
         MVC   SRROFC,SPACES                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
         SPACE 1                                                                
RQF00    L     RF,REMOTEC          SET UP REPORT TITLE                          
         USING REMOTED,RF                                                       
         CLC   REMOTKEY(4),=C'AC55'  TEST OUTPUT IS REMOTE                      
         BNE   *+10                                                             
         MVC   REMOTKEY+4(1),QLEDGER                                            
         DROP  RF                                                               
*                                                                               
         MVC   SVUNIT,QUNIT                                                     
         MVC   SVLDGR,QLEDGER                                                   
         CLC   QAPPL,SPACES        SEQUENCE FOR STACKED REQUESTS                
         BE    RQF03                                                            
         XC    STKKEY,STKKEY                                                    
         LA    R3,STKKEY           BUILD STACK KEY                              
         USING SRMRECD,R3                                                       
         MVI   SRMKTYP,SRMKTYPQ    TYPE                                         
         MVI   SRMKSUB,SRMKSCRQ                                                 
         MVC   SRMKCPY,RCCOMPFL    COMPANY                                      
         MVC   SRMKUNT(2),QUNIT    UNIT/LEDGER                                  
         MVC   SRMKDTE,TODAY1      DATE PWOS                                    
         MVC   SRMKUSR,ORIGINUM    USER                                         
         PACK  DUB,QAPPL(5)        SAVE SEQUENCE NUMBER                         
         CVB   R1,DUB                                                           
         STCM  R1,3,SRMKSEQ                                                     
         OI    RNSW,STACK          SET STACKED REQUESTS                         
         MVI   FCRDACC,C'N'        SKIP FIRST REQUEST                           
         MVI   FCRDTRNS,C'N'                                                    
         B     XIT                                                              
         DROP  R3                                                               
*                                                                               
RQF03    MVC   RQOFFC,QOFFICE      SET UP THE OFFICE FILTER                     
         TM    RQOFFC,X'40'        TEST ALL EXCEPT                              
         BO    *+10                                                             
         MVC   RQOFFC,SPACES                                                    
         TM    RNSW,PROF                                                        
         BO    *+8                 BUILD PROFILES FIRST TIME                    
         BAS   RE,PRFL             BUILD PROFILE TABLE                          
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,WORK)                                  
         CLC   QEND,SPACES                                                      
         BE    *+10                                                             
         MVC   WORK(6),QEND                                                     
         GOTO1 GETBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                       
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,THISBRD)                               
         MVC   WORK+30(6),WORK+12  SAVE LAST DAY OF THIS BROADCAST              
         MVC   DMCB+8(4),=F'-1'                                                 
*                                                                               
         GOTO1 ADDAY,DMCB,WORK+6,WORK                                           
         GOTO1 GETBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                       
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,LASTBRD)                               
*                                                                               
         MVC   WORK+6(6),WORK+30   END OF THIS                                  
         MVC   DMCB+8(4),=F'1'     PLUS ONE DAY                                 
         GOTO1 ADDAY,DMCB,WORK+6,WORK                                           
         GOTO1 GETBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                       
         GOTO1 DATCON,DMCB,(0,WORK+12),(1,NEXTBRD)  END OF NEXT BROAD           
*                                                                               
         MVI   RCSUBPRG,0                                                       
         MVC   MNTH,THISMNTH       SET UP MOS                                   
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,CHKDATE)                               
         CLC   QSTART,SPACES                                                    
         BE    RQF09                                                            
         GOTO1 DATCON,DMCB,(0,QSTART),(1,CHKDATE)                               
         MVC   MNTH(1),QSTART+1    USE CHECK DATE FOR MOS                       
         MVC   MNTH+1(1),QSTART+3                                               
         CLI   QSTART+2,C'1'                                                    
         BNE   RQF09                                                            
         MVI   MNTH+1,C'A'                                                      
         CLI   QSTART+3,C'0'                                                    
         BE    RQF09                                                            
         MVI   MNTH+1,C'B'                                                      
         CLI   QSTART+3,C'1'                                                    
         BE    RQF09                                                            
         MVI   MNTH+1,C'C'                                                      
*                                                                               
RQF09    CLC   QEND,SPACES         ANY DATE FILTERING                           
         BE    RQF11                                                            
         GOTO1 DATCON,DMCB,(0,QEND),(1,ENDATE)                                  
*                                                                               
RQF11    MVI   RQSW,0                                                           
         CLI   QOPT1,C'Y'          URGENT AND CREDITS                           
         BNE   *+8                                                              
         OI    RQSW,RQURG+RQCRD                                                 
         CLI   QOPT1,C'U'          URGENT                                       
         BNE   *+8                                                              
         OI    RQSW,RQURG                                                       
         CLI   QOPT2,C'Y'          CASH DISCOUNT ITEMS                          
         BNE   *+8                                                              
         OI    RQSW,RQCSD                                                       
         CLI   QOPT3,C'A'          APPROVED                                     
         BNE   *+8                                                              
         OI    RQSW,RQAPP                                                       
         MVC   CLNTF,SPACES        CLIENT FILTER                                
         MVC   PRDTF,SPACES        PRODUCT FILTER                               
         MVC   CLPRXF,SPACES        CLIENT/PRODUCT EXCLUDE FILTER               
         USING RUNXTRAD,RF                                                      
         L     RF,VEXTRAS                                                       
         OC    VLISTREC,VLISTREC                                                
         BNZ   XIT                                                              
         MVC   CLNTF,QSELECT       CLIENT FILTER                                
         MVC   PRDTF,QSELECT+3     PRODUCT FILTER                               
         TM    CLNTF,X'40'         IS CLIENT FIELD AN ALL EXCEPT                
         BO    XIT                 NO, FILTERS ARE OK                           
         CLI   PRDTF,X'40'         IS THERE A SPECIFIC PRODUCT                  
         BE    XIT                 NO, FILTERS ARE STILL OK                     
*                                  EXCLUDING A SPECIFIC CLIENT PRODUCT          
*                                  *CLI  PROD                                   
         MVC   CLNTF,SPACES        CLEAR CLIENT                                 
         MVC   PRDTF,SPACES              PRODUCT FILTER FIELDS                  
         MVC   CLPRXF,QSELECT      AND SET UP CLIENT/PRODUCT EXCLUDE            
         OI    CLPRXF,X'40'                                                     
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* LEDGER FIRST                                                        *         
***********************************************************************         
         SPACE 1                                                                
LGF00    CLI   FCRDACC,C'N'        STACKED REQUEST                              
         BE    XIT                                                              
         L     R4,ADLDGNAM         GET LEDGER NAME                              
         USING NAMELD,R4                                                        
         MVC   LDGNME,SPACES                                                    
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LDGNME(0),NAMEREC                                                
         DROP  R4                                                               
*                                                                               
         L     R2,ADLDGEL          GET THE OFFICE POSITION                      
         USING LDGELD,R2                                                        
         MVI   LGSW,0              LEDGER CONTROL                               
         CLI   PROGPROF+1,C'Y'     PROFILE FOR INVOICE-APPROVAL                 
         BNE   *+8                 X'02' BIT SETTING SCHEME                     
         OI    LGSW,APPRV                                                       
         TM    LDGSTAT,X'20'       TEST CANADIAN LEDGER                         
         BNO   *+8                                                              
         OI    LGSW,CANAD                                                       
         TM    LDGSTAT,X'08'       IS IT EXPENDITURE                            
         BNO   *+8                                                              
         OI    LGSW,CEXPN                                                       
         L     RF,AETXT1           US TEXT FIELDS                               
         TM    LGSW,CANAD          IS IT CANADIAN                               
         BNO   *+8                                                              
         L     RF,ACTXT1           CANADIAN TEXT FIELDS                         
         MVC   TXT1(TXTLQ),0(RF)   MOVE TEXT FIELDS                             
         MVI   BYTE,0                                                           
         TM    LGSW,CEXPN                                                       
         BNO   *+8                                                              
         MVI   BYTE,CEXQ           SET COKE EXPENDITURE                         
         L     R3,ASYSTAB          GET SYSTEM ENTRY                             
         DROP  R2                                                               
*                                                                               
LGF03    MVC   SYENT,0(R3)         SAVE THE SYSTEM ENTRY                        
         CLI   0(R3),C' '          IS IT THE LAST ONE                           
         BE    LGF05               TAKE IT                                      
         CLC   QLEDGER,0(R3)       MATCH LEDGER                                 
         BNE   LGF04               LOOK FOR ANOTHER                             
         CLI   1(R3),MEDQ          TEST MEDIA                                   
         BE    *+14                                                             
         CLC   SYFMT,PROGPROF+7    MATCH FORMAT CODE                            
         BNE   LGF04                                                            
         CLI   BYTE,0              SPECIAL SYSTEM REQUIRED                      
         BE    LGF05               NO, TAKE THIS ONE                            
         CLC   SYEQU,BYTE          IS THIS THE SPECIAL SYSTEM                   
         BE    LGF05               TAKE IT                                      
*                                                                               
LGF04    LA    R3,L'SYENT(R3)      USE NEXT ENTRY                               
         B     LGF03                                                            
*                                                                               
LGF05    BAS   RE,OFNM             CHECK NUMBER FROM OFFICE CHECK               
         BAS   RE,IDNM             GET AGENCY LOGO                              
         TM    LGSW,CEXPN          TEST COKE EXPENDITURE                        
         BNO   *+16                                                             
         TM    FTPSW,FTPTRN        FILE TRANSFER                                
         BNO   *+8                                                              
         OI    ALTXSW,CCXTRN       SET EXPENDITURE TRANSFER                     
         CLI   TYPCHK,C' '         TYPE OF CHECK RUN                            
         BNH   LGF09               UNAUTHORIZED USER                            
         CLI   RCPOSTNG,C'N'                                                    
         BE    *+8                                                              
         BAS   RE,OPNWK            OPEN WORKER FILE                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PROGPROF+3       MINIMUM AMOUNT                               
         CVD   RF,DUB                                                           
         MP    DUB,=P'100'         X 100                                        
         ZAP   MINCHK,DUB          MINIMUM                                      
         CP    MINCHK,=P'0'                                                     
         BH    *+10                                                             
         ZAP   MINCHK,=P'001'      DEFAULT MINIMUM IS $1                        
         ZAP   MAXCHK,=P'99999999999'                                           
         ICM   RF,1,PROGPROF+4     MAXIMUM AMOUNT                               
         BZ    LGF07                                                            
         CVD   RF,DUB                                                           
         MP    DUB,=P'1000000'     PROFILE VALUE X $10,000.00                   
         ZAP   MAXCHK,DUB                                                       
*                                                                               
LGF07    L     RF,REMOTEC          SET UP REPORT TITLE                          
         USING REMOTED,RF                                                       
         TM    FTPSW,FTPTRN        FILE TRANSFER                                
         BNO   *+8                                                              
         MVI   REMOTCLS,C'G'                                                    
         TM    RNSW,SOON           ONLY FOR SOON CHECKS                         
         BZ    LGFX                                                             
         MVC   REMOTFNO(2),POWCODE                                              
         MVC   REMOTFNO+2(1),QLEDGER                                            
         MVI   REMOTFNO+3,C'$'                                                  
         DROP  RF                                                               
*                                                                               
         L     R4,ADLEDGER         GET LEDGER RECORD                            
         MVI   ELCODE,X'EA'        LEDGER LOCK ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING LGLELD,R4                                                        
         TM    RNSW2,RUNTST        RUN=TEST                                     
         BO    *+14                                                             
         TM    LGLSTAT,LGLSLOCK    TEST LEDGER IS LOCKED                        
         BO    *+6                                                              
         DC    H'0'                LEDGER MUST BE LOCKED FOR SOON               
         MVC   LUID,LGLLUID        SAVE LUID                                    
         B     LGFX                                                             
*                                                                               
LGF09    MVI   FCRDACC,C'N'        UNAUTHORIZED USER (DON'T PROCESS)            
         MVI   FCRDTRNS,C'N'                                                    
         MVI   FCPRCDIR,C'N'                                                    
         OI    LGSW,UNAUTH         SET UNAUTHORIZED STATUS                      
         MVC   P+5(20),=CL20'UNAUTHORIZED USER'                                 
         GOTO1 ACREPORT                                                         
*                                                                               
LGFX     B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNT                                                     *         
***********************************************************************         
         SPACE 1                                                                
PAC00    MVI   FCRDTRNS,C'N'       DEFAULT IS NO CHECK                          
         MVI   FCPRCDIR,C'N'                                                    
         ZAP   CHTOT,=P'0'         CLEAR ACCOUNT ACCUMULATORS                   
         ZAP   CHCSD,=P'0'                                                      
*                                                                               
         ICM   R4,15,ADACCNAM      MUST HAVE PAYEE NAME                         
         BZ    XIT                                                              
         USING NAMELD,R4                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   NAMEREC(0),SPACES                                                
         BNH   XIT                                                              
         DROP  R4                                                               
*                                                                               
         XC    ANXTRN,ANXTRN       ADDRESS OF NEXT TRANSACTION IN TABLE         
         CLI   QOPT4,C'Y'          DO WE PAY HOLD VENDORS?                      
         BE    PAC03               YES                                          
         TM    RNSW,RERNY          TEST RERUN                                   
         BO    PAC03               DON'T CHECK PAY=NO                           
         L     R2,ADACCSTA         LOAD RSTELD ELEMENT                          
         USING RSTELD,R2                                                        
         CLI   RSTLN,RSTLN1Q       IS THIS NEW LENGTH?                          
         BNH   PAC03               NO                                           
         TM    RSTSTAT2,X'20'      IS VENDOR'S PAY STATUS = NO?                 
         BO    XIT                 YES - SKIP VENDOR                            
         DROP  R2                                                               
*                                                                               
         USING ABLELD,R2                                                        
PAC03    L     R2,ADACCBAL                                                      
         LTR   R2,R2               IS THERE A BALANCE ELEMENT                   
         BZ    XIT                                                              
         TM    RNSW,RERNY          TEST RERUN                                   
         BO    PAC05               DON'T CHECK BALANCE                          
         TM    ALTXSW,CCXTRN       TEST COKE TRANSFER                           
         BO    PAC05                                                            
         CLI   PROGPROF+2,C'Y'                                                  
         BE    PAC05               SKIP TEST BELOW                              
         CP    ABLCR,ABLDR         CREDITS MUST BE GREATER THAN                 
         BNH   XIT                 DEBITS IF WE ARE TO WRITE A CHECK            
         DROP  R2                                                               
*                                                                               
PAC05    MVI   CHKTYPE,0           TRANSACTION TYPE DETERMINES CHECK            
         XC    NUMTRN,NUMTRN       CLEAR TRANSACTION COUNT                      
         XC    SRVEND,SRVEND       CLEAR COKE VENDOR                            
         TM    ALTXSW,CCXTRN       TEST COKE EXPENDITURE TRANSFER               
         BNO   PACOKX                                                           
         L     R4,ADACC                                                         
         CLC   8(3,R4),=C'100'     TEST BOTTLER BOUGHT                          
         BL    XIT                 NO CHECKS                                    
         AH    R4,DATADISP                                                      
         SR    R1,R1                                                            
*                                                                               
         USING FFTELD,R4           GET VENDOR NUMBER FOR COKE                   
PAC07    AR    R4,R1                                                            
         CLI   0(R4),0             TEST EOR                                     
         BE    XIT                 NO VENDOR -- SKIP ACCOUNT                    
         IC    R1,1(R4)                                                         
         CLI   0(R4),FFTELQ                                                     
         BNE   PAC07                                                            
         CLI   FFTTYPE,FFTTVEND                                                 
         BNE   PAC07                                                            
         IC    R1,FFTDLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   SRVEND(0),FFTDATA                                                
         DROP  R4                                                               
*                                                                               
PACOKX   MVI   FCRDTRNS,C'Y'       OK TO PROCESS A CHECK                        
         MVI   FCPRCDIR,C'Y'       GET DIRECTORY MODE                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS A TRANSACTION DIRECTORY                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R3                                                       
PTD00    L     R3,4(R1)                                                         
         CLI   TRNKSTYP,X'81'      TEST CHECK ITEMS                             
         BE    PTDNO                                                            
         TM    TRNKSTAT,X'20'      TEST REVERSED                                
         BO    PTDNO                                                            
*                                                                               
PTD09    MVC   CLNT,TRNKCULC+12    GET THE CLIENT CODE                          
         MVC   PRDT,TRNKREF        PRODUCT CODE                                 
         TM    SYEQU,PRDQ+EXPQ     IS IT PRODUCTION OR EXPENSE                  
         BZ    *+16                                                             
         MVC   CLNT,TRNKCULC+3     CLIENT IS IN CONTRA                          
         MVC   PRDT,SPACES         DON'T HAVE PRODUCT - YET                     
*                                                                               
         USING RUNXTRAD,RF                                                      
         L     RF,VEXTRAS                                                       
         OC    VLISTREC,VLISTREC                                                
         BNZ   PTD11               IF LIST RECORD SKIP CLIENT EDIT              
         CLC   CLNTF,SPACES        CLIENT IN REQUEST                            
         BE    PTD11                                                            
         CLC   CLNT,CLNTF          TEST REQUESTED CLIENT                        
         BE    PTD11                                                            
         TM    CLNTF,X'40'         'NOT' LOGIC                                  
         BO    PTDNO                                                            
         NI    CLNT,X'BF'                                                       
         CLC   CLNT,CLNTF          CAN WE EXCLUDE THIS CLIENT                   
         BE    PTDNO                                                            
         OI    CLNT,X'40'                                                       
*                                                                               
PTD11    L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         MVC   OFFICE,TRNKOFF      NEW OFFICE                                   
         TM    ACMINDS,ACMINEWO    TEST NEW OFFICES                             
         BO    PTD13                                                            
         MVC   OFFICE,SPACES                                                    
         MVC   OFFICE(1),TRNKSANL     OLD OFFICE                                
         CLC   RQOFFC,SPACES                                                    
         BE    PTD13                                                            
         CLC   OFFICE,RQOFFC       OFFICE                                       
         BNE   PTDNO                                                            
*                                                                               
PTD13    MVI   FILTER,C'Y'                                                      
         MVI   DATECK,C'Y'                                                      
         TM    RNSW,RERNY          TEST RERUN                                   
         BO    *+16                DON'T CHECK PAY=NO                           
         BAS   RE,FLTR             CLIENT PROFILE FILTERS                       
         CLI   FILTER,C'Y'                                                      
         BNE   PTDNO                                                            
*                                                                               
         BAS   RE,XFLTR            SPECIAL AGENCY SPECIFIC FILTERS              
         CLI   FILTER,C'Y'                                                      
         BNE   PTDNO                                                            
*                                                                               
*                                                                               
PTD17    CLC   QEND,SPACES                                                      
         BE    PTD19                                                            
         CLI   DATECK,C'N'         OVERRIDE THE DATE CHECK                      
         BE    PTD19                                                            
         CLC   TRNKDATE,ENDATE     ONLY WRITE CHECKS FOR POSTINGS               
         BH    PTDNO               OLDER THAN REQUEST DATE                      
*                                                                               
PTD19    DS    0H                                                               
PTDYES   SR    R1,R1                                                            
         B     *+8                                                              
PTDNO    LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT                                                              
         DROP  R3,RF                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS A TRANSACTION                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNELD,R2                                                        
PTN00    L     R2,ADTRANS                                                       
         CLI   0(R2),X'44'                                                      
         BNE   XIT                                                              
         TM    TRNSTAT,X'80'       TEST DEBIT                                   
         BO    XIT                                                              
         TM    TRNSTAT,X'04'       TEST HELD                                    
         BO    XIT                                                              
*                                                                               
         L     R5,ACLIPRO          A(CLIENT PROFILE)                            
         USING PRFD,R5                                                          
         CLI   PRFPAIO,C'Y'        PAY APPROVED ITEMS ONLY?                     
         BNE   *+12                                                             
         TM    TRNSTAT,X'02'                                                    
         BNO   XIT                                                              
*                                                                               
         TM    RQSW,RQAPP          APPROVED ONLY                                
         BO    *+12                                                             
         TM    LGSW,APPRV          APPROVAL SYSTEM                              
         BNO   *+12                                                             
         TM    TRNSTAT,X'02'       TEST ITEM IS APPROVED                        
         BNO   XIT                                                              
*                                                                               
         TM    RQSW,RQURG          TEST URGENT ONLY                             
         BNO   PTN03                                                            
         TM    TRNSTAT,X'40'       TEST URGENT TRANSACTION                      
         BO    PTN03                                                            
         TM    RQSW,RQCRD          URGENTS & CREDITS                            
         BNO   XIT                                                              
         CP    TRNAMNT,=P'0'       NOT MINUS                                    
         BH    XIT                                                              
*                                                                               
PTN03    TM    SYEQU,MEDQ          TEST MEDIA LEDGER                            
         BO    PTN05                                                            
         CLI   SYSPROF+1,C'Y'      AUTHORIZATION SYSTEM                         
         BNE   PTN05                                                            
         TM    TRNSTAT,X'08'       THAN ITEM MUST BE AUTHORIZED                 
         BNO   XIT                                                              
*                                                                               
PTN05    CLC   NUMTRN,=Y(PTNMX)    IS TABLE FULL                                
         BNL   XIT                                                              
         LR    R3,R2                                                            
         SH    R3,DATADISP                                                      
         USING TRNRECD,R3                                                       
         TM    RNSW,RERNY          IS IT A RERUN=Y                              
         BNO   PTN09                                                            
         CLC   TRNKEY+ACCOUSED(2),TODAY2                                        
         BNE   XIT                 NOT USED TODAY                               
         L     R1,ADTRANS                                                       
         SR    R0,R0                                                            
*                                                                               
         USING TRSELD,R1                                                        
PTN07    IC    R0,TRSLN            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   TRSEL,0                                                          
         BE    XIT                                                              
         CLI   TRSEL,TRSELQ        TEST STATUS ELEMENT                          
         BNE   PTN07                                                            
         OC    TRSUSER,TRSUSER     TEST ORIGIN NUMBER                           
         BZ    XIT                                                              
         CLC   TRSUSER,ORIGINUM                                                 
         BNE   XIT                                                              
         TM    RNSW,RERNF          RERUN = FORCE                                
         BO    PTN11               DON'T CHECK REQUEST NUMBER                   
         OC    TRSUREQ,TRSUREQ     NO REQUEST NUMBER - INCLUDE IT               
         BZ    XIT                                                              
         CLI   TYPCHK,TYPSOON                                                   
         BNE   *+10                                                             
         SP    TRSUREQ,=P'100'                                                  
         CP    TRSUREQ,RCRQTOT     REQUEST NUMBER                               
         BNE   XIT                                                              
         TM    TRSSTAT,TRSSACHQ                                                 
         BNO   XIT                                                              
         B     PTN11                                                            
         DROP  R1                                                               
*                                                                               
PTN09    OC    TRNKEY+ACCOUSED(2),TRNKEY+ACCOUSED                               
         BNZ   XIT                 AND IT'S USED, SKIP IT                       
*                                                                               
PTN11    BAS   RE,GETCD            THIS WILL GET ITEM CD                        
         TM    RQSW,RQCSD          CASH DISCOUNT ONLY                           
         BNO   PTN27                                                            
         CP    TRNAMNT,=P'0'                                                    
         BL    PTN27                                                            
         CP    CD,=P'0'            SKIP IF NO CD                                
         BE    XIT                                                              
*                                                                               
PTN27    DS    0H                                                               
*        CP    TRNAMNT,=P'0'                                                    
*        BNE   *+14                                                             
*        CP    CD,=P'0'            SKIP ZERO AMOUNT                             
*        BE    XIT                                                              
         CLC   QBILGRUP,SPACES     FILTERING ON ESTIMATE NUMBER                 
         BE    PTN28               NO                                           
         PACK  DUB,QBILGRUP        ESTIMATE NUMBER                              
         CVB   R1,DUB                                                           
         STH   R1,HALF             STORE BINARY IN ESTNUM                       
         MVI   ELCODE,X'46'                                                     
         L     R4,ADTRANS                                                       
         BAS   RE,FIRSTEL                                                       
         BNE   PTN28               SKIP CHECK IF NO X'46'                       
         USING XPYELD,R4                                                        
         TM    QBILGRUP,X'40'      IS THIS ALL EXCEPT                           
         BNO   *+18                YES                                          
         CLC   HALF,XPYEST         MATCH ESTIMATE                               
         BE    PTN28               KEEP IT                                      
         B     XIT                                                              
         CLC   HALF,XPYEST         ALL EXCEPT                                   
         BE    XIT                 SO IF IT MATCHES, SKIP IT                    
         DROP  R4                                                               
*                                                                               
PTN28    CLC   PRDT,SPACES         TEST ALREADY HAVE PRODUCT                    
         BNE   PTN29                                                            
         MVI   ELCODE,X'23'        GET 'OTHERS' ELEMENT                         
         L     R4,ADTRANS                                                       
         BAS   RE,FIRSTEL                                                       
         BNE   PTN31                                                            
         USING OTHELD,R4                                                        
         MVC   PRDT,OTHNUM         GET PRODUCT CODE                             
         DROP  R4                                                               
*                                                                               
PTN29    CLC   CLPRXF,SPACES       CLIENT/PRODUCT EXCLUDE                       
         BE    PTN30               NO                                           
         CLC   CLPR,CLPRXF         TEST CLIENT/PRODCUT                          
         BNE   PTN31               PAY ALL OTHERS                               
         B     XIT                 EXCLUDE THIS CLIENT/PRODUCT                  
*                                                                               
PTN30    CLC   PRDTF,SPACES        PRODUCT FILTER                               
         BE    PTN31                                                            
         CLC   PRDT,PRDTF          TEST PRODUCT VS. FILTER                      
         BE    PTN31                                                            
         TM    PRDTF,X'40'         TEST EXCLUDE PRODUCT                         
         BO    XIT                 POSITIVE FILTER(DIDN'T MATCH)                
         CLC   PRDT,SPACES         IS THERE A PRODUCT                           
         BE    PTN31               NO, TAKE IT                                  
         NI    PRDT,X'BF'                                                       
         CLC   PRDT,PRDTF          TEST PRODUCT VS. FILTER(NEGATIVE)            
         BE    XIT                 EXCLUDE                                      
         OI    PRDT,X'40'                                                       
*                                                                               
PTN31    ICM   R5,15,ANXTRN        NEXT TABLE ENTRY                             
         BNZ   *+8                                                              
         L     R5,APTNTAB          FIRST TIME, START TABLE                      
         USING PTND,R5                                                          
         XC    PTNKEY(PTNLQ),PTNKEY                                             
         CLI   PROGPROF+8,C'Y'     CHECKS BY CLIENT                             
         BNE   PTN32                                                            
         TM    SYEQU,PRDQ+MEDQ+EXPQ       PRODUCTION, MEDIA OR EXPENSE          
         BZ    PTN32                                                            
         MVC   PTNCLI,CLNT         CLIENT                                       
*                                                                               
PTN32    MVC   PTNCKEY(PTNCKLQ),TRNKOFF   OFFICE/CONTRA/DATE/REF/SBR            
         ZAP   PTNAMT,TRNAMNT      AMOUNT                                       
         ZAP   PTNCD,CD            C.D.                                         
         ZAP   PTNTAMT,=P'0'                                                    
         ZAP   PTNTCD,=P'0'                                                     
         MVC   PTNCNM,SPACES       CONTRA ACCOUNT NAME                          
         MVI   PTNCLN,0            SET CONTRA NAMELENGTH TO ZERO                
         L     R4,ADSUBAC                                                       
         CLI   0(R4),X'43'                                                      
         BNE   PTN33                                                            
         USING CACELD,R4                                                        
         SR    R1,R1                                                            
         IC    R1,CACLN                                                         
         SH    R1,=H'18'                                                        
         BM    PTN33                                                            
         EX    R1,*+4                                                           
         MVC   PTNCNM(0),CACNAME                                                
         AH    R1,=H'1'            LENGTH OF NAME                               
         STC   R1,PTNCLN                                                        
         DROP  R3,R4                                                            
*                                                                               
PTN33    LA    R5,PTNLQ(R5)                                                     
         ST    R5,ANXTRN                                                        
         XC    PTNREC,PTNREC       CLEAR NEXT ENTRY                             
         LH    R0,NUMTRN           UPDATE TRANSACTION COUNT                     
         AH    R0,=H'1'                                                         
         STH   R0,NUMTRN                                                        
         AP    CHTOT,TRNAMNT       ADD AMOUNT TO CHECK TOTAL                    
         AP    CHCSD,CD            ADD C.D. TO CHECK C.D.                       
         CLI   TRNTYPE,37          TYPE 37                                      
         BE    XIT                                                              
         CLI   TRNTYPE,45          AND 45 CAN'T HELP DECIDE                     
         BE    XIT                 WHICH REMITTANCE TO USE                      
         MVC   CHKTYPE,TRNTYPE     SAVE TYPE TO HELP DETERMINE                  
         B     XIT                 WHICH REMITTANCE FORM TO USE                 
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ACCOUNT LAST                                                        *         
***********************************************************************         
         SPACE 1                                                                
ACL00    OC    NUMTRN,NUMTRN                                                    
         BZ    XIT                                                              
         TM    FTPSW,FTPTRN        FILE TRANSFER                                
         BO    ACL01                                                            
         CP    CHTOT,=P'0'                                                      
         BNH   XIT                                                              
*                                                                               
ACL01    MVC   XSPRM2+2(2),NUMTRN  NUMBER IN TABLE                              
         GOTO1 XSORT,XSPRM         SORT THE TABLE                               
         L     R4,ADACC                                                         
         MVC   PAYACC,0(R4)        PAYEE ACCOUNT                                
         MVC   PAYNME,SPACES       ACCOUNT NAME                                 
*                                                                               
         L     R4,ADACCNAM                                                      
         USING NAMELD,R4                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   PAYNME(0),NAMEREC                                                
         AH    R1,=H'1'            GET LENGTH OF NAME                           
         STC   R1,SRANML                                                        
         MVC   SRANME,PAYNME       ACCOUNT NAME TO SORT RECORD                  
         BAS   RE,ADDR             ADD THE ADDRESS                              
         DROP  R4                                                               
*                                                                               
         MVI   SROPT,0                                                          
         CLI   PROGPROF,C'Y'       AMOUNT IN WORDS                              
         BNE   *+8                                                              
         OI    SROPT,SROWRD                                                     
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,7,SYTYP                                                       
         AR    RE,RB               RF TO TYPE TABLE                             
*                                                                               
ACL05    CLI   0(RE),0             LAST ENTRY ?                                 
         BE    ACL06               TAKE THE DEFAULT                             
         CLC   CHKTYPE,0(RE)       MATCH TYPE                                   
         BE    ACL06                                                            
         LA    RE,L'TYENT(RE)                                                   
         B     ACL05                                                            
*                                                                               
ACL06    MVC   TYENT,0(RE)                                                      
         MVC   SRTYPE,CHKTYPE      ACCOUNT TYPE                                 
         MVC   SRSYENT,SYENT       SYSTEM ENTRY                                 
         MVC   SRTYENT,TYENT       TYPE ENTRY                                   
         L     R5,APTNTAB          R5 = TRANSACTION TABLE                       
         SR    R3,R3               CHECK SEQUENCE NUMBER                        
*                                                                               
ACL07    LA    R0,1                ITEM COUNT                                   
         AH    R3,=H'1'            INCREMENT SEQUENCE                           
         LR    R1,R5               SAVE START OF SEARCH                         
         USING PTND,R5                                                          
         ZAP   CHTOT,PTNAMT        GET CHECK TOTAL                              
         ZAP   CHCSD,PTNCD                                                      
*                                                                               
ACL09    LA    RF,PTNLQ(R5)        LOOK TO NEXT                                 
         OC    0(PTNLQ,RF),0(RF)   END OF TABLE                                 
         BZ    ACL11                                                            
         CLC   PTNCLI,PTNCLI-PTNREC(RF) SAME CLIENT                             
         BNE   ACL11                                                            
         CH    R0,=H'800'          MAX ITEMS ON CHECK                           
         BNL   ACL11                                                            
         LR    R5,RF                                                            
         AH    R0,=H'1'            ITEM COUNT                                   
         AP    CHTOT,PTNAMT        ADD TO TOTAL                                 
         AP    CHCSD,PTNCD                                                      
         B     ACL09                                                            
*                                                                               
ACL11    OC    PTNCLI,PTNCLI       NO CHECKS FOR THIS RUN IF                    
         BNZ   ACL12               - ANY CHECKS ARE < 0                         
         TM    FTPSW,FTPTRN        FILE TRANSFER                                
         BO    ACL12                                                            
         CP    CHTOT,=P'0'         TEST CHECK TOTAL                             
         BL    ACL27                                                            
*        BNH   ACL27                                                            
ACL12    LR    R5,R1               R5 TO FIRST FOR THIS CLIENT                  
*                                                                               
ACL13    ZAP   PTNTAMT,CHTOT       STAMP WITH CHECK TOTALS                      
         ZAP   PTNTCD,CHCSD                                                     
         STCM  R3,3,PTNSEQ         SEQUENCE NUMBER                              
         STCM  R0,3,PTN#INV        NUMBER OF INVOICES                           
         LA    R5,PTNLQ(R5)                                                     
         CR    R5,RF               END OF THIS CLIENT                           
         BNE   ACL13                                                            
         OC    0(PTNLQ,RF),0(RF)   END OF TABLE                                 
         BNZ   ACL07                                                            
         L     R5,APTNTAB          R5 = TRANSACTION TABLE                       
*                                                                               
ACL19    OC    PTNREC,PTNREC       ANYTHING LEFT IN TABLE                       
         BZ    ACL27               END OF TABLE                                 
         ZAP   CHTOT,PTNTAMT                                                    
         ZAP   CHCSD,PTNTCD                                                     
         TM    FTPSW,FTPTRN        FILE TRANSFER                                
         BO    *+14                                                             
         CP    CHTOT,MINCHK        TEST AMOUNT LOWER THAN MINIMUM               
         BL    ACL26               TEST FOR MAXIMUM AMOUNT                      
         CP    CHTOT,MAXCHK        TEST IF HIGHER THAN MAXIMUM AMT              
         BH    ACL26               SKIP IF OVER MAXIMUM                         
ACL20    OI    RQSW,RQACT          TURN ON ACTIVITY                             
         L     R3,ATRNIO                                                        
         USING TRNRECD,R3                                                       
         L     RF,ADACC                                                         
         MVC   TRNKCULA,0(RF)      ACCOUNT                                      
         MVC   TRNKOFF(PTNCKLQ),PTNCKEY TRANACTION KEY                          
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,ATRNIO,ATRNIO                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T READ TRANSACTION                       
         MVC   TRNKEY+ACCOUSED(2),TODAY2 MARK IT USED                           
         LA    R1,TRNKEY+ACCORFST                                               
         SR    R0,R0                                                            
*                                                                               
         USING TRSELD,R1                                                        
ACL21    IC    R0,TRSLN            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   TRSEL,0                                                          
         BE    ACL23                                                            
         CLI   TRSEL,TRSELQ        TEST STATUS ELEMENT                          
         BNE   ACL21                                                            
         OI    TRSSTAT,TRSSACHQ    SET CHECK WRITTEN BY AC55                    
         NI    TRSSTAT,X'FF'-TRSSVOID NOT VOID                                  
         XC    TRSVOID,TRSVOID                                                  
         ZAP   TRSUREQ,RCRQTOT     REQUEST NUMBER                               
         CLI   TYPCHK,TYPSOON                                                   
         BNE   *+10                                                             
         AP    TRSUREQ,=P'100'     ADD 100 TO DISTINQUISH SOON                  
         MVC   TRSUSER,ORIGINUM    ORIGIN NUMBER                                
         DROP  R1,R3                                                            
*                                                                               
ACL23    TM    RNSW2,WRTNO         NOT WRITING TO FILE                          
         BO    ACL25                                                            
         GOTO1 DATAMGR,DMCB,DMWRT,ACCFIL,(R3),(R3)                              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ACL25    BAS   RE,BSR              BUILD THE SORT RECORD                        
ACL26    LA    R5,PTNLQ(R5)        R5 = NEXT ITEM                               
         B     ACL19                                                            
*                                                                               
ACL27    B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* REQUEST LAST                                                        *         
***********************************************************************         
         SPACE 1                                                                
RQL00    CLC   QAPPL,SPACES        INITIAL STACK REQUEST                        
         BNE   RQL07               YES, PROCESS FIRST IN STACK                  
         TM    RQSW,RQACT          ANY ACTIVITY FOR THIS REQUEST                
         BO    RQL07               YES, PROCESS NEXT                            
         TM    LGSW,UNAUTH         TEST UNAUTHORIZED STATUS                     
         BO    XIT                                                              
         ZAP   CHTOT,=P'0'                                                      
         ZAP   CHCSD,=P'0'                                                      
         XC    SREC(SRKLQ),SREC    CLEAR KEY                                    
         BAS   RE,BSR              PUT ONE ZERO RECORD                          
         LA    R2,REQTAB                                                        
         LA    R3,28               MAX OF 28 BAD REQUESTS                       
*                                                                               
RQL03    OC    0(3,R2),0(R2)                                                    
         BZ    RQL05                                                            
         LA    R2,3(R2)                                                         
         BCT   R3,RQL03                                                         
         B     XIT                 MORE THAN 28-GO AWAY                         
*                                                                               
RQL05    ZAP   0(3,R2),RCRQTOT     REQUEST WITH NO CHECKS                       
*                                                                               
RQL07    TM    RNSW,STACK          STACKED REQUESTS                             
         BNO   XIT                                                              
         MVI   FCRDACC,C'Y'        SET TO READ ACCOUNTS                         
         MVI   FCRDTRNS,C'Y'       AND TRANSCATIONS                             
         LA    R3,STKKEY                                                        
         USING SRMRECD,R3                                                       
         LA    RF,SRMKACT+(L'SRMKACT-1)                                         
         DROP  R3                                                               
         SR    R1,R1               BUMP TO NEXT RECORD                          
         IC    R1,0(RF)                                                         
         AH    R1,=H'1'                                                         
         STC   R1,0(RF)                                                         
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCFIL,STKKEY,AIO                            
         L     R3,AIO                                                           
         CLC   0(SRMKACT-SRMKEY,R3),STKKEY TEST SAME STACK                      
         BNE   XIT                 ALL DONE                                     
         MVC   STKKEY,0(R3)        SAVE NEW KEY                                 
         MVC   QRECORD,SPACES                                                   
         MVC   QRECORD2,SPACES                                                  
         USING ACCRECD,R3                                                       
         SR    R1,R1                                                            
         ICM   R1,3,ACCKEY+ACCORLEN                                             
         LA    R1,0(R1,R3)         R1 = END OF RECORD                           
         MVI   0(R1),0             FORCE END RECORD(OLD BUG IN REQUEST)         
         LA    R4,ACCKEY+ACCORFST                                               
         SR    R0,R0                                                            
         DROP  R3                                                               
*                                                                               
         USING RQCELD,R4                                                        
RQL13    CLI   RQCEL,RQCELQ        REQUEST CARD ELEMENT                         
         BNE   RQL15                                                            
         LA    RF,QRECORD          SET REQUEST DETAILS                          
         CLI   RQCSEQ,1                                                         
         BE    *+8                                                              
         LA    RF,QRECORD2                                                      
         MVC   0(L'RQCCARD,RF),RQCCARD                                          
*                                                                               
RQL15    CLI   RQCEL,0             END OF RECORD                                
         BE    RQL17                                                            
         IC    R0,RQCLN                                                         
         AR    R4,R0                                                            
         B     RQL13                                                            
         DROP  R4                                                               
*                                                                               
RQL17    L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         MVI   ACMMODE,REQFRST     SIGNAL CONTROLLER TO RESET REQFRST           
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* RUN LAST                                                            *         
***********************************************************************         
         SPACE 1                                                                
RNL00    LA    R0,NACTOT           CLEAR ACCOUNT ACCUMULATORS                   
         LA    R1,ACTOTS                                                        
         ZAP   0(L'ACTOTS,R1),=P'0'                                             
         LA    R1,L'ACTOTS(R1)                                                  
         BCT   R0,*-10                                                          
         TM    LGSW,UNAUTH         TEST UNAUTHORIZED STATUS                     
         BO    GETSX               END SORT                                     
*                                                                               
         GOTO1 APOST               OPEN THE WORKER FILE                         
         ZAP   CURLNE,=P'0'        CURRENT LINE NUMBER                          
         ZAP   DLNLNE,=P'50'       DEADLINE                                     
         ZAP   MAXLNE,=P'47'       MAX LINES ON REMITTANCE ADVICE               
         TM    RNSW,MICR                                                        
         BNO   *+10                                                             
         ZAP   MAXLNE,=P'42'       MAX LINES FOR MICR                           
         ZAP   ML1LNE,MAXLNE                                                    
         SP    ML1LNE,=P'1'        MAX LESS ONE                                 
         ZAP   ML2LNE,MAXLNE                                                    
         SP    ML2LNE,=P'2'        MAX LESS TWO                                 
         ZAP   ML3LNE,MAXLNE                                                    
         SP    ML3LNE,=P'3'        MAX LESS THREE                               
         BAS   RE,LNUP             SET THE LINEUP PAGES                         
         BAS   RE,GETS             GET SORT RECORDS AND PRINT CHECKS            
         MVI   MODE,RUNLAST                                                     
         MVI   RCSUBPRG,1                                                       
         TM    RNSW,MICR                                                        
         BO    *+10                                                             
         AP    VOIDS,=P'1'         VOID FOR THE LAST PAGE                       
         MVC   QRECORD,SPACES                                                   
         BAS   RE,NEWPGE                                                        
         MVI   SPACING,2                                                        
         BAS   RE,PRTLNE                                                        
         BAS   RE,PRTLNE                                                        
         BAS   RE,SYS              GET THE SYSTEM NUMBER                        
         MVC   P+TOTCHAT(L'AC@ASYS),AC@ASYS  'ACCOUNT SYSTEM'                   
         MVC   P+TOTCHAT+18(1),BYTE SET SYSTEM NUMBER                           
         BAS   RE,PRTLNE                                                        
*                                                                               
         MVI   SPACING,2                                                        
         MVC   P+TOTCHAT(L'AC@TCSH),AC@TCSH   'TOTAL CASH'                      
         LA    R2,P+TOTFIGS                                                     
         LA    R2,2(R2)                                                         
         CURED RUNTOT,(14,(R2)),2,COMMAS=YES                                    
         LR    R1,R2               ADD LEADING $999.99                          
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-12                                                             
         BCTR  R1,0                                                             
         MVI   0(R1),C'$'                                                       
         BAS   RE,PRTLNE                                                        
*                                                                               
         MVI   SPACING,2                                                        
         MVC   P+TOTCHAT(L'TXT3),TXT3 GOOD CHECKS                               
         EDIT  CHOKS,(5,(R2))                                                   
         BAS   RE,PRTLNE                                                        
*                                                                               
         MVI   SPACING,2                                                        
         MVC   P+TOTCHAT(L'TXT4),TXT4 VOID CHECKS                               
         TM    PRNTSW,SHUTTLE      IF GOING TO SHUTTLE 1 VOID                   
         BNO   *+10                                                             
         AP    VOIDS,=P'1'         FROM LAST TIME                               
         EDIT  VOIDS,(5,(R2))                                                   
         BAS   RE,PRTLNE                                                        
*                                                                               
         MVI   SPACING,2                                                        
         MVC   P+TOTCHAT(L'TXT5),TXT5 TOTAL CHECKS                              
         ZAP   CHUSED,CHOKS        OK'S                                         
         AP    CHUSED,VOIDS        PLUS VOIDS                                   
         EDIT  CHUSED,(5,(R2))                                                  
         BAS   RE,PRTLNE                                                        
*                                                                               
         CP    NOADD,=P'0'         CHECKS WITHOUT AN ADDRESS                    
         BE    RNL05                                                            
         BAS   RE,PRTLNE                                                        
*                                                                               
         MVC   P+TOTCHAT(L'TXT6),TXT6 NO ADDRESS-CHECKS                         
         EDIT  NOADD,(3,P+45)                                                   
         BAS   RE,PRTLNE                                                        
*                                                                               
RNL05    OC    REQTAB,REQTAB       ANY NO-CHECK REQUESTS                        
         BZ    RNL11                                                            
         BAS   RE,PRTLNE                                                        
         MVC   P+TOTCHAT(L'TXT7),TXT7  NO CHECKS FOR                            
         LA    R3,REQTAB                                                        
         LA    R4,HEAD10           USE AS TEMP WORK SPACE                       
         LA    R5,28                                                            
         LA    R2,4(R2)                                                         
*                                                                               
RNL07    OC    0(3,R3),0(R3)       ANY MORE                                     
         BZ    RNL09                                                            
         EDIT  (P3,0(R3)),(5,(R4))                                              
         MVI   5(R4),C','          COMMAS BETWEEN                               
         LA    R3,3(R3)                                                         
         LA    R4,6(R4)                                                         
         BCT   R5,RNL07                                                         
*                                                                               
RNL09    BCTR  R4,R0               DROP  LAST COMMA                             
         MVI   0(R4),C' '                                                       
*                                                                               
         GOTO1 ADSQUASH,DMCB,HEAD10,200                                         
         L     R5,DMCB+4                                                        
         GOTO1 CHOPPER,DMCB,((R5),HEAD10),(43,(R2)),(C'P',4)                    
         L     R5,DMCB+8                                                        
         BCTR  R5,0                                                             
         CVD   R5,DUB                                                           
         AP    CURLNE,DUB                                                       
         MVC   HEAD10,SPACES                                                    
         MVC   HEAD11,SPACES                                                    
         BAS   RE,PRTLNE                                                        
*                                                                               
RNL11    CLC   ALPHAID,BOCLARO     CLOSE BOCLARO TAPE                           
         BNE   RNL12                                                            
         GOTO1 ABOCLARO                                                         
         B     RNL13                                                            
*                                                                               
RNL12    CLC   ALPHAID,CCUSA       CLOSE COKE TAPE                              
         BE    *+14                                                             
         CLC   ALPHAID,MINDSHRE    CLOSE MINDSHARE TAPE                         
         BNE   RNL13                                                            
         L     RF,AEDTRN                                                        
         BASR  RE,RF                                                            
*                                                                               
RNL13    BAS   RE,VDCK             VOID AND POST THE LAST CHECK                 
         CLI   TYPCHK,TYPSOON      DDS AND LOCAL CHECKS                         
         BNE   RNL15               ALWAYS CREATED REGISTER                      
         CP    RUNTOT,=P'0'        SOON CHECKS                                  
         BE    RNL17               DON'T CREATE REGISTER FOR ZERO RUN           
*                                                                               
RNL15    GOTO1 APOST               CLOSE THE WORKER FILE                        
RNL17    AP    CHNUM,=P'1'         1 TO CURRENT FOR FIRST NEXT TIME             
         L     R4,ADLEDGER         GET LEDGER RECORD                            
         MVI   ELCODE,X'EA'        LEDGER LOCK ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   RNL18                                                            
         TM    RNSW,SOON           TEST SOON RUN                                
         BNO   RNL18                                                            
         USING LGLELD,R4                                                        
         XC    LGLDATE,LGLDATE     UNLOCK THE LEDGER                            
         NI    LGLSTAT,X'FF'-LGLSLOCK                                           
         DROP  R4                                                               
*                                                                               
RNL18    DS    0H                                                               
         L     R4,ADLEDGER                                                      
*MN      TM    STATUS,CHKAUTH                                                   
*MN      BZ    RNL19                                                            
*                                                                               
         USING CHARECD,RF          CHECK AUTHORIZATION RECORD                   
         L     RF,AIO                                                           
         MVC   CHAKEY,SPACES                                                    
         MVI   CHAKTYP,CHAKTYPQ    X'10'                                        
         MVC   CHAKCULA,0(R4)      C/U/L                                        
         DROP  RF                                                               
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,AIO,AIO                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*MN      BNE   *+8                                                              
         L     R4,AIO                                                           
RNL19    AH    R4,DATADISP                                                      
*                                                                               
RNL20    CLI   0(R4),X'54'                                                      
         BE    RNL22                                                            
         CLI   0(R4),0                                                          
         BE    RNL26                                                            
RNL21    SR    R0,R0                                                            
         IC    R0,1(R4)            AND FIND OFFICE CHECK ELEMENT                
         AR    R4,R0                                                            
         B     RNL20                                                            
*                                                                               
         USING OCNELD,R4                                                        
RNL22    CLC   ORIGINUM,OCNOFFID                                                
         BNE   RNL21                                                            
         CP    RUNTOT,=P'0'        ZERO RUN                                     
         BE    RNL24               DON'T UPDATE CHECK NUMBER                    
         MVC   OCNBEF,OCNAFT       UPDATE THE CHECK NUMBERS                     
         UNPK  OCNAFT,CHNUM                                                     
         OI    OCNAFT+5,X'F0'                                                   
*                                                                               
         CLI   TYPCHK,TYPLOCL      LOCAL                                        
         BNE   RNL23                                                            
         TM    RNSW,MICR           TEST MICR                                    
         BO    RNL26                                                            
         MVC   OCNDPLR,TODAY2      SET LOCAL REGISTER PENDING                   
         B     RNL26                                                            
*                                                                               
RNL23    CLI   TYPCHK,TYPSOON                                                   
         BNE   RNL26                                                            
         TM    RNSW,MICR           TEST MICR                                    
         BNO   RNL26                                                            
         XC    OCNDPSR,OCNDPSR     CLEAR SOON REGISTER PENDING                  
         B     RNL26                                                            
*                                                                               
RNL24    CLI   TYPCHK,TYPSOON      ZERO SOON RUN                                
         BNE   RNL26                                                            
         XC    OCNDPSR,OCNDPSR     CLEARING PENDING REGISTER                    
         DROP  R4                                                               
*                                                                               
RNL26    DS    0H                                                               
*MN      TM    STATUS,CHKAUTH      IF X'54' ELEMS LOCATED ON X'10' REC          
*MN      BZ    RNL28               THEN MUST WRITE BACK X'10' REC               
         TM    RNSW2,WRTNO         NOT WRITING TO FILE                          
         BO    RNL28                                                            
         GOTO1 DATAMGR,DMCB,DMWRT,ACCFIL,AIO,AIO                                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                ERROR WRITING CHECK AUTH RECORD              
*                                                                               
RNL28    TM    PRNTSW,SHUTTLE      FOR DDS-SHUTTLE - LAST CHECK IS VOID         
         BNO   RNL31                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         LA    R0,5                TO BE LEFT IN BOX FOR NEXT TIME              
*                                                                               
RNL29    MVC   P+10(30),=CL30'****  LINEUP    ******'                           
         BAS   RE,PRTLNE                                                        
         BCT   R0,RNL29                                                         
         ZAP   CURLNE,=P'3'                                                     
         MVI   VOIDSW,X'FF'        VOID THE CHECK                               
         SP    VOIDS,=P'1'         BUT DON'T COUNT IT                           
         SP    CHNUM,=P'1'                                                      
         BAS   RE,VDCK                                                          
*                                                                               
RNL31    TM    RNSW2,WRTNO         NOT WRITING TO FILE                          
         BO    RNL40                                                            
         L     R4,ADLEDGER         GET LEDGER RECORD                            
         MVC   HALF,LDGRLEN-LDGRECD(R4)                                         
         AH    R4,HALF                                                          
         BCTR  R4,0                                                             
         MVI   0(R4),0                                                          
         L     R4,ADLEDGER         GET LEDGER RECORD                            
         GOTO1 DATAMGR,DMCB,DMWRT,ACCFIL,(R4),(R4)                              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RNL40    LM    R0,R1,MAINLEN                                                    
         FREEMAIN R,LV=(0),A=(1)                                                
         TM    RNSW,MICR+SOON      TEST MICR/SOON                               
         BNO   RUN44                                                            
         CP    RUNTOT,=P'0'        TEST ZERO RUN                                
         BNE   RUN43                                                            
         MVC   P,SPACES                                                         
         MVC   P+1(L'AC@ZRUN),AC@ZRUN    ZERO RUN - NO CHECKS TO WRITE          
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
*                                                                               
RUN43    GOTO1 ARUNREG             RUN THE REGISTER                             
*                                                                               
RUN44    TM    RNSW,SOON           TEST SOON                                    
         BNO   *+8                                                              
         BAS   RE,FACOUT           WRITE FACWK FILES                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* WRITE FACWK RECORDS FOR SOON UPDATES                                *         
***********************************************************************         
         SPACE 1                                                                
FACOUT   NTR1  ,                                                                
         XC    PBLK,PBLK                                                        
         LA    R3,PBLK                                                          
         USING FWRECD,R3                                                        
         L     R4,REMOTEC                                                       
         USING REMOTED,R4                                                       
         MVC   FWRLEN,=Y(FWRLNQ)   USER INFO FOR SRUPD00                        
         MVC   FWRUSER,=C'USER='                                                
         MVC   FWRUSID,IDABBR                                                   
         MVC   FWRLUID,LUID                                                     
         MVC   FWRPQID,PQID                                                     
         MVC   FWRWKID,WKID        CHECK FILE                                   
         TM    RNSW,MICR                                                        
         BNO   *+10                                                             
         MVC   FWRWKID,WKIDP       POSTING FILE (IF MICR)                       
         CP    RUNTOT,=P'0'        TEST ZERO RUN                                
         BNE   *+10                                                             
         XC    FWRWKID,FWRWKID     NO WORKER FILES                              
*                                                                               
         L     RF,VSSB                                                          
         USING SSOOFF,RF                                                        
         L     R2,SSOFWNDX         INDEX                                        
         L     R0,SSOFWBUF         BUFFER                                       
         GOTO1 DATAMGR,DMCB,(0,FACADD),(0,FACWRK),(R2),(R3),(R0)                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CP    RUNTOT,=P'0'        TEST ZERO RUN                                
         BE    XIT                                                              
         TM    RNSW,MICR+SOON      TEST MICR - SOON                             
         BNO   XIT                                                              
         XC    PBLK,PBLK                                                        
         LA    R3,PBLK                                                          
         USING FWRECD,R3                                                        
         MVC   FWRLEN,=Y(FWRLNQ)   USER INFO FOR SRUPD00                        
         MVC   FWRUSER,=C'LAST='                                                
         L     RF,REMOTABF         GET REGISTER KEY FROM BUFFER                 
         MVC   FWRPQID,0(RF)                                                    
         L     RF,VSSB                                                          
         USING SSOOFF,RF                                                        
         L     R2,SSOFWNDX         INDEX                                        
         L     R0,SSOFWBUF         BUFFER                                       
         GOTO1 DATAMGR,DMCB,(0,FACADD),(0,FACWRK),(R2),(R3),(R0)                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         DROP  R3,R4,RF                                                         
         EJECT                                                                  
***********************************************************************         
* BUILD AND PUT THE SORT RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING PTND,R5                                                          
BSR      NTR1  ,                                                                
         XC    SRVAR,SRVAR                                                      
         ZAP   SRREQ,RCRQTOT       REQUEST NUMBER                               
         MVC   SRMNTH,MNTH         MONTH OF SERVICE                             
         MVC   SRCDTE,CHKDATE      CHECK DATE                                   
         MVC   SRROFC,RQOFFC       OFFICE REQUEST                               
         ZAP   SRCAMT,CHTOT        CHECK TOTAL                                  
         OI    RNSW,RNACT          ACTIVITY SWITCH                              
         ZAP   CAMT,=PL6'99999999999'                                           
         SP    CAMT,CHTOT          COMPLIMENT FOR SORT                          
         ZAP   SRCCD,CHCSD         CASH DISCOUNT TOTAL                          
         ZAP   CCSD,=PL6'99999999999'                                           
         SP    CCSD,CHCSD                                                       
         CP    SRCAMT,=P'0'        TEST ZERO CHECK                              
         BNE   BSR03                                                            
         CLI   MODE,REQLAST        AT REQLAST                                   
         BNE   BSR03                                                            
         OI    SRSTAT,SRSZRO       SET ZERO REQUEST                             
         B     BSR20                                                            
*                                  BUILD THE REQUEST SORT FIELDS                
BSR03    LA    R4,SORTOPT+1        R4 = REQUESTED SORT OPTIONS                  
         LA    R0,L'SORTOPT-1      R0 = NUMBER OF OPTIONS                       
         LA    RF,SRVAR                                                         
*                                                                               
BSR05    CLI   0(R4),0             END OF SORT FIELDS FOR THIS OPTION           
         BE    BSR15                                                            
         L     R1,ASORTFLD         R1 = SORT FIELD DEFINITION                   
*                                                                               
BSR07    CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                INVALID SORT FIELD OPTION                    
         CLC   0(1,R4),0(R1)       MATCH OPTION TO SORT FIELD                   
         BE    BSR09                                                            
         LA    R1,L'SORTFLD(R1)                                                 
         B     BSR07                                                            
*                                                                               
BSR09    SR    R3,R3                                                            
         IC    R3,1(R1)            LENGTH OF DATA                               
         BCTR  R3,0                                                             
         MVC   BSR13+4(2),2(R1)    SET DATA SOURCE IN MOVE                      
         EX    R3,*+4                                                           
BSR13    MVC   0(0,RF),0           DATA TO SORT FIELD                           
         LA    RF,1(R3,RF)         RF TO NEXT OUTPUT FIELD                      
         LA    R4,1(R4)            NEXT SORT OPTION FIELD                       
         BCT   R0,BSR05                                                         
*                                                                               
BSR15    L     R3,ATRNIO           A(TRANSACTION RECORD)                        
         USING TRNRECD,R3                                                       
         MVC   SRCLI,PTNCLI        CLIENT                                       
         MVC   SRSEQ,PTNSEQ        SEQUENCE                                     
         MVC   SR#INV,PTN#INV      NUMBER OF ITEMS                              
         MVC   SRACC,TRNKCULA      ACCOUNT                                      
         MVC   SRKOFC,TRNKOFF      OFFICE                                       
         MVC   SRCNTR,TRNKCULC     CONTRA                                       
         MVC   SRTDTE,TRNKDATE     DATE                                         
         MVC   SRTREF,TRNKREF      REFERENCE                                    
         MVC   SRTSBR,TRNKSBR      SUBREFRENCE                                  
         MVC   CLNT,TRNKCULC+12    GET THE CLIENT CODE                          
         TM    SYEQU,PRDQ+EXPQ     IS IT PRODUCTION OR EXPENSE                  
         BZ    *+10                                                             
         MVC   CLNT,TRNKCULC+3                                                  
         MVC   SRTCLIC,CLNT        CLIENT CODE                                  
         LA    R2,TRNKEY+ACCORFST  TRANSACTION ELEMENT                          
         USING TRNELD,R2                                                        
         ZAP   SRTAMT,TRNAMNT      PAYABLE AMOUNT                               
         BAS   RE,GETCD                                                         
         ZAP   SRTCD,CD            CASH DISCOUNT                                
         MVC   SRTANL,TRNANAL      OFFICE FROM TRANSACTION                      
         MVI   SRTNRL,0            NARRATIVE LENGTH                             
         MVI   SRTNRR,C' '                                                      
         MVC   SRTNRR+1(L'SRTNRR-1),SRTNRR                                      
         SR    R1,R1                                                            
         IC    R1,TRNLN                                                         
         SH    R1,=Y(TRNLN1Q)                                                   
         BNP   BSR17                                                            
                                                                                
         CLI   PROGPROF+9,C'N'                                                  
         BE    BSR17                                                            
                                                                                
         STC   R1,SRTNRL           NARRATIVE LENGTH                             
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   SRTNRR(0),TRNNARR   AND NARRATIVE                                
         CLC   SRTNRR(L'SPACES),SPACES                                          
         BNE   *+8                                                              
         MVI   SRTNRL,0                                                         
*                                                                               
BSR17    MVC   SRCNME,PTNCNM       CONTRA NAME                                  
         MVC   SRCNML,PTNCLN       AND LENGTH                                   
         MVC   SRTYPE,CHKTYPE      ACCOUNT TYPE                                 
         BAS   RE,SETL             SET ELEMENT DATA                             
         MVI   SRSTAT,0                                                         
         TM    RQSW,RQURG          URGENT                                       
         BNO   *+8                                                              
         OI    SRSTAT,SRSURG                                                    
         DROP  R2                                                               
*       - - - - - - - - - - - - -                                               
         L     R2,ADACCSTA         LOAD RSTELD ELEMENT                          
         USING RSTELD,R2                                                        
         CLI   RSTLN,RSTLN3Q       IS THIS NEW LENGTH?                          
         BL    BSR20               NO                                           
         TM    RSTMAIL,RSTMAIOV    IS THIS SPECIAL MAILING?                     
         BZ    *+8                                                              
         OI    SRSTAT,SRMAIL       SET IT IN THE SORT                           
         DROP  R2                                                               
*       - - - - - - - - - - - - -                                               
BSR20    GOTO1 ADSORTER,DMCB,=C'PUT',SREC                                       
         OI    SCS,SCSACT          SET ACTIVITY SWITCH                          
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ADD THE ADDRESS TO THE SORT RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
ADDR     NTR1  ,                                                                
         XC    SRADDR(4*L'SRADDR),SRADDR                                        
         ICM   R2,15,ADACCADD      A(ADDRESS ELEMENT)                           
         BZ    ADDR09              NO ADDRESS                                   
         USING ADRELD,R2                                                        
         SR    R0,R0                                                            
         ICM   R0,1,ADRNUM         NUMBER OF LINES                              
         BZ    ADDR09              NO ADDRESS                                   
         CLI   ADRNUM,1            ONLY ONE LINE                                
         BNE   *+14                                                             
         CLC   ADRADD1,SPACES      AND IT'S SPACES                              
         BE    ADDR09              NO ADDRESS                                   
         LA    R5,SRADDR                                                        
         LA    R4,ADRADD1                                                       
*                                                                               
ADDR03   MVC   0(L'SRADDR,R5),0(R4) UP TO 4 LINES OF ADDRESS                    
         LA    R4,L'ADRADD1(R4)                                                 
         LA    R5,L'SRADDR(R5)                                                  
         BCT   R0,ADDR03                                                        
         B     XIT                                                              
*                                                                               
ADDR09   AP    NOADD,=P'1'         NO ADDRESS                                   
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SET DATA FROM OTHER ELEMENTS                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R3                                                       
SETL     NTR1  ,                                                                
         XC    SROTH(SROTHL),SROTH    CLEAR OTHER ELEMENT DATA                  
         XC    SRXPY(SRXPYL),SRXPY    EXTRA DATA                                
         XC    SRMTD(SRMTDL),SRMTD    AND MEDIA TRANSFER                        
         XC    SRXMOS,SRXMOS          CLEAR MONTH OF SERVICE                    
         MVC   SRXAC,SPACES           EXPENSE ACCOUNT                           
         ZAP   SRTGST,=P'0'                                                     
         ZAP   SRTPST,=P'0'                                                     
         ZAP   SRTGRS,=P'0'                                                     
         SR    R0,R0                                                            
         LA    R4,TRNKEY+ACCORFST                                               
*                                                                               
SETL3    IC    R0,1(R4)            BUMP THRU RECORD                             
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    SETLX                                                            
*                                                                               
         CLI   0(R4),X'23'         OTHERS ELEMENT                               
         BNE   SETL5                                                            
         USING OTHELD,R4                                                        
         MVC   SROPRD(6),OTHNUM    UP TO 6 PRODUCT/ESTIMATE                     
         MVC   SROJOB,OTHNUM+6     UP TO 6 BYTE JOB                             
         CLI   OTHDATE,C' '                                                     
         BNH   SETL3                                                            
         CLI   OTHDATE,C'0'                                                     
         BNL   SETL3                                                            
         MVC   SROADV,OTHDATE      MOS                                          
         B     SETL3                                                            
         DROP  R4                                                               
*                                                                               
SETL5    CLI   0(R4),X'46'         EXTRA PAY ELEMENT                            
         BNE   SETL7                                                            
         USING XPYELD,R4                                                        
         ZAP   SRXCD,XPYCD         CASH DISCOUNT                                
         MVC   SRXCLI,XPYCLI       CLIENT NAME                                  
         MVC   SRXPRD,XPYPRO       PRODUCT NAME                                 
         MVC   SRXINV,XPYINV       INVOICE NUMBER                               
         MVC   SRXPER,XPYPER       PERIOD                                       
         MVC   SRXEST,XPYEST       ESTIMATE NUMBER                              
         CLI   XPYLN,XPYLN2Q                                                    
         BL    *+10                                                             
         MVC   SRXINVD,XPYDATE     INVOICE DATE                                 
         B     SETL3                                                            
         DROP  R4                                                               
*                                                                               
SETL7    CLI   0(R4),X'1A'         MEDIA TRANSFER                               
         BNE   SETL8                                                            
         USING MDTELD,R4                                                        
         CLI   MDTMOS,C' '                                                      
         BNH   SETL3                                                            
         CLI   MDTMOS,C'0'                                                      
         BNL   SETL3                                                            
         MVC   SRMADV,MDTMOS                                                    
         B     SETL3                                                            
         DROP  R4                                                               
*                                                                               
SETL8    CLI   0(R4),X'50'         GST                                          
         BNE   SETL9                                                            
         USING SCIELD,R4                                                        
         CLI   SCITYPE,C'T'                                                     
         BNE   *+14                                                             
         ZAP   SRTGST,SCIAMNT                                                   
         B     SETL3                                                            
         CLI   SCITYPE,C'Q'                                                     
         BNE   *+14                                                             
         ZAP   SRTPST,SCIAMNT                                                   
         B     SETL3                                                            
         CLI   SCITYPE,C'E'                                                     
         BNE   SETL3                                                            
         ZAP   SRTGRS,SCIAMNT                                                   
         B     SETL3                                                            
         DROP  R4                                                               
*                                                                               
SETL9    CLI   0(R4),X'BA'         STUDIO JOB                                   
         BNE   SETL10                                                           
         USING LNKELD,R4                                                        
         MVC   SROPRD,LNKAGJB+3                                                 
         MVC   SROJOB,LNKAGJB+6                                                 
         B     SETL3                                                            
         DROP  R4                                                               
*                                                                               
SETL10   CLI   0(R4),X'4C'         EXPENSE ACCOUNT(PROD X-JOB)                  
         BNE   SETL11                                                           
         USING SPDELD,R4                                                        
         CLC   SPDACCS(2),=C'SE'                                                
         BNE   SETL3                                                            
         SR    R1,R1                                                            
         IC    R1,SPDLN                                                         
         SH    R1,=H'5'            SKIP CODE/LEN/UL                             
         BM    SETL3                                                            
         EX    R1,*+4                                                           
         MVC   SRXAC(0),SPDACCS+2                                               
         B     SETL3                                                            
         DROP  R3,R4                                                            
                                                                                
SETL11   CLI   0(R4),X'60'         TRANSACTION STATUS ELEMENT                   
         BNE   SETL3                                                            
         USING TRSELD,R4                                                        
         MVC   SRXMOS,TRSPMOS                                                   
         B     SETL3                                                            
         DROP  R4                                                               
                                                                                
*                                                                               
SETLX    MVC   SRADV,SRMADV        SET MOS FROM 1A ELEMENT                      
         OC    SRADV,SRADV         ADVERTISING MONTH FROM 1A ELEMENT            
         BNZ   *+10                                                             
         MVC   SRADV,SROADV        IF NONE, USE OTHER MOS                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET TRANSACTION CASH DISCOUNT                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R3                                                       
GETCD    NTR1  ,                                                                
         ZAP   CD,=P'0'            C.D.                                         
         SR    R0,R0                                                            
         LA    R4,TRNKEY+ACCORFST                                               
*                                                                               
GETCD3   IC    R0,1(R4)            BUMP THRU RECORD                             
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    XIT                                                              
*                                                                               
GETCD5   CLI   0(R4),X'46'         EXTRA PAY ELEMENT                            
         BNE   GETCD7                                                           
         USING XPYELD,R4                                                        
         ZAP   CD,XPYCD                                                         
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
GETCD7   CLI   0(R4),X'50'         SUBSIDIARY CASH                              
         BNE   GETCD3                                                           
         USING SCIELD,R4                                                        
         CLI   SCITYPE,C'D'        IS IT CASH DISCOUNT                          
         BNE   GETCD3                                                           
         ZAP   CD,SCIAMNT                                                       
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* GET SORT RECORDS AND PRINT THE CHECKS                               *         
***********************************************************************         
         SPACE 1                                                                
GETS     NTR1  ,                                                                
         MVI   MCSW,ACF+SBF        SET MODE STACK                               
         TM    SCS,SCSACT          ANY SORT ACTIVITY                            
         BO    GETS01                                                           
GETSX    GOTO1 ADSORTER,DMCB,=C'END'                                            
         B     XIT                                                              
*                                                                               
GETS01   TM    SCS,SCSEOF          HAS EOF BEEN SET                             
         BO    XIT                 ALL DONE                                     
         GOTO1 ADSORTER,DMCB,=C'GET'                                            
         ICM   R4,15,DMCB+4                                                     
         BNZ   GETS02                                                           
         OI    SCS,SCSEOF          SET SORT EOF                                 
         B     GETS03                                                           
*                                                                               
GETS02   L     R2,ANXTSR           R2 = A(SAVE AREA FOR NEXT)                   
         LA    R3,SRLNQ            R3 = SORT RECORD LENGTH                      
         LR    R5,R3                                                            
         MVCL  R2,R4               MOVE RECORD TO NEXT AREA                     
         TM    SCS,SCSREC          IS THERE A RECORD IN SREC                    
         BO    GETS03                                                           
         OI    SCS,SCSREC                                                       
         B     GETS30              MOVE THIS TO SREC AND GET ANOTHER            
*                                                                               
GETS03   TM    MCSW,ACF            IS IT ACCOUNT FIRST                          
         BNO   GETS11                                                           
         TM    SRSTAT,SRSZRO       SKIP ZERO REQUESTS                           
         BO    GETS30                                                           
         MVI   VOIDSW,C'N'                                                      
         ZAP   CURLNE,=P'0'        LINE COUNT FOR PRINT                         
         MVI   NARRWRK,C' '       CLEAR NARRATIVE WORK AREA                     
         MVC   NARRWRK+1(L'NARRWRK-1),NARRWRK                                   
         MVC   CLISAVE,SPACES                                                   
         MVC   INVNO,SPACES                                                     
         MVC   SYENT,SRSYENT                                                    
         MVC   TYENT,SRTYENT                                                    
         MVC   CHKTYPE,SRTYPE                                                   
         MVC   RCSUBPRG,TYSBP      SET SPROG FOR HEADS                          
                                                                                
         CLI   QLANG,LANGFRE                                                    
         BNE   GETS08                                                           
         ZIC   RF,RCSUBPRG                                                      
         AH    RF,=H'7'                                                         
         STC   RF,RCSUBPRG                                                      
                                                                                
GETS08   SR    RF,RF                                                            
         ICM   RF,7,TYRTN          DISPLACEMENT TO OVERLAY                      
         AR    RF,RB                                                            
         TM    FTPSW,FTPTRN        FILE TRANSFER                                
         BNO   *+8                                                              
         L     RF,AEDTRN           A(EDICT TRANSFER)                            
         ST    RF,ASBRTN           A(SUB-ROUTINE)                               
         MVI   MODE,ACCFRST                                                     
         BASR  RE,RF               PASS ACCFRST                                 
         NI    MCSW,ALL-ACF                                                     
*                                                                               
GETS11   TM    MCSW,SBF            IS IT FIRST FOR SUBACCOUNT                   
         BNO   GETS13                                                           
         L     RF,ASBRTN                                                        
         MVI   MODE,SBACFRST                                                    
         BASR  RE,RF                                                            
         NI    MCSW,ALL-SBF                                                     
*                                                                               
GETS13   L     RF,ASBRTN                                                        
         MVI   MODE,PROCTRNS       SET TO PROCESS TRANSACTIONS                  
         BASR  RE,RF                                                            
         CLC   ALPHAID,BOCLARO     GO & DO BOCLAROS TAPE                        
         BNE   GETS15                                                           
         GOTO1 ABOCLARO                                                         
*                                                                               
GETS15   AP    RUNTOT,SRTAMT                                                    
         AP    SUBTOT,SRTAMT                                                    
         ZAP   SUBSAVE,SRTAMT      FOR CLIENT TOTAL SUPPRESSION                 
         AP    CHTOT,SRTAMT                                                     
         CLI   VOIDSW,C'Y'         IF VOID WRITTEN IN OVERLAY                   
         BNE   GETS17              WRITE WORKER RECORD HERE                     
         GOTO1 APOST                                                            
*                                                                               
GETS17   GOTO1 APOST                                                            
         LA    R3,SREC             R3 = RECORD JUST PROCESSED                   
         L     R4,ANXTSR           R4 = NEXT RECORD                             
         TM    SCS,SCSEOF          LAST RECORD                                  
         BO    *+14                                                             
         CLC   0(SRACLQ,R3),0(R4)  TEST SAME REQ/ACC/CLI/SEQ                    
         BE    GETS19                                                           
         BAS   RE,SBL              LAST SUBACCOUNT                              
         BAS   RE,ABL              LAST ACCOUNT                                 
         MVI   MCSW,ACF+SBF        SET MODE STACK                               
         B     GETS30                                                           
*                                                                               
GETS19   CLC   0(SRCNLQ,R3),0(R4)  SAME CONTRA                                  
         BE    GETS30              SAME ACCOUNT AND CONTRA                      
         BAS   RE,SBL                                                           
         OI    MCSW,SBF            TURN ON FIRST FOR NEXT TIME                  
*                                                                               
GETS30   LA    R2,SREC             R2 = A(CURRENT RECORD)                       
         L     R4,ANXTSR           R4 = A(NEXT RECORD)                          
         LA    R3,SRLNQ            R3 = SORT RECORD LENGTH                      
         LR    R5,R3                                                            
         MVCL  R2,R4               MOVE RECORD TO NEXT AREA                     
         B     GETS01                                                           
         EJECT                                                                  
***********************************************************************         
* PROCESS THE SUB-ACCOUNT BREAKS                                      *         
***********************************************************************         
         SPACE 1                                                                
SBL      NTR1  ,                                                                
         TM    FTPSW,FTPTRN        FILE TRANSFER                                
         BO    SBL17                                                            
         CP    SUBTOT,=P'0'        DON'T BOTHER IF NO ACTIVITY                  
         BE    SBL17                                                            
         MVI   MODE,SBACLAST                                                    
         L     RF,ASBRTN                                                        
         BASR  RE,RF                                                            
         CLI   VOIDSW,C'Y'         WRITE A VOID RECORD- IF THE OVERLAY          
         BNE   SBL05               PRINTED A VOID CHECK                         
         GOTO1 APOST                                                            
*                                                                               
SBL05    TM    TYSTA,TYCLT         PRINT CLIENT TOTAL                           
         BNO   SBL17                                                            
         CP    SUBTOT,SUBSAVE      IF CLIENT TOTAL = LAST TRANSACTION           
         BNE   SBL09               DONT PRINT CLIENT TOTAL                      
         LA    RF,PRTLNE           TEST 1 LINE REMAINING                        
         CP    CURLNE,ML1LNE                                                    
         BL    *+8                                                              
         LA    RF,VDCK                                                          
         BASR  RE,RF               BLANK LINE OR NEW CHECK                      
         B     SBL17                                                            
*                                                                               
SBL09    LA    RF,PRTLNE           TEST THREE LINES REMAIN                      
         CP    CURLNE,ML3LNE       3 LINES NEEDED FOR SUB- TOTAL LINE           
         BL    *+8                                                              
         LA    RF,VDCK                                                          
         BASR  RE,RF               BLANK LINE OR NEW CHECK                      
         MVC   P+20(50),SUBTXT                                                  
         CP    SUBC1,=P'0'         NET PAYABLE(FOR PRINT)                       
         BE    SBL13                                                            
         CURED SUBC1,(11,P+48),2,MINUS=YES                                      
*                                                                               
SBL13    CP    SUBC2,=P'0'         CASH DISCOUNT-PRINT                          
         BE    SBL15                                                            
         CURED SUBC2,(9,P+64),2,MINUS=YES                                       
*                                                                               
SBL15    LA    RF,P+74                                                          
         CURED SUBTOT,(11,(RF)),2,MINUS=YES                                     
         BAS   RE,PRTLNE                                                        
         BAS   RE,PRTLNE           BLANK LINE AFTER TOTAL                       
*                                                                               
SBL17    LA    R0,NSBTOT           CLEAR SUB-ACCOUNT ACCUMULATORS               
         LA    R1,SBTOTS                                                        
         ZAP   0(L'SBTOTS,R1),=P'0'                                             
         LA    R1,L'SBTOTS(R1)                                                  
         BCT   R0,*-10                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS THE ACCOUNT BREAKS                                          *         
***********************************************************************         
         SPACE 1                                                                
ABL      NTR1  ,                                                                
         MVI   MODE,ACCLAST        RESET MODE TO ACCLAST                        
         L     RF,ASBRTN           GO TO OVERLAY                                
         BASR  RE,RF                                                            
         TM    FTPSW,FTPTRN        FILE TRANSFER                                
         BO    ABL21                                                            
         CLI   VOIDSW,C'Y'                                                      
         BNE   ABL01                                                            
         GOTO1 APOST                                                            
*                                                                               
ABL01    TM    TYSTA,TYCKT         SUB-ROUTINE HANDLES CHECK TOTAL              
         BO    ABL21                                                            
*                                                                               
ABL03    LA    RF,PRTLNE                                                        
         CP    CURLNE,ML2LNE       2 LINES FOR VENDOR TOTAL                     
         BL    *+8                                                              
         LA    RF,VDCK                                                          
         BASR  RE,RF               BLANK LINE OR NEW CHECK                      
         MVC   P+20(50),ACCTXT                                                  
         CP    CHC1,=P'0'          NET PAYABLE(PRINT)                           
         BE    ABL07                                                            
         CURED CHC1,(11,P+48),2,MINUS=YES                                       
*                                                                               
ABL07    ZAP   CHCSD,CHC2          CASH DISCOUNT FOR PROD                       
         TM    TYSTA,TYPCD         PRINT CASH DISCOUNT                          
         BNO   ABL11                                                            
         CP    CHC2,=P'0'          ANY C.D.                                     
         BE    ABL09                                                            
         CURED CHC2,(9,P+64),2,MINUS=YES                                        
ABL09    CURED CHTOT,(11,P+73),2,MINUS=YES                                      
         B     ABL15                                                            
*                                                                               
ABL11    CP    CHC2,=P'0'          PROD C.D.                                    
         BE    ABL13                                                            
         CURED CHC2,(9,P+65),2,MINUS=YES                                        
ABL13    CURED CHTOT,(12,P+73),2,MINUS=YES                                      
*                                                                               
ABL15    BAS   RE,PRTLNE                                                        
*                                                                               
ABL21    BAS   RE,PCK              PRINT THE CHECK                              
ABL23    CP    CHTOT,SRCAMT        DOES THE TOTAL MATCH                         
         BE    *+6                                                              
         DC    H'0'                A BUG                                        
         LA    R0,NACTOT           CLEAR ACCOUNT ACCUMULATORS                   
         LA    R1,ACTOTS                                                        
         ZAP   0(L'ACTOTS,R1),=P'0'                                             
         LA    R1,L'ACTOTS(R1)                                                  
         BCT   R0,*-10                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT THE LINEUP PAGES                                              *         
***********************************************************************         
         SPACE 1                                                                
LNUP     TM    RNSW,RNACT          ANY RUN ACTIVITY                             
         BZR   RE                  SKIP LINE-UPS                                
         NTR1  ,                                                                
         TM    FTPSW,FTPTRN        FILE TRANSFER?                               
         BO    LNUP03                                                           
         TM    RNSW,MICR           MICR                                         
         BO    LNUP03                                                           
         CLI   TYPCHK,TYPDDS       PRINTED AT DDS                               
         BNE   LNUP03                                                           
         TM    PRNTSW,SHUTTLE      IS PRINT TO BE SHUTTLED?                     
         BO    *+10                NO OPERATOR VOIDS FOR SHUTTLE                
         AP    CHNUM,=P'2'                                                      
         BAS   RE,LNUP12           TWO LINEUPS WITH VOIDS                       
         ZAP   CURLNE,=P'0'        CURRENT LINE NUMBER                          
         BAS   RE,LNUP12                                                        
         B     XIT                                                              
*                                                                               
LNUP03   SP    CHNUM,=P'1'                                                      
         LA    R0,2                                                             
LNUP05   MVI   VOIDSW,C'Y'         TWO VOID POSTINGS FOR REGISTER               
         GOTO1 APOST               WITHOUT LINEUPS                              
         BCT   R0,LNUP05                                                        
*                                                                               
         TM    FTPSW,FTPTRN        FILE TRANSFER                                
         BO    XIT                                                              
         TM    RNSW,MICR           MICR                                         
         BO    LNUP07                                                           
         SR    R3,R3                                                            
         ICM   R3,1,PROGPROF+6     NUMBER OF LINE-UP VOIDS                      
         BZ    XIT                                                              
         ZAP   CURLNE,=P'0'        CURRENT LINE NUMBER                          
         BAS   RE,LNUP12           PRINT THE LINE-UPS                           
         BCT   R3,*-10                                                          
         B     XIT                                                              
*                                                                               
LNUP07   TM    PRNTSW,LASER        TEST LASER                                   
         BNO   XIT                                                              
         MVC   P(132),SPACES       SKIP TO CHANNEL 1                            
         GOTO1 PRINT,DMCB,P,=C'BC01'                                            
         B     XIT                                                              
*                                                                               
LNUP12   LR    R0,RE                                                            
         MVC   P(132),SPACES                                                    
         GOTO1 PRINT,DMCB,P,=C'BC01'                                            
         MVI   P+1,C'X'                                                         
         MVC   P+2(84),P+1                                                      
*                                                                               
         GOTO1 PRINT,DMCB,P,=C'BL01'   2 LINES OF XXXXXXX'S                     
         BASR  RE,RF                                                            
         MVC   P,SPACES                                                         
         BAS   RE,VDCK             VOID THE CHECK                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT A CHECK                                                       *         
***********************************************************************         
         SPACE 1                                                                
PCK      NTR1  ,                                                                
         TM    FTPSW,FTPTRN        FILE TRANSFER                                
         BNO   PCK01                                                            
         AP    CHNUM,=P'1'                                                      
         B     PCK31                                                            
*                                                                               
PCK01    CP    CHTOT,=P'0'         DON'T PRINT CHECK FOR ZERO OR MINUS          
         BH    PCK03                                                            
         BAS   RE,VDCK                                                          
         B     XIT                                                              
*                                                                               
PCK03    AP    CHOKS,=P'1'                                                      
         AP    CHNUM,=P'1'                                                      
         GOTO1 ADLN                                                             
         TM    PRNTSW,NUMBER       PRINT NUMBER ON CHECK                        
         BZ    PCK05                                                            
         GOTO1 PRINT,DMCB,P,=C'BL07'                                            
         UNPK  P+70(6),CHNUM                                                    
         OI    P+75,X'F0'                                                       
         CLI   NUMTYP,0                                                         
         BE    *+10                                                             
         MVC   P+70(1),NUMTYP                                                   
         GOTO1 (RF),(R1),,=C'BL01'                                              
         MVC   P,SPACES                                                         
         B     PCK07                                                            
*                                                                               
PCK05    GOTO1 PRINT,DMCB,P,=C'BL08'                                            
*                                                                               
PCK07    TM    PRNTSW,LASER      ANY LASER PRINTS AMOUNT IN WORDS               
         BO    *+12                                                             
         TM    SROPT,SROWRD      AMOUNT IN WORDS                                
         BNO   PCK13                                                            
         ZAP   DUB,CHTOT                                                        
         L     R2,AIO                                                           
         GOTO1 NUMTOLET,DMCB,(RCLANG,DUB),(C'P',(R2))                           
         A     R2,DMCB                                                          
         BCTR  R2,0                R2 TO LAST LETTER OR NUMBER                  
         CLI   0(R2),C'S'                                                       
         BNE   PCK11                                                            
         MVC   1(3,R2),=C' 00'                                                  
         L     R4,DMCB                                                          
         LA    R4,3(R4)                                                         
         ST    R4,DMCB             ADJUST LENGTH FOR 00 PENNIES                 
*                                                                               
PCK11    L     R2,AIO                                                           
         LR    R4,R2                                                            
         A     R4,DMCB                                                          
         MVC   0(4,R4),=C'/100'                                                 
         L     R4,DMCB                                                          
         LA    R4,4(R4)                                                         
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
*                                                                               
         GOTO1 CHOPPER,DMCB,((R4),(R2)),(55,P+17),(C'P',2)                      
         GOTO1 PRINT,DMCB,P,=C'BL01'                                            
         MVC   P,SPACES                                                         
         GOTO1 PRINT,DMCB,PSECOND,=C'BL01'                                      
         MVC   PSECOND,SPACES                                                   
         CLI   PROGPROF+5,0        SPECIAL FOR V+B                              
         BNE   PCK15               WILL CAUSE DATE TO PRINT TWO HIGHER          
         GOTO1 PRINT,DMCB,PSECOND,=C'BL02'                                      
         B     PCK15                                                            
*                                                                               
PCK13    GOTO1 PRINT,DMCB,P,=C'BL02'                                            
         CLI   PROGPROF+5,0        SPECIAL FOR V+B                              
         BNE   PCK15               WILL CAUSE DATE TO PRINT TWO HIGHER          
         GOTO1 PRINT,DMCB,P,=C'BL02'                                            
*                                                                               
PCK15    GOTO1 DATCON,DMCB,(1,SRCDTE),(8,P+6)                                   
*                                                                               
PCK17    CLI   PROGPROF+5,0                                                     
         BE    PCK19                                                            
         GOTO1 PRINT,DMCB,P,=C'BL03'  PRINT DATE IF OPTION ON                   
         MVC   P,SPACES                                                         
*                                                                               
PCK19    LA    RF,SRANME        PAYEE NAME                                      
         SR    R4,R4                                                            
         IC    R4,SRANML        LENGTH OF NAME                                  
         LA    R5,P+39                                                          
         CLI   PROGPROF+5,0                                                     
         BE    PCK21                                                            
         LA    R5,P                                                             
         ZIC   R0,PROGPROF+5                                                    
         BCTR  R0,0                                                             
         AR    R5,R0          GET COLUMN NUMBER FOR NAME AND ADDRESS            
*                                                                               
PCK21    GOTO1 CHOPPER,DMCB,((R4),0(RF)),(28,0(R5)),(C'P',2)                    
         MVC   BYTE,DMCB+11        NO OF LINES IN NAME                          
         CURED CHTOT,(13,P+68),2,COMMAS=YES                                     
                                                                                
         CLI   QLANG,LANGFRE                                                    
         BNE   *+8                                                              
         MVI   P+81,C'$'                                                        
                                                                                
         LA    R1,P+68             FILL WITH ***999.99                          
         CLI   0(R1),C' '                                                       
         BH    *+16                                                             
         MVI   0(R1),C'*'                                                       
         LA    R1,1(R1)                                                         
         B     *-16                                                             
*                                                                               
         GOTO1 PRINT,DMCB,P,=C'BL01'                                            
         MVC   P,SPACES                                                         
         CLI   BYTE,2                                                           
         BL    PCK23                                                            
         GOTO1 (RF),(R1),PSECOND                                                
         MVC   PSECOND,SPACES                                                   
*                                                                               
PCK23    LA    R0,4                PRINT THE ADDRESS                            
         LA    R4,SRADDR                                                        
*                                                                               
PCK25    OC    0(L'SRADDR,R4),0(R4)                                             
         BZ    PCK31                                                            
         MVC   0(L'SRADDR,R5),0(R4)                                             
         GOTO1 PRINT,DMCB,P,=C'BL01'                                            
         MVC   P,SPACES                                                         
         LA    R4,L'SRADDR(R4)                                                  
         BCT   R0,PCK25                                                         
*                                                                               
PCK31    GOTO1 APOST                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VOID A CHECK                                                        *         
***********************************************************************         
         SPACE 1                                                                
VDCK     NTR1  ,                                                                
         TM    FTPSW,FTPTRN        FILE TRANSFER                                
         BO    XIT                                                              
         AP    VOIDS,=P'1'         COUNT VOIDS                                  
         AP    CHNUM,=P'1'         INCREMENT THE CHECK NUMMER                   
         GOTO1 ADLN                PRINT THE DEADLINE AT END OF REMIT.          
         GOTO1 PRINT,DMCB,P,=C'BL03'                                            
         BAS   RE,VOID             PRINT THE VOID CHECK                         
         BAS   RE,NEWPGE           FORCE NEW PAGE                               
*                                                                               
         TM    RNSW,MICR                                                        
         BNO   *+16                                                             
         SP    VOIDS,=P'1'         COUNT VOIDS                                  
         SP    CHNUM,=P'1'         INCREMENT THE CHECK NUMMER                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT THE VOID CHECK                                                *         
***********************************************************************         
         SPACE 1                                                                
VOID     NTR1  ,                                                                
         MVC   P,SPACES                                                         
         GOTO1 PRINT,DMCB,P,=C'BL06'                                            
         LA    R2,P+39                                                          
         L     R5,APVOID                                                        
                                                                                
         CLI   QLANG,LANGFRE                                                    
         BNE   *+8                                                              
         L     R5,APNUL                                                         
                                                                                
         LA    R3,5                                                             
*                                                                               
VOID2    MVC   0(40,R2),0(R5)                                                   
         GOTO1 PRINT,DMCB,P,=C'BL01'                                            
         LA    R5,40(R5)                                                        
         BCT   R3,VOID2                                                         
*                                                                               
         MVC   P,SPACES                                                         
         GOTO1 PRINT,DMCB,P,=C'BL09'                                            
         LA    R3,2                                                             
         LA    R2,P+3                                                           
         LA    R4,20                                                            
*                                                                               
VOID8    MVC   0(L'AC@VOID,R2),AC@VOID       'VOID'                             
         LA    R2,4(R2)                                                         
         BCT   R4,VOID8                                                         
*                                                                               
VOID10   GOTO1 PRINT,DMCB,P,=C'BL01'                                            
         BCT   R3,VOID10                                                        
         MVC   P,SPACES                                                         
         CLI   VOIDSW,X'FF'                                                     
         BE    VOIDX          END OF RUN - THIS IS VOID FOR NEXT TIME           
         MVI   VOIDSW,C'Y'                                                      
         TM    RNSW,MICR                                                        
         BNO   VOID15                                                           
         MVI   VOIDSW,C'N'                                                      
         B     XIT                                                              
VOID15   GOTO1 APOST                                                            
VOIDX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* OFFICE  AND CLIENT FILTERING                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R3                                                       
FLTR     NTR1  ,                                                                
         BAS   RE,CLPF             GET CLIENT PROFILE                           
         L     R5,ACLIPRO          A(CLIENT PROFILE)                            
         USING PRFD,R5                                                          
*                                                                               
         USING RUNXTRAD,RF                                                      
         L     RF,VEXTRAS                                                       
         OC    VLISTREC,VLISTREC                                                
         BZ    FLTR03              NO LIST RECORD                               
         GOTO1 ACLIST,DMCB,VLISTREC,CLNT                                        
         CLI   DMCB,0                                                           
         BNE   *+6                                                              
         DC    H'0'                BAD LIST RECORD                              
         CLI   DMCB,C'E'                                                        
         BE    FLTRNO              EXCLUDE THIS CLIENT                          
         DROP  RF                                                               
*                                                                               
FLTR03   L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         CLI   QSELECT,C'-'        TEST NEGATIVE LIST                           
         BE    *+14                TREAT LIKE AN 'ALL' REQUEST                  
         OC    VLISTREC,VLISTREC                                                
         BNZ   FLTR05                                                           
         CLI   PRFXALL,C'Y'        EXCLUDE ALL MONTHS                           
         BNE   FLTR05                                                           
         CLI   CLNTF,X'C0'         HIGHER THAN 'C0' MEANS                       
         BH    FLTR05              A REQUEST FOR THIS CLIENT                    
         B     FLTRNO                                                           
*                                                                               
FLTR05   CLI   PRFX2MTH,C'A'       ALWAYS EXCLUDE 2 MONTS                       
         BE    FLTR07                                                           
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         CLI   QSELECT,C'-'        TEST NEGATIVE LIST                           
         BE    *+14                TREAT LIKE AN 'ALL' REQUEST                  
         OC    VLISTREC,VLISTREC                                                
         BNZ   FLTR09              LIST RECORD IS NOT ALL                       
         CLI   PRFX2MTH,C'Y'       DO IT ONLY WHEN REQUESTING ALL               
         BNE   FLTR09                                                           
         CLI   CLNTF,X'C0'                                                      
         BH    FLTR09              NOT REQUESTING ALL                           
FLTR07   CLC   TRNKDATE,LASTBRD                                                 
         BNL   FLTRNO                                                           
*                                                                               
FLTR09   CLI   PRFX1MTH,C'A'       ALWAYS EXCLUDE 1 MONTH                       
         BE    FLTR11                                                           
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         CLI   QSELECT,C'-'        TEST NEGATIVE LIST                           
         BE    *+14                TREAT LIKE AN 'ALL' REQUEST                  
         OC    VLISTREC,VLISTREC                                                
         BNZ   FLTR13                                                           
         CLI   PRFX1MTH,C'Y'       DO IT ONLY WHEN REQUESTING ALL               
         BNE   FLTR13                                                           
         CLI   CLNTF,X'C0'                                                      
         BH    FLTR13              NOT REQUESTING ALL                           
*                                                                               
FLTR11   CLC   TRNKDATE,THISBRD                                                 
         BNL   FLTRNO                                                           
*                                                                               
FLTR13   CLI   PRFI1MTH,C'A'       ALWAYS INCLUDE NEXT MONTH                    
         BE    FLTR15                                                           
         L     RF,VEXTRAS                                                       
         CLI   QSELECT,C'-'        TEST NEGATIVE LIST                           
         BE    *+14                TREAT LIKE AN 'ALL' REQUEST                  
         OC    VLISTREC,VLISTREC                                                
         BNZ   FLTR17                                                           
         CLI   PRFI1MTH,C'Y'       DO IT ONLY WHEN REQUESTING ALL               
         BNE   FLTR17                                                           
         CLI   CLNTF,X'C0'                                                      
         BH    FLTR17              NOT REQUESTING ALL                           
FLTR15   CLC   TRNKDATE,NEXTBRD                                                 
         BH    FLTRNO                                                           
         MVI   DATECK,C'N'         NOW SKIP ENDATE CHECK                        
*                                                                               
FLTR17   LA    R3,OFFLIST                                                       
FLTR19   CLI   0(R3),0             END OF OFFICE LIST                           
         BE    FLTR21                                                           
         CLC   0(1,R3),OFFICE                                                   
         BE    FLTR21                                                           
         TM    0(R3),X'40'                                                      
         BO    FLTRNO                                                           
         NI    OFFICE,ALL-X'40'                                                 
         CLC   0(1,R3),OFFICE                                                   
         BE    FLTRNO                                                           
         OI    OFFICE,X'40'                                                     
         LA    R3,1(R3)                                                         
         B     FLTR19                                                           
*                                                                               
FLTR21   LA    R3,CLILIST                                                       
FLTR25   CLI   0(R3),0             END OF CLIENT LIST                           
         BE    FLTRYES                                                          
         CLC   0(3,R3),CLNT                                                     
         BE    FLTRYES                                                          
         TM    0(R3),X'40'                                                      
         BO    FLTRNO                                                           
         NI    CLNT,ALL-X'40'                                                   
         CLC   0(3,R3),CLNT                                                     
         BE    FLTRNO                                                           
         OI    CLNT,X'40'                                                       
         LA    R3,3(R3)                                                         
         B     FLTR25                                                           
*                                                                               
FLTRNO   MVI   FILTER,C'N'                                                      
FLTRYES  B     XIT                                                              
         DROP  R3,R5,RF                                                         
         EJECT                                                                  
***********************************************************************         
* SPECIAL AGENCY SPECFIC CODE                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R3                                                       
*                                                                               
XFLTR    NTR1  ,                                                                
         TM    ALTXSW,CCXTRN       TEST COKE TRANSFER                           
         BNO   XFLTR2                                                           
         CLC   TRNKDATE,=X'940101' START WITH '94 DATA                          
         BL    XFLTRN                                                           
         B     XIT                                                              
*                                                                               
XFLTR2   TM    LGSW,CEXPN          TEST 'OLD' COKE SYSTEM                       
         BNO   XFLTR3                                                           
         CLC   TRNKDATE,=X'940101' END WITH '94 DATA                            
         BNL   XFLTRN                                                           
         B     XIT                                                              
*                                                                               
XFLTR3   CLC   ALPHAID,AMMIRATI    AMMIRATI NETWORK                             
         BNE   XIT                                                              
         CLI   QLEDGER,C'U'                                                     
         BNE   XIT                                                              
         CLC   TRNKDATE,=X'990401'                                              
         BNL   XFLTR4                                                           
         CLC   ORIGINUM,APLND      TEST TRANSFER ID                             
         BE    XFLTRN                                                           
         B     XIT                                                              
*                                                                               
XFLTR4   MVI   HALF,C'N'           SET ID - NOT MATCH                           
         CLC   ORIGINUM,APLND      TEST TRANSFER ID                             
         BNE   *+8                                                              
         MVI   HALF,C'Y'           MATCHES TRANSFER ID                          
         MVI   HALF+1,C'N'         SET CLIENT - NOT IN LIST                     
         LA    RF,PUCLITAB                                                      
XFLTR6   CLC   0(3,RF),CLNT        MATCH CLIENT TABLE                           
         BE    XFLTR7                                                           
         LA    RF,L'PUCLITAB(RF)                                                
         CLI   0(RF),X'FF'                                                      
         BNE   XFLTR6                                                           
         B     XFLTR8                                                           
XFLTR7   MVI   HALF+1,C'Y'         SET CLIENT IN LIST                           
XFLTR8   CLC   HALF(1),HALF+1                                                   
         BE    XIT                                                              
*                                                                               
XFLTRN   MVI   FILTER,C'N'                                                      
         B     XIT                                                              
*                                                                               
APLND    DC    AL2(7847)                                                        
PUCLITAB DS    0CL3                AMMIRATI CLIENTS FOR WESTERN FTP             
         DC    C'BKK'                                                           
         DC    C'LAB'                                                           
         DC    C'LGO'                                                           
         DC    C'RCA'                                                           
         DC    C'UPS'                                                           
         DC    X'FF'                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD THE PROFILE TABLE                                             *         
***********************************************************************         
         SPACE 1                                                                
PRFL     NTR1  ,                                                                
         OI    RNSW,PROF           SET PROFILES BUILT                           
         L     R5,APROF            A(PROFILE TABLE)                             
         USING PRFD,R5                                                          
         XC    PRFCOMP(L'PRFLEN),PRFCOMP                                        
         MVC   PRFCOMP,QCOMPANY                                                 
         LA    R1,L'PRFLEN(R5)                                                  
         MVI   0(R1),X'FF'         END OF TABLE                                 
         XC    CNTFK,CNTFK                                                      
         LA    R3,CNTFK                                                         
         USING CTUREC,R3                                                        
         MVI   CTUKTYP,C'U'                                                     
         MVI   CTUKSYS,C'A'                                                     
         MVC   CTUKPROG+1(2),=C'CK'                                             
         MVC   CTUKAGY,ALPHAID                                                  
         L     R4,AIO                                                           
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,(R3),(R4)                             
         CLC   CTUKEY,0(R4)                                                     
         BNE   XIT                                                              
         BAS   RE,PRFV             COMPANY PROFILES                             
         MVC   CTUKUNT(2),QUNIT                                                 
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,(R3),(R4)                             
         CLC   CTUKEY,0(R4)                                                     
         BNE   XIT                                                              
         BAS   RE,PRFV             UNIT/LEDGER PROFILES                         
*                                                                               
PRFL03   GOTO1 DATAMGR,DMCB,DMRSEQ,CTFILE,(R4),(R4)                             
         CLC   0(CTUKACT-CTUKEY,R3),0(R4)                                       
         BNE   XIT                                                              
         BAS   RE,PRFV             CLIENT PROFILES                              
         B     PRFL03                                                           
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* EXTRACT PROFILE VALUE                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING CTUREC,R3                                                        
         USING PRFD,R5                                                          
PRFV     LA    R2,28(R4)           R2 TO FIRST ELEMENT                          
PRFV03   CLI   0(R2),0                                                          
         BER   RE                                                               
         CLI   0(R2),X'72'                                                      
         BE    PRFV05                                                           
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     PRFV03                                                           
*                                                                               
         USING CTPVD,R2                                                         
PRFV05   MVC   PRFVAL,CTPVALUE      PROFILE VALUES                              
         LR    R3,R4                                                            
         MVC   PRFCOMP,RCCOMPFL     COMPANY                                     
         MVC   PRFLEDG(2),CTUKUNT   UNIT/ LEDGER                                
         MVC   PRFCLI,CTUKACT       CLIENT(ACCOUNT)                             
         LA    R3,CNTFK                                                         
         LA    R5,L'PRFLEN(R5)                                                  
         XC    PRFCOMP(L'PRFLEN),PRFCOMP                                        
         MVI   PRFCOMP,X'FF'                                                    
         BR    RE                                                               
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
***********************************************************************         
* GET PROFILE FOR CLIENT/LEDGER/COMPANY                               *         
***********************************************************************         
         SPACE 1                                                                
CLPF     NTR1  ,                                                                
         USING PRFD,R5                                                          
         L     R5,APROF             A(PROFILE TABLE)                            
*                                                                               
CLPF03   ST    R5,ACLIPRO           A(CLIENT PROFILE)                           
CLPF05   LA    R5,L'PRFLEN(R5)                                                  
         CLI   PRFCOMP,X'FF'                                                    
         BE    XIT                  END OF TABLE                                
         CLC   PRFCOMP(3),QCOMPANY                                              
         BNE   XIT                                                              
         OC    PRFCLI,PRFCLI                                                    
         BZ    CLPF03               UNIT/LEDGER PROFILE                         
         CLC   PRFCLI,CLNT                                                      
         BNE   CLPF05                                                           
         ST    R5,ACLIPRO                                                       
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* GET OFFICE-CHECK NUMBER AND LEDGER LOCK STATUS                      *         
***********************************************************************         
         SPACE 1                                                                
OFNM     NTR1  ,                                                                
         MVI   FTPSW,0             INIT FILE TRANSFER SWITCH                    
         XC    OFFLIST,OFFLIST     INIT OFFICE                                  
         XC    CLILIST,CLILIST     AND CLIENT LISTS                             
         LA    R3,OFFLIST                                                       
         LA    R5,CLILIST                                                       
         L     R4,ADLEDGER                                                      
*MN      NI    STATUS,X'FF'-CHKAUTH                                             
*                                                                               
         USING CHARECD,RF          CHECK AUTHORIZATION RECORD                   
         L     RF,AIO                                                           
         MVC   CHAKEY,SPACES                                                    
         MVI   CHAKTYP,CHAKTYPQ    X'10'                                        
         MVC   CHAKCULA,0(R4)      C/U/L                                        
         DROP  RF                                                               
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,AIO,AIO                               
         CLI   8(R1),0                                                          
         BNE   OFNM15                                                           
*MN      BNE   *+12                                                             
         L     R4,AIO                                                           
         AH    R4,DATADISP                                                      
*MN      OI    STATUS,CHKAUTH                                                   
*                                                                               
OFNM03   CLI   0(R4),X'54'                                                      
         BE    OFNM05                                                           
         CLI   0(R4),0                                                          
         BE    OFNM15                                                           
OFNM04   SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     OFNM03                                                           
*                                                                               
         USING OCNELD,R4                                                        
OFNM05   L     R1,ADOFFALD                                                      
         LTR   R1,R1                                                            
         BZ    OFNM06                                                           
         USING OFFALD,R1                                                        
         TM    OFFACST4,X'01'      TEST NEW OFFICES IN USE                      
         BZ    OFNM06                                                           
         CLI   OCNFILT,C'*'        OFFICE FILTERS NOT ALLOWED                   
         BNE   OFNM06                                                           
         DC    H'0'                                                             
*                                                                               
OFNM06   CLC   ORIGINUM,OCNOFFID   MATCH ID                                     
         BNE   OFNM11                                                           
         MVI   TYPCHK,TYPDDS       DEFAULT TYPE IS DDS                          
         TM    OCNSTAT,OCNSLOCL    TEST LOCAL CHECKS                            
         BNO   *+8                                                              
         MVI   TYPCHK,TYPLOCL      SET LOCAL                                    
         TM    RNSW,SOON                                                        
         BNO   *+8                                                              
         MVI   TYPCHK,TYPSOON      OR SOON                                      
         TM    OCNSTAT,OCNSPRTN    PRINT NUMBER ON CHECK                        
         BZ    *+8                                                              
         OI    PRNTSW,NUMBER                                                    
         TM    OCNSTAT,OCNSSHUT    PRINTING ON DDS-SHUTTLE                      
         BZ    *+8                                                              
         OI    PRNTSW,SHUTTLE                                                   
         CLI   OCNLN,OCNLN3Q                                                    
         BL    OFNM07                                                           
         CLI   OCNLASR,0           TEST LASER CHECKS (DDS)                      
         BE    *+8                                                              
         OI    PRNTSW,LASER                                                     
         TM    OCNSTAT2,OCNSLBLT   LASER (BOTTOM LINE TECHNOLOGY)               
         BZ    *+8                                                              
         OI    PRNTSW,LASER                                                     
         TM    PRNTSW,LASER        ANY LASER ARE TREATED LIKE MICR              
         BZ    *+8                                                              
         OI    RNSW,MICR                                                        
         TM    OCNSTAT2,OCNSMICR   TEST LOCAL CHECKS                            
         BNO   *+8                                                              
         OI    RNSW,MICR                                                        
         TM    OCNSTAT2,OCNSFTP    FILE TRANSFER                                
         BZ    *+12                                                             
         OI    FTPSW,FTPTRN        SET FOR FILE TRANSFER                        
         OI    RNSW,MICR           SET MICRO FOR AUTO REGISTER                  
         TM    OCNSTAT2,OCNS820    820 DATA TRANSFER                            
         BZ    *+12                                                             
         OI    FTPSW,FTPTRN        SET FOR FILE TRANSFER                        
         OI    RNSW,MICR           SET MICRO FOR AUTO REGISTER                  
*                                                                               
OFNM07   PACK  CHNUM,OCNAFT                                                     
         TM    RNSW,RERNY          RERUN = Y                                    
         BZ    *+10                                                             
         PACK  CHNUM,OCNBEF                                                     
         MVC   NUMTYP,OCNNTYP                                                   
         L     R2,ASORTS           GET SORT OPTION                              
         MVC   SORTOPT,0(R2)       SET THE DEFAULT                              
         CLI   OCNLN,OCNLN3Q                                                    
         BL    OFNM09                                                           
         MVC   POOFFC,OCNPOFF      POSTING OFFICE                               
         OC    POOFFC,SPACES                                                    
*                                                                               
OFNM08   CLI   0(R2),X'FF'         END OF SORT OPTION LIST                      
         BE    OFNM09                                                           
         CLC   OCNSORT,0(R2)                                                    
         BE    *+12                                                             
         LA    R2,L'SORTS(R2)                                                   
         B     OFNM08                                                           
         MVC   SORTOPT,0(R2)       SET THE SORT LIST                            
*                                                                               
OFNM09   CLI   OCNFILT,C' '        IF NO FILTER                                 
         BNH   OFNM04              CONTINUE EXCLUDE LIST                        
         CLI   OCNFILT,C'*'                                                     
         BNE   OFNM10              NOT OFFICE MUST BE CLIENT                    
         MVC   0(1,R3),OCNFILT+1   OFFICE CODE TO LIST                          
         LA    R3,1(R3)                                                         
         B     OFNM04                                                           
*                                                                               
OFNM10   MVC   CLILIST(3),OCNFILT  SAVE CLIENT FILTER                           
         XC    CLILIST+3(3),CLILIST+3                                           
         XC    OFFLIST,OFFLIST     IF CLIENT SPECIFIC, NO NEED TO               
         B     OFNM15              TO CHECK THE OFFICE                          
*                                                                               
OFNM11   CLI   OCNFILT,C'*'                                                     
         BNE   OFNM13              MUST BE CLIENT FILTER                        
         MVC   0(1,R3),OCNFILT+1   OFFICE CODE TO LIST                          
         NI    0(R3),ALL-X'40'                                                  
         LA    R3,1(R3)                                                         
         B     OFNM04                                                           
*                                                                               
OFNM13   CLI   OCNFILT,C' '        BUILD CLIENT EXCLUDE LIST                    
         BNH   OFNM04                                                           
         MVC   0(3,R5),OCNFILT                                                  
         NI    0(R5),ALL-X'40'                                                  
         LA    R5,3(R5)                                                         
         B     OFNM04                                                           
*                                                                               
OFNM15   B     XIT                                                              
         DROP  R1,R4                                                            
         EJECT                                                                  
***********************************************************************         
* GET ID NAME                                                         *         
***********************************************************************         
         SPACE 1                                                                
IDNM     NTR1  ,                                                                
         MVC   IDABBR,SPACES       GET LOGO FOR ORIGIN ID                       
         MVC   POWCODE,ALPHAID     DEFAULT FOR POWER CODE IS ALPHA              
         XC    CNTFK,CNTFK                                                      
         USING CTIREC,R3                                                        
         LA    R3,CNTFK                                                         
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),ORIGINUM                                             
         L     R4,AIO                                                           
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,(R3),(R4)                             
         CLC   CTIKEY,0(R4)                                                     
         BNE   XIT                                                              
         LA    R2,28(R4)                                                        
         SR    R0,R0                                                            
         DROP  R3                                                               
*                                                                               
IDNM3    CLI   0(R2),0                                                          
         BE    XIT                                                              
         CLI   0(R2),X'02'                                                      
         BE    IDNM5                                                            
         CLI   0(R2),X'30'                                                      
         BE    IDNM7                                                            
*                                                                               
IDNM4    IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     IDNM3                                                            
*                                                                               
         USING CTDSCD,R2                                                        
IDNM5    SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         SH    R1,=H'3'                                                         
         CH    R1,=H'6'                                                         
         BNH   *+8                                                              
         LA    R1,6                                                             
         EX    R1,*+4                                                           
         MVC   IDABBR(0),CTDSC                                                  
         B     IDNM4                                                            
*                                                                               
         USING CTDSTD,R2                                                        
IDNM7    MVC   POWCODE,CTDSTPOW      USE POWER CODE                             
         B     IDNM4                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET SYSTEM NUMBER                                                   *         
***********************************************************************         
         SPACE 1                                                                
SYS      NTR1  ,                                                                
         MVI   BYTE,0                                                           
         L     R2,AIO                                                           
         USING CTWREC,R2                                                        
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'S'                                                     
         MVI   CTWKSYSN,CTWKACC                                                 
         GOTO1 DATAMGR,DMCB,DMREAD,CTFILE,CTWREC,CTWREC                         
         CLI   8(R1),0                                                          
         BNE   XIT                                                              
*                                                                               
         SR    R0,R0                                                            
         LA    R1,CTWDATA                                                       
         USING SYSELD,R1           LOCATE SYSTEM ELEMENT FOR SENO               
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
SYS3     CLI   SYSEL,0             TEST E-O-R                                   
         BE    XIT                                                              
         CLI   SYSEL,SYSELQ        TEST SYSTEM ELEMENT                          
         BNE   SYS5                                                             
         CLC   SYSSEN,MCIDSENO     TEST SE NUMBER                               
         BNE   SYS5                                                             
         LA    RE,SYSNAME+L'SYSNAME-1                                           
         CLI   0(RE),C' '          LOCATE LAST CHARACTER OF NAME                
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVC   BYTE,0(RE)          THIS IS THE ACC FILE NUMBER                  
         B     XIT                                                              
*                                                                               
SYS5     IC    R0,SYSLEN           BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     SYS3                                                             
         DROP  R1,R2,RF                                                         
         EJECT                                                                  
***********************************************************************         
* SPOT STATION                                                        *         
***********************************************************************         
         SPACE 1                                                                
SPS      NTR1  ,                                                                
         CLI   MODE,ACCFRST        FIRST FOR ACCOUNT                            
         BNE   SPS5                                                             
         MVC   SUBTXT,SPACES                                                    
         MVC   ACCTXT,SPACES                                                    
         MVC   ACCTXT+33(L'TXT1),TXT1 CHECK TOTAL                               
         ZAP   CURLNE,=P'0'                                                     
         B     XIT                                                              
*                                  PROCESS TRANSACTION                          
SPS5     CLI   MODE,PROCTRNS                                                    
         BNE   SPS30                                                            
         CP    CURLNE,=P'0'                                                     
         BNE   SPS7                                                             
         BAS   RE,SPSHEAD          HEADUP A REMITTANCE                          
*                                                                               
SPS7     CP    CURLNE,MAXLNE                                                    
         BL    SPS9                                                             
         BAS   RE,VDCK                                                          
         BAS   RE,SPSHEAD          HEADUP A REMITTANCE                          
*                                                                               
SPS9     CLC   P+23(51),SPACES     DO WE HAVE A LINE FROM LAST TIME?            
         BE    *+8                                                              
         BAS   RE,SPPRN            IF SO-PRINT IT & BUILD NEXT                  
         BAS   RE,SPDTL            BUILD DETAIL LINE                            
         B     XIT                                                              
*                                  LAST FOR THE SUBACCOUNT                      
SPS30    CLI   MODE,SBACLAST                                                    
         BNE   XIT                                                              
         CURED SUBTOT,(11,P+74),2,MINUS=YES                                     
         BAS   RE,SPPRN            PRINT LAST TRANSACTION + CLI TOTAL           
         ZAP   SUBTOT,=P'0'                                                     
         B     XIT                                                              
*                                  ROUNTINE TO HEAD A NEW REMITTANCE            
         DC    F'0'                                                             
SPSHEAD  ST    RE,SPSHEAD-4                                                     
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,SRACC+4          R4 TO STATION                                
         LA    R2,HEAD4            R2 TO HEADLINE AREA                          
         BAS   RE,SETSTA           SET STATION HEADLINE                         
         ZAP   CURLNE,=P'4'                                                     
         L     RE,SPSHEAD-4                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SPOT REP                                                            *         
***********************************************************************         
         SPACE 1                                                                
SPR      NTR1  ,                                                                
         CLI   MODE,ACCFRST                                                     
         BNE   SPR3                                                             
         MVC   SUBTXT,SPACES                                                    
         MVC   ACCTXT,SPACES                                                    
         MVC   ACCTXT+33(L'TXT1),TXT1      CHECK TOTAL                          
         ZAP   STATOT,=P'0'                                                     
         ZAP   CURLNE,=P'0'                                                     
         XC    SVSTA,SVSTA                                                      
         B     XIT                                                              
*                                                                               
SPR3     CLI   MODE,PROCTRNS                                                    
         BNE   SPR30                                                            
         CP    CURLNE,=P'0'        FIRST FOR THIS CHECK                         
         BNE   SPR7                                                             
         ZAP   CURLNE,=P'1'                                                     
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,SRCNTR+4                                                      
         BAS   RE,MIDUP                                                         
         XC    SVSTA,SVSTA                                                      
*                                                                               
SPR7     CP    CURLNE,MAXLNE                                                    
         BL    SPR9                                                             
         BAS   RE,VDCK                                                          
         LA    R4,SRCNTR+4                                                      
         CLC   SVSTA,SRCNTR+4                                                   
         BE    *+8                                                              
         LA    R4,SVSTA                                                         
         BAS   RE,MIDUP                                                         
*                                                                               
SPR9     CLC   P+23(51),SPACES     DO WE HAVE A LINE FROM LAST TIME?            
         BE    SPR11                                                            
         BAS   RE,SPPRN            PRINT LAST ITEM                              
*                                                                               
SPR11    OC    SVSTA,SVSTA         SETUP NEW ITEM                               
         BZ    SPR15                                                            
         CLC   SVSTA,SRCNTR+4                                                   
         BE    SPR15                                                            
         BAS   RE,STATOTL          STATION TOTAL                                
         CP    CURLNE,ML3LNE       IS THERE ENOUGH ROOM                         
         BL    *+8                 IF THERE IS, PRINT CALL LETTERS              
         BAS   RE,VDCK             ELSE, SKIP TO NEW REMITTANCE                 
         LA    R4,SRCNTR+4                                                      
         BAS   RE,MIDUP            SET INTERMEDIATE STATION CHANGES             
*                                                                               
SPR15    MVC   SVSTA,SRCNTR+4                                                   
         AP    STATOT,SRTAMT                                                    
         BAS   RE,SPDTL            SETUP THE DETAIL LINE                        
         B     XIT                                                              
*                                                                               
SPR30    CLI   MODE,SBACLAST       LAST FOR THE SUBACCOUNT                      
         BNE   SPR40                                                            
         CURED SUBTOT,(11,P+74),2,MINUS=YES                                     
         BAS   RE,SPPRN            PRINT LAST TRANSACTION + CLI TOTAL           
         ZAP   SUBTOT,=P'0'                                                     
         B     XIT                                                              
*                                                                               
SPR40    CLI   MODE,ACCLAST        LAST FOR THE ACCOUNT                         
         BNE   XIT                                                              
         BAS   RE,STATOTL          TOTAL FOR LAST STATION                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT STATION TOTAL LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
STATOTL  NTR1  ,                                                                
         CP    CURLNE,ML3LNE       MUST HAVE 3 LEFT                             
         BL    *+12                                                             
         BAS   RE,VDCK             VOID THE CHECK                               
         B     *+8                                                              
         BAS   RE,PRTLNE           BLANK LINE                                   
         CURED STATOT,(11,P+74),2,MINUS=YES                                     
         MVC   P+53(L'AC@STOT),AC@STOT         STATION TOTAL                    
         ZAP   STATOT,=P'0'                                                     
         BAS   RE,PRTLNE           PRINT THE TOTAL                              
         BAS   RE,PRTLNE           AND SKIP A LINE                              
         B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* SET MIDLINES                                                        *         
***********************************************************************         
         SPACE 1                                                                
         DC    F'0'                                                             
MIDUP    ST    RE,MIDUP-4                                                       
         LA    R2,MID1             R2,TO PRINT AREA                             
         BAS   RE,SETSTA           SET STATION HEADLINES                        
         AP    CURLNE,=P'3'                                                     
         MVI   FORCEMID,C'Y'                                                    
         L     RE,MIDUP-4                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SET-UP SPOT DETAIL LINE                                             *         
***********************************************************************         
         SPACE 1                                                                
SPDTL    NTR1  ,                                                                
         CURED SRTAMT,(12,P+62),2,MINUS=YES                                     
         OC    SRXPY(SRXPYL),SRXPY EXTRA PAYMENT ELEMENT                        
         BZ    SPDTL19                                                          
         MVC   P+2(20),SRXCLI                                                   
         MVC   P+23(11),SRXPRD                                                  
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,SRXEST         ESTIMATE NUMBER                              
         BZ    SPDTL6                                                           
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+29(3),DUB+6(2)                                                 
*                                                                               
SPDTL6   MVC   P+50(10),SRXINV                                                  
         CLI   SRXPER+6,C' '                                                    
         BNE   SPDTL7              IF NO END DO MMMDD/YY                        
         GOTO1 DATCON,DMCB,(0,SRXPER),(8,P+35)                                  
         B     SPDTL17                                                          
*                                                                               
SPDTL7   CLI   SRACC+3,C'N'                                                     
         BE    SPDTL11             NETWORK IS SPECIAL                           
*                                                                               
SPDTL9   GOTO1 DATCON,DMCB,(0,SRXPER+6),(9,P+35)  MMM/YY BROADCAST              
         B     SPDTL17                                                          
*                                                                               
SPDTL11  CLC   SRXPER(6),SRXPER+6     FOR NETWORK IF START=END                  
         BNE   SPDTL13                                                          
         GOTO1 DATCON,DMCB,(0,SRXPER),(8,P+35) MMMDD/YY                         
         B     SPDTL17                                                          
*                                                                               
SPDTL13  DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,SRXPER),(0,SRXPER)                                
         GOTO1 DATCON,DMCB,(0,SRXPER+6),(0,SRXPER+6)                            
         GOTO1 GETBROAD,DMCB,(1,SRXPER),WORK,GETDAY,ADDAY                       
         GOTO1 GETBROAD,DMCB,(1,SRXPER+6),WORK+12,GETDAY,ADDAY                  
         CLC   WORK(12),WORK+12         IS IT ONE BROADCAST MONTH               
         BE    SPDTL9                   IF IT IS DO IT LIKE SPOT                
         GOTO1 DATCON,DMCB,(0,SRXPER),(8,P+35)    IF START NOT = END            
         GOTO1 DATCON,DMCB,(0,SRXPER+6),(8,WORK)  MMMDD-DD/YY                   
         MVI   P+40,C'-'                                                        
         MVC   P+41(5),WORK+3                                                   
*                                                                               
SPDTL17  CLI   SRTNRL,0            HANDLE COMMENTS ON REGULAR POSTINGS          
         BE    XIT                                                              
         SR    R3,R3                                                            
         IC    R3,SRTNRL                                                        
         BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   NARRWRK(0),SRTNRR                                                
         B     XIT                                                              
*                                                                               
SPDTL19  CLI   SRTNRL,0            USE NARRATIVE IF NO 46 ELEMENT               
         BE    XIT                                                              
         SR    R3,R3                                                            
         IC    R3,SRTNRL                                                        
         GOTO1 CHOPPER,DMCB,((R3),SRTNRR),(56,P+02),(C'P',1)                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT SPOT DETAIL LINE                                              *         
***********************************************************************         
         SPACE 1                                                                
SPPRN    NTR1  ,                                                                
         BAS   RE,PRTLNE           PRINT TRANSACTION                            
         LA    R0,5                                                             
         LA    R1,PBLK                                                          
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,*-10                                                          
         CLC   NARRWRK(80),SPACES ANY COMMENTS                                  
         BE    XIT                                                              
*                                                                               
         LA    R2,NARRWRK+L'NARRWRK-1                                           
         CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         LA    R3,NARRWRK                                                       
         SR    R2,R3                                                            
         LA    R2,1(R2)                                                         
         GOTO1 ADSQUASH,DMCB,NARRWRK,(R2)                                       
         L     R2,DMCB+4                                                        
         GOTO1 CHOPPER,DMCB,((R2),NARRWRK),(50,PBLK+23),(C'P',5)                
         MVI   NARRWRK,C' '                                                     
         MVC   NARRWRK+1(L'NARRWRK-1),NARRWRK                                   
         L     R4,DMCB+8                                                        
         CVD   R4,DOUBLE                                                        
         AP    DOUBLE,CURLNE                                                    
         CP    DOUBLE,MAXLNE       TEST ENOUGH LINES FOR NARRATIVE              
         BL    *+8                                                              
         BAS   RE,VDCK             NOT ENOUGH VOID IT                           
         BAS   RE,PRN              PRINT NARRATIVE                              
         BAS   RE,PRTLNE           SKIP A LINE                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SET STATION CALL LETTERS                                            *         
*                                                                     *         
* R4 = STATION CALL LETTERS                                           *         
* R2 = PRINT AREA (HEADS OR MIDS)                                     *         
***********************************************************************         
         SPACE 1                                                                
SETSTA   LR    R0,RE                                                            
         MVC   1(L'AC@STA,R2),AC@STA       'STATION'                            
         LA    R5,L'AC@STA(R2)                                                  
         BRAS  RE,FNDNXT           R5 TO NEXT SPACE                             
         MVI   0(R5),C':'          REPLACE WITH COLON                           
         MVC   10(5,R2),0(R4)      STATION CALL LETTERS                         
         MVI   1+L'P(R2),C'-'      UNDERLINE MID/HEAD2                          
         MVC   2+L'P(12,R2),1+L'P(R2)                                           
         CLI   SRACC+2,C'X'        LEDGER X                                     
         BE    SETSTAX                                                          
         CLI   SRACC+3,C'X'        AND ACCOUNT X                                
         BE    SETSTAX             END HERE                                     
         MVC   2+L'P(15,R2),1+L'P(R2) EXTEND THE UNDELINE                       
         MVC   14(3,R2),=C'-AM'    AND ADD THE BAND                             
         CLI   4(R4),C'A'          -AM                                          
         BE    SETSTAX                                                          
         MVI   15(R2),C'F'                                                      
         CLI   4(R4),C'F'          -FM                                          
         BE    SETSTAX                                                          
         MVC   14(3,R2),=C'-TV'    OR -TV                                       
SETSTAX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT PUBLICATION                                                   *         
***********************************************************************         
         SPACE 1                                                                
PRP      NTR1  ,                                                                
         CLI   MODE,ACCFRST        FIRST FOR THE ACCOUNT                        
         BNE   PRP7                                                             
         MVC   SUBTXT,SPACES                                                    
         MVC   ACCTXT,SPACES                                                    
         MVC   SUBTXT+2(L'AC@CLITO),AC@CLITO    'CLIENT TOTALS'                 
         MVC   ACCTXT+2(L'TXT2),TXT2              CHECK TOTALS                  
         XC    CLINO,CLINO                                                      
         ZAP   CURLNE,=P'0'                                                     
         B     XIT                                                              
*                                                                               
PRP7     CLI   MODE,PROCTRNS       PROCESS A TRANSACTION                        
         BNE   XIT                                                              
         CP    CURLNE,=P'0'                                                     
         BNE   PRP9                                                             
         MVC   CLINO,SRCNTR+12                                                  
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+1(L'SRANME),SRANME PRINT PUB NAME                              
         AP    CURLNE,=P'1'                                                     
         BAS   RE,PRTLNE                                                        
         BAS   RE,PRTLNE                                                        
         BAS   RE,NEWCLI                                                        
*                                                                               
PRP9     CP    CURLNE,MAXLNE       HAVE WE RUN OUT OF LINES                     
         BL    *+8                                                              
         BAS   RE,VDCK                                                          
         CLC   CLINO,SRCNTR+12     SAME CLIENT                                  
         BE    PRP17               YES-BRANCH                                   
         BAS   RE,NEWCLI                                                        
         MVC   CLINO,SRCNTR+12                                                  
*                                                                               
PRP17    ZAP   DOUBLE,SRTAMT                                                    
         BAS   RE,PRDTL            SETUP DETAIL LINE                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT REP                                                           *         
***********************************************************************         
         SPACE 1                                                                
PRR      NTR1  ,                                                                
         CLI   MODE,ACCFRST        FIRST FOR THE ACCOUNT                        
         BNE   PRR9                                                             
         MVC   SUBTXT,SPACES                                                    
         MVC   ACCTXT,SPACES                                                    
         MVC   SUBTXT+2(L'AC@CLITO),AC@CLITO    'CLIENT TOTALS'                 
         MVC   ACCTXT+2(L'TXT2),TXT2            CHECK TOTALS                    
         ZAP   CURLNE,=P'0'                                                     
         XC    PUBNO,PUBNO                                                      
         XC    CLINO,CLINO                                                      
         ZAP   PUBC1,=P'0'                                                      
         ZAP   PUBC2,=P'0'                                                      
         ZAP   PUBTOT,=P'0'                                                     
         B     XIT                                                              
*                                                                               
PRR9     CLI   MODE,PROCTRNS       PROCESS A TRANSACTION                        
         BNE   PRR20                                                            
         CP    CURLNE,=P'0'                                                     
         BNE   PRR11                                                            
         MVC   PUBNO,SRCNTR                                                     
         MVC   CLINO,SRCNTR+12                                                  
         BAS   RE,NEWPGE                                                        
         BAS   RE,NEWPUB                                                        
         BAS   RE,NEWCLI                                                        
*                                                                               
PRR11    CP    CURLNE,MAXLNE       HAVE WE RUN OUT OF LINES                     
         BL    *+8                                                              
         BAS   RE,VDCK                                                          
         CLC   PUBNO,SRCNTR        SAME PUB?                                    
         BE    PRR15                                                            
         BAS   RE,PUBTOTP                                                       
         BAS   RE,NEWPUB                                                        
         BAS   RE,NEWCLI                                                        
         MVC   PUBNO,SRCNTR                                                     
         MVC   CLINO,SRCNTR+12                                                  
         B     PRR17                                                            
*                                                                               
PRR15    CLC   CLINO,SRCNTR+12     SAME CLIENT                                  
         BE    PRR17                                                            
         BAS   RE,NEWCLI                                                        
         MVC   CLINO,SRCNTR+12                                                  
*                                                                               
PRR17    ZAP   DOUBLE,SRTAMT                                                    
         AP    PUBTOT,SRTAMT                                                    
         BAS   RE,PRDTL            PRINT THE DETAIL LINE                        
         B     XIT                                                              
*                                                                               
PRR20    CLI   MODE,ACCLAST        LAST FOR THE ACCOUNT                         
         BNE   XIT                                                              
         BAS   RE,PUBTOTP          TOTAL FOR LAST PUB                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT PUBLICATION TOTALS                                            *         
***********************************************************************         
         SPACE 1                                                                
PUBTOTP  NTR1  ,                                                                
         LA    RF,PRTLNE                                                        
         CP    CURLNE,ML2LNE       MUST HAVE 2 LINES                            
         BL    *+8                                                              
         LA    RF,VDCK                                                          
         BASR  RE,RF               BLANK OR NEW PAGE                            
         MVC   P+22(L'AC@PUBT),AC@PUBT   'PUBLICATION TOTALS'                   
         CURED PUBC1,(11,P+48),2,MINUS=YES                                      
         CURED PUBC2,(9,P+64),2,MINUS=YES                                       
         CURED PUBTOT,(11,P+73),2,MINUS=YES                                     
         ZAP   PUBC1,=P'0'                                                      
         ZAP   PUBC2,=P'0'                                                      
         ZAP   PUBTOT,=P'0'                                                     
         BAS   RE,PRTLNE           PRINT TOTAL                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT PUBLICATION NAME AND NUMBER                                   *         
***********************************************************************         
         SPACE 1                                                                
NEWPUB   NTR1  ,                                                                
         CP    CURLNE,=P'1'        FIRST PRINT LINE                             
         BNH   NEWP1               DON'T SKIP A LINE                            
         LA    RF,PRTLNE                                                        
         CP    CURLNE,ML3LNE       MUST HAVE 3 LINES                            
         BL    *+8                                                              
         LA    RF,VDCK                                                          
         BASR  RE,RF                                                            
*                                                                               
NEWP1    CLC   SRCNTR+1(11),SPACES                                              
         BE    NEWP4                                                            
         MVC   P+1(L'SRCNME),SRCNME  CONTRA ACCOUNT NAME                        
         SR    R1,R1                                                            
         IC    R1,SRCNML                                                        
         LA    R2,P+4(R1)          POINT 2 BEYOND NAME                          
         PACK  DUB(6),SRCNTR+1(11)                                              
         MVC   DUB+5(1),SRCNTR+11   EDITION                                     
         GOTO1 PUBEDIT,DMCB,DUB,(C'S',1(R2))                                    
         CLC   SRCNTR+9(3),=C'ZZZ'                                              
         BNE   NEWP3                                                            
         LA    RF,17(R2)                                                        
         CLI   0(RF),C','                                                       
         BE    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   1(3,RF),=C'ALL'                                                  
*                                                                               
NEWP3    MVI   0(R2),C'('                                                       
         LA    R2,17(R2)                                                        
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),C')'                                                       
*                                                                               
NEWP4    BAS   RE,PRTLNE           PUBLICATION                                  
         BAS   RE,PRTLNE           BLANK LINE                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT CLIENT NAME                                                   *         
***********************************************************************         
         SPACE 1                                                                
NEWCLI   NTR1  ,                                                                
         CP    CURLNE,ML1LNE                                                    
         BL    *+8                                                              
         BAS   RE,VDCK                                                          
         OC    SRXCLI,SRXCLI      EXTRA PAYMENT ELEMENT                         
         BZ    *+10                                                             
         MVC   P+1(20),SRXCLI     USE CLIENT NAME                               
         BAS   RE,PRTLNE                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SETUP PUBLICATION DETAIL  LINE                                      *         
***********************************************************************         
         SPACE 1                                                                
PRDTL    NTR1  ,                                                                
         LA    R0,5                CLEAR PRINT BLOCK                            
         LA    R1,PBLK                                                          
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         CURED SRTAMT,(11,P+73),2,MINUS=YES                                     
         AP    SUBC1,DOUBLE        NET PAYABLE                                  
         AP    CHC1,DOUBLE                                                      
         AP    PUBC1,DOUBLE                                                     
         OC    SRXPY(SRXPYL),SRXPY EXTRA PAYMENT ELEMENT                        
         BZ    PRDTL15                                                          
         MVC   P+36(13),SRXINV                                                  
         CURED SRXCD,(9,P+64),2,MINUS=YES                                       
         AP    DOUBLE,SRXCD       ADD C.D. TO NET FOR EDIT                      
         CURED (P8,DOUBLE),(11,P+48),2,MINUS=YES                                
         AP    SUBC1,SRXCD        ADD C.D. TO NET                               
         AP    CHC1,SRXCD                                                       
         AP    PUBC1,SRXCD                                                      
         AP    SUBC2,SRXCD        AND TO CASH DISCOUNT                          
         AP    CHC2,SRXCD                                                       
         AP    PUBC2,SRXCD                                                      
         MVC   P+1(20),SRXPRD                                                   
         GOTO1 DATCON,DMCB,(0,SRXPER),(9,P+22)                                  
         CLI   SRXPER+6,0                                                       
         BE    PRDTL3                                                           
         MVI   P+28,C'-'                                                        
         GOTO1 (RF),(R1),(0,SRXPER+6),(9,P+29)                                  
         B     PRDTL7                                                           
*                                                                               
PRDTL3   CLC   SRXPER+4(2),=C'00'                                               
         BE    PRDTL7                                                           
         MVC   P+22(13),SPACES     SHOW SINGLE DATE- IFPAID THAT WAY            
         GOTO1 (RF),(R1),,(8,P+22)                                              
*                                                                               
PRDTL7   BAS   RE,PRTLNE                                                        
         SR    R3,R3                                                            
         CLI   SRTNRL,0                                                         
         BE    XIT                 EXIT IF NO NARRATIVE                         
         SR    R3,R3                                                            
         IC    R3,SRTNRL                                                        
         GOTO1 ADSQUASH,DMCB,SRTNRR,(R3)                                        
         L     R3,DMCB+4                                                        
         CH    R3,=H'112'          GREATER THAN 4 X 28 - BIGGER CHOP            
         BH    PRDTL9                                                           
         GOTO1 CHOPPER,DMCB,((R3),SRTNRR),(28,PBLK+22),(C'P',4)                 
         B     PRDTL17                                                          
*                                                                               
PRDTL9   GOTO1 CHOPPER,DMCB,((R3),SRTNRR),(48,PBLK+2),(C'P',5)                  
         B     PRDTL17                                                          
*                                                                               
*                                                                               
PRDTL15  LA    R4,1                                                             
         ST    R4,DMCB+8                                                        
         CLI   SRTNRL,0            USE NARRATIVE IF NO 46 ELEMENT               
         BE    PRDTL16                                                          
         SR    R3,R3                                                            
         IC    R3,SRTNRL                                                        
         GOTO1 CHOPPER,DMCB,((R3),SRTNRR),(48,P+2),(C'P',1)                     
PRDTL16  BAS   RE,PRTLNE                                                        
*                                                                               
PRDTL17  L     R4,DMCB+8                                                        
         CVD   R4,DOUBLE                                                        
         AP    DOUBLE,CURLNE                                                    
         CP    DOUBLE,MAXLNE       ENOUGH LINES FOR NARRATIVE                   
         BL    *+8                                                              
         BAS   RE,VDCK             NOT ENOUGH VOID CHECK - START NEW            
         BAS   RE,PRN                                                           
         BAS   RE,PRTLNE           BLANK LINE                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRODUCTION / EXPENSE                                                *         
***********************************************************************         
         SPACE 1                                                                
EXP      NTR1  ,                                                                
         CLI   MODE,ACCFRST        FIRST FOR ACCOUNT                            
         BNE   EXP3                                                             
         ZAP   CURLNE,=P'0'                                                     
         MVC   SUBTXT,SPACES                                                    
         MVC   ACCTXT,SPACES                                                    
         MVC   SUBTXT+00(L'AC@CCTL),AC@CCTL  'CLIENT/CATEGORY TOTAL'            
         MVC   ACCTXT+00(L'TXT8),TXT8        TOTALS FOR CHECK                   
         XC    CLISAVE,CLISAVE                                                  
         B     XIT                                                              
*                                                                               
EXP3     CLI   MODE,PROCTRNS       PROCESS TRANSACTION                          
         BNE   EXP33                                                            
         CP    CURLNE,=P'0'                                                     
         BNE   EXP5                                                             
         BAS   RE,NEWPGE                                                        
         BAS   RE,CLIOUT           FIRST CLIENT FOR 2ND & SUBSQ. CHECKS         
         BAS   RE,PRTLNE                                                        
         LA    R0,5                                                             
         LA    R1,PBLK                                                          
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
EXP5     CP    CURLNE,MAXLNE       HAVE WE RUN OUT OF LINES                     
         BL    EXP7                                                             
         BAS   RE,VDCK                                                          
         BAS   RE,PRTLNE                                                        
*                                                                               
EXP7     CLC   CLISAVE,SRCNTR+3    SAME CLIENT                                  
         BE    EXP11                                                            
         CP    CURLNE,ML2LNE                                                    
         BL    *+8                                                              
         BAS   RE,VDCK                                                          
         BAS   RE,CLIOUT                                                        
         BAS   RE,PRTLNE                                                        
*                                                                               
EXP11    CLC   SRTREF,INVNO                                                     
         BE    EXP15                                                            
         CP    INVCNT,=P'1'                                                     
         BE    EXP13                                                            
         BAS   RE,INVPRT                                                        
*                                                                               
EXP13    MVC   INVNO,SRTREF                                                     
         ZAP   INVPAY,=P'0'                                                     
         ZAP   INVCNT,=P'0'                                                     
         ZAP   INVCD,=P'0'                                                      
*                                                                               
EXP15    MVC   PBLK+42(6),SRTREF                                                
         AP    INVPAY,SRTAMT                                                    
         AP    INVCNT,=P'1'                                                     
         CURED SRTAMT,(11,PBLK+74),2,MINUS=YES                                  
         ZAP   DOUBLE,SRTAMT       SAVE FOR CD CALCULATION                      
         MVI   NARRWRK,X'40'                                                    
         MVC   NARRWRK+1(L'NARRWRK-1),NARRWRK                                   
         CLI   SRTNRL,0            IS THERE A NARRATIVE                         
         BE    EXP17                                                            
         SR    R3,R3                                                            
         IC    R3,SRTNRL                                                        
         BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   NARRWRK+31(0),SRTNRR                                             
*                                                                               
EXP17    CP    SRTCD,=P'0'         CASH DISCOUNT                                
         BE    EXP25                                                            
         CURED SRTCD,(9,PBLK+64),2,MINUS=YES                                    
         AP    DOUBLE,SRTCD                                                     
         AP    SUBC2,SRTCD          CD FOR SUB-TOTAL                            
         AP    CHC2,SRTCD           AND CHECK TOTAL                             
         AP    INVCD,SRTCD                                                      
*                                                                               
EXP25    CLI   SROPRD,0             OTHERS ELEMENT                              
         BE    EXP27                                                            
         LA    R5,NARRWRK                                                       
         MVC   0(L'AC@PRO,R5),AC@PRO       C'PRODUCT='                          
         LA    R5,L'AC@PRO-1(R5)                                                
         BRAS  RE,FNDNXT                                                        
         MVI   0(R5),C'='                                                       
         MVC   1(6,R5),SROPRD       PRODUCT CODE                                
         LA    R5,8(R5)                                                         
         MVC   0(L'AC@JOB,R5),AC@JOB    C'JOB='                                 
         LA    R5,L'AC@JOB-1(R5)                                                
         BRAS  RE,FNDNXT                                                        
         MVI   0(R5),C'='                                                       
         MVC   1(6,R5),SROJOB       PRODUCT CODE                                
*                                                                               
EXP27    AP    SUBC1,DOUBLE         NET PAYABLE FOR SUB-TOTAL                   
         AP    CHC1,DOUBLE          AND CHECK TOTAL                             
         CURED DOUBLE,(11,PBLK+48),2,MINUS=YES                                  
         LA    R4,NARRWRK+L'NARRWRK-1 FIND LENGTH OF NARRATIVE                  
         CLI   0(R4),X'40'                                                      
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R5,NARRWRK-1                                                     
         SR    R4,R5                                                            
         BNP   EXP29                                                            
         GOTO1 ADSQUASH,DMCB,NARRWRK,(R4)                                       
         L     R4,DMCB+4           SQUASHED LENGTH                              
         GOTO1 CHOPPER,DMCB,((R4),NARRWRK),(39,PBLK+01),(C'P',4)                
         SR    R3,R3                                                            
         IC    R3,DMCB+11          NUMBER OF LINES                              
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
*                                                                               
EXP29    LA    R3,1                                                             
         CVD   R3,DUB                                                           
         AP    DUB,CURLNE                                                       
         CP    DUB,MAXLNE          WILL IT ALL FIT                              
         BNH   *+8                                                              
         BAS   RE,VDCK             IF NOT GO TO NEXT CHECK                      
         LA    R5,PBLK                                                          
         LA    R0,5                                                             
*                                                                               
EXP31    MVC   P,0(R5)             PRINT THE BLOCK                              
         BAS   RE,PRTLNE                                                        
         MVC   0(132,R5),SPACES                                                 
         LA    R5,132(R5)                                                       
         CLC   0(132,R5),SPACES                                                 
         BE    XIT                                                              
         BCT   R0,EXP31                                                         
         B     XIT                                                              
*                                                                               
EXP33    CLI   MODE,SBACLAST       LAST FOR THE SUBACCOUNT                      
         BNE   XIT                                                              
         ZAP   DOUBLE,INVPAY                                                    
         AP    DOUBLE,INVCD                                                     
         CP    DOUBLE,SUBC1        NET PAYABLE VS NET PAYABLE                   
         BE    EXP35                                                            
         CP    INVCNT,=P'1'        IF ONLY 1 LINE - DON'T BOTHER                
         BE    EXP35                                                            
         BAS   RE,INVPRT                                                        
*                                                                               
EXP35    ZAP   INVCNT,=P'0'                                                     
         ZAP   INVPAY,=P'0'                                                     
         ZAP   INVCD,=P'0'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET AND PRINT CLIENT NAME                                           *         
***********************************************************************         
         SPACE 1                                                                
CLIOUT   NTR1  ,                                                                
         MVC   P+1(L'SRCNME),SRCNME    CONTRA NAME                              
         CLC   ALPHAID,=C'BD'          BDNY  SPECIAL(NO NAME)                   
         BNE   CLI2                                                             
         CLI   QLEDGER,C'X'                                                     
         BNE   CLI2                                                             
         MVC   P+1(L'SRCNME),SPACES    CLEAR NAME                               
*                                                                               
CLI2     MVC   CLISAVE,SRCNTR+3                                                 
         MVC   INVNO,SRTREF                                                     
         ZAP   INVCNT,=P'0'                                                     
         ZAP   INVPAY,=P'0'                                                     
         ZAP   INVCD,=P'0'                                                      
         BAS   RE,PRTLNE                                                        
         B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* PRINT INVOICE TOTAL LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
INVPRT   NTR1                                                                   
         CP    INVPAY,=P'0'                                                     
         BNE   *+14                                                             
         CP    INVCD,=P'0'                                                      
         BE    XIT                                                              
         CP    CURLNE,ML3LNE       MUST HAVE 3 LINES                            
         BL    *+12                                                             
         BAS   RE,VDCK                                                          
         B     *+8                                                              
         BAS   RE,PRTLNE           SKIP A LINE                                  
*                                                                               
INVP2    MVC   P+20(L'AC@INVTO),AC@INVTO     '* TOTAL FOR INVOICE *'            
         MVC   P+42(6),INVNO                                                    
         CURED INVPAY,(10,P+75),2,MINUS=YES                                     
         CURED INVCD,(9,P+64),2,MINUS=YES,ZERO=BLANK                            
         AP    INVPAY,INVCD                                                     
         CURED INVPAY,(10,P+49),2,MINUS=YES                                     
         BAS   RE,PRTLNE                                                        
         BAS   RE,PRTLNE                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* COKE EXPENDITURE                                                    *         
***********************************************************************         
         SPACE 1                                                                
CXP      NTR1  ,                                                                
         CLI   MODE,ACCFRST        FIRST FOR THE ACCOUNT                        
         BNE   CXP3                                                             
         MVC   SUBTXT,SPACES                                                    
         MVC   ACCTXT,SPACES                                                    
         MVC   SUBTXT+00(L'AC@PRDT),AC@PRDT      'PRODUCT TOTAL'                
         MVC   ACCTXT+00(L'TXT8),TXT8                                           
         XC    LASTACC,LASTACC                                                  
         ZAP   ITEMS,=P'0'                                                      
         BAS   RE,NEWPGE                                                        
         B     XIT                                                              
*                                                                               
CXP3     CLI   MODE,PROCTRNS       PROCESS TRANSACTION                          
         BNE   CXP27                                                            
         MVC   THISACC(L'SRACC),SRACC                                           
         MVC   THISACC+L'SRACC(7),SRKOFC                                        
         CLC   THISACC,LASTACC                                                  
         BE    CXP9                                                             
         BAS   RE,PRDTOT                                                        
         CP    CURLNE,ML2LNE                                                    
         BL    *+8                                                              
         BAS   RE,VDCK                                                          
*                                                                               
CXP7     MVC   P+1(L'SRCNME),SRCNME IF KEY CHANGE PRINT PRODUCT                 
         BAS   RE,PRTLNE                                                        
         BAS   RE,PRTLNE                                                        
         MVC   LASTACC,THISACC                                                  
*                                                                               
CXP9     CP    CURLNE,ML1LNE      NOW PRINT THE LINE                            
         BL    CXP13                                                            
         BAS   RE,VDCK                                                          
         MVC   P+1(L'SRCNME),SRCNME PRINT PRODUCT                               
         BAS   RE,PRTLNE                                                        
         BAS   RE,PRTLNE                                                        
*                                                                               
CXP13    MVC   P+31(6),SRTREF                                                   
         GOTO1 DATCON,DMCB,(1,SRTDTE),(8,P+38)                                  
         ZAP   INVNET,SRTAMT                                                    
         AP    PRDNET,SRTAMT                                                    
         AP    PRDPAY,SRTAMT                                                    
         AP    CHC1,SRTAMT          FOR CHECK TOTAL                             
*                                                                               
         LA    R0,5                                                             
         LA    R1,PBLK                                                          
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,*-10                                                          
         MVI   NARRWRK,C' '                                                     
         MVC   NARRWRK+1(L'NARRWRK-1),NARRWRK                                   
         CLI   SRTNRL,0                                                         
         BE    CXP15                                                            
         SR    R3,R3                                                            
         IC    R3,SRTNRL                                                        
         BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   NARRWRK(0),SRTNRR                                                
*                                                                               
CXP15    OC    SRXPY(SRXPYL),SRXPY    EXTRA PAYMENT ELEMENT                     
         BZ    CXP21                                                            
         MVC   P+2(26),SRXCLI         MEDIA/VEHICLE NAME                        
         AP    PRDCD,SRXCD                                                      
         AP    INVNET,SRXCD           ADD CD TO INVOICE NET                     
         AP    PRDNET,SRXCD           ADD CD TO PRODUCT NET                     
         AP    CHC1,SRXCD             FOR CHECK TOTAL                           
         AP    CHC2,SRXCD             FOR CHECK TOTAL                           
*                                                                               
CXP21    CURED INVNET,(10,P+49),2,MINUS=YES                                     
         CURED SRXCD,(9,P+65),2,MINUS=YES,ZERO=BLANK                            
         CURED SRTAMT,(10,P+75),2,MINUS=YES                                     
         AP    ITEMS,=P'1'                                                      
*                                                                               
CXP23    BAS   RE,PRTLNE                                                        
         CLC   NARRWRK(80),SPACES ANY COMMENTS                                  
         BE    XIT                                                              
         LA    R2,NARRWRK+L'NARRWRK-1                                           
         CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         LA    R3,NARRWRK                                                       
         SR    R2,R3                                                            
         LA    R2,1(R2)                                                         
         GOTO1 ADSQUASH,DMCB,NARRWRK,(R2)                                       
         L     R2,DMCB+4                                                        
         GOTO1 CHOPPER,DMCB,((R2),NARRWRK),(43,PBLK+30),(C'P',5)                
         L     R4,DMCB+8                                                        
         CVD   R4,DOUBLE                                                        
         AP    DOUBLE,CURLNE                                                    
         CP    DOUBLE,MAXLNE                                                    
         BL    *+8                                                              
         BAS   RE,VDCK                                                          
         BAS   RE,PRN                                                           
         BAS   RE,PRTLNE                                                        
         B     XIT                                                              
*                                                                               
CXP27    CLI   MODE,ACCLAST       LAST ACCOUNT                                  
         BNE   XIT                                                              
         BAS   RE,PRDTOT           PRINT PRODUCT TOTAL                          
         CP    CURLNE,ML1LNE       1 LINE  FOR CHECK TOTAL                      
         BL    *+8                                                              
         BAS   RE,VDCK                                                          
*                                                                               
CXP29    MVC   P+20(50),ACCTXT                                                  
         CP    CHC1,=P'0'          NET PAYABLE                                  
         BE    CXP31                                                            
         CURED CHC1,(11,P+48),2,MINUS=YES                                       
*                                                                               
CXP31    CP    CHC2,=P'0'          CD                                           
         BE    CXP33                                                            
         CURED CHC2,(9,P+64),2,MINUS=YES                                        
*                                                                               
CXP33    CURED CHTOT,(11,P+74),2,MINUS=YES                                      
         BAS   RE,PRTLNE                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT PRODUCT TOTAL LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
PRDTOT   NTR1  ,                                                                
         CP    ITEMS,=P'2'                                                      
         BL    PRDTOT4             NO PRODUCT TOTALS IF ONE ITEM                
         CP    CURLNE,ML2LNE                                                    
         BL    *+8                                                              
         BAS   RE,VDCK                                                          
*                                                                               
PRDTOT2  MVC   P+20(L'AC@PROTO),AC@PROTO     '* TOTAL FOR PRODUCT *'            
         CURED PRDNET,(10,P+49),2,MINUS=YES                                     
         CURED PRDCD,(9,P+65),2,MINUS=YES,ZERO=BLANK                            
         CURED PRDPAY,(10,P+75),2,MINUS=YES                                     
         BAS   RE,PRTLNE                                                        
         BAS   RE,PRTLNE                                                        
*                                                                               
PRDTOT4  ZAP   PRDNET,=P'0'                                                     
         ZAP   PRDCD,=P'0'                                                      
         ZAP   PRDPAY,=P'0'                                                     
         ZAP   ITEMS,=P'0'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* AOR                                                                 *         
***********************************************************************         
         SPACE 1                                                                
AOR      NTR1  ,                                                                
         CLI   MODE,ACCFRST         FIRST FOR THE ACCOUNT                       
         BNE   AOR5                                                             
         MVC   SUBTXT,SPACES                                                    
         MVC   ACCTXT,SPACES                                                    
         MVC   ACCTXT+33(L'TXT1),TXT1 CHECK TOTAL                               
         BAS   RE,NEWPGE                                                        
*        AP    CURLNE,=P'1'                                                     
         B     XIT                                                              
*                                                                               
AOR5     CLI   MODE,PROCTRNS       PROCESS TRANSACTIONS                         
         BNE   AOR30                                                            
         CP    CURLNE,MAXLNE                                                    
         BL    AOR11                                                            
         BAS   RE,VDCK                                                          
*        AP    CURLNE,=P'1'                                                     
*                                                                               
AOR11    CURED SRTAMT,(11,P+74),2,MINUS=YES                                     
*                                                                               
AOR13    CLI   SROPRD,0            OTHERS ELEMENT                               
         BE    AOR19                                                            
         MVC   P+40(3),SROEST      ESTIMATE NUMBER                              
*                                                                               
AOR19    OC    SRXPY(SRXPYL),SRXPY EXTRA PAYMENT ELEMENT                        
         BZ    AOR20                                                            
         MVC   P+2(20),SRXCLI                                                   
         MVC   P+24(11),SRXPRD                                                  
         GOTO1 DATCON,DMCB,(0,SRXPER),(8,P+47)                                  
         MVC   P+64(10),SRXINV                                                  
AOR20    CLI   SRADV,X'C9'         TEST VALID YEAR/MONTH                        
         BH    AOR21                                                            
         SR    RE,RE                                                            
         IC    RE,SRADV            TEST YEAR                                    
         SLL   RE,28               REMOVE DECADE                                
         SRL   RE,28                                                            
         LTR   RE,RE                                                            
         BZ    AOR21                                                            
         CH    RE,=H'9'                                                         
         BH    AOR21                                                            
         CLI   SRADV+1,X'01'       TEST MONTH                                   
         BL    AOR21                                                            
         CLI   SRADV+1,X'09'                                                    
         BNH   *+20                                                             
         CLI   SRADV+1,X'10'                                                    
         BL    AOR21                                                            
         CLI   SRADV+1,X'12'                                                    
         BH    AOR21                                                            
         MVC   WORK(2),SRADV                                                    
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(6,P+56)                                    
*                                                                               
AOR21    CLC   P+2(10),SPACES                                                   
         BNE   AOR23                                                            
         CLI   SRTNRL,0            ANY NARRATIVE                                
         BE    AOR25                                                            
         SR    R3,R3                                                            
         IC    R3,SRTNRL                                                        
         GOTO1 CHOPPER,DMCB,((R3),SRTNRR),(56,P+02),(C'P',1)                    
         B     AOR25                                                            
*                                                                               
AOR23    CLI   SRTNRL,0            HANDLE COMMENTS ON REGULAR POSTINGS          
         BE    AOR25               EXIT IF NO COMMENTS                          
         SR    R3,R3                                                            
         IC    R3,SRTNRL                                                        
         BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   NARRWRK(0),SRTNRR                                                
*                                                                               
AOR25    BAS   RE,AORDP                                                         
         B     XIT                                                              
*                                                                               
AOR30    CLI   MODE,SBACLAST       LAST FOR THE SUBACCOUNT                      
         BNE   XIT                                                              
         CP    CURLNE,ML2LNE       TWO LINES NEEDED FOR CLIENT TOTAL            
         BL    *+8                                                              
         BAS   RE,VDCK                                                          
*                                                                               
AOR33    MVC   P+53(L'AC@CLITO),AC@CLITO      'CLIENT TOTAL'                    
         CURED SUBTOT,(11,P+74),2,MINUS=YES                                     
         BAS   RE,PRTLNE           PRINT CLIENT TOTAL                           
         BAS   RE,PRTLNE                                                        
         ZAP   SUBTOT,=P'0'                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* AOR DETAIL PRINTING                                                           
***********************************************************************         
         SPACE 1                                                                
AORDP    NTR1  ,                                                                
         BAS   RE,PRTLNE           PRINT TRANSACTION                            
         LA    R0,5                                                             
         LA    R1,PBLK                                                          
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,*-10                                                          
         CLC   NARRWRK(80),SPACES ANY COMMENTS                                  
         BE    XIT                                                              
         LA    R2,NARRWRK+L'NARRWRK-1                                           
         CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         LA    R3,NARRWRK                                                       
         SR    R2,R3                                                            
         LA    R2,1(R2)                                                         
         GOTO1 ADSQUASH,DMCB,NARRWRK,(R2)                                       
         L     R2,DMCB+4                                                        
         GOTO1 CHOPPER,DMCB,((R2),NARRWRK),(50,PBLK+24),(C'P',5)                
         MVI   NARRWRK,C' '                                                     
         MVC   NARRWRK+1(L'NARRWRK-1),NARRWRK                                   
         L     R4,DMCB+8                                                        
         CVD   R4,DOUBLE                                                        
         AP    DOUBLE,CURLNE                                                    
         CP    DOUBLE,MAXLNE                                                    
         BL    *+8                                                              
         BAS   RE,VDCK                                                          
         BAS   RE,PRN         PRINT THE COMMENTS                                
         BAS   RE,PRTLNE           BLANK LINE                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* UNWIRED REP                                                         *         
***********************************************************************         
         SPACE 1                                                                
UWR      NTR1  ,                                                                
         CLI   MODE,ACCFRST        FIRST FOR THE ACCOUNT                        
         BNE   UWR3                                                             
         MVC   SUBTXT,SPACES                                                    
         MVC   ACCTXT,SPACES                                                    
         MVC   ACCTXT+33(L'TXT1),TXT1    CHECK TOTAL                            
         BAS   RE,NEWPGE                                                        
         L     R3,AREMTAB                                                       
         USING BIND,R3                                                          
         XC    BININ,BININ         CLEAR THE BINARY TABLE                       
         B     XIT                                                              
*                                                                               
UWR3     CLI   MODE,PROCTRNS       PROCESS A TRANSACTION                        
         BNE   UWR21                                                            
         USING REMITD,R5                                                        
         MVC   RECWRK,SPACES       CLEAR WORK AREA                              
         LA    R5,RECWRK                                                        
         LA    R1,REAMT            CLEAR PACKED FIELDS                          
         LA    R0,RENUM                                                         
         ZAP   0(L'REAMT,R1),=P'0'                                              
         LA    R1,L'REAMT(R1)                                                   
         BCT   R0,*-10                                                          
         AP    REAMT,SRTAMT        AMOUNT OF TRANS INTO WORK AREA               
*                                                                               
UWR5     OC    SRXPY(SRXPYL),SRXPY EXTRA PAYMENT ELEMENT                        
         BZ    UWR19                                                            
         MVC   RECLI,SRXCLI        CLIENT                                       
         MVC   REINV,SRXINV        INVOICE NUMBER                               
         MVC   REEST,SRXEST        EST                                          
         CLI   SRXPER+6,C' '                                                    
         BNE   UWR11               IF NO END DO MMMDD/YY                        
         GOTO1 DATCON,DMCB,(0,SRXPER),(8,REMOS)                                 
         B     UWR19                                                            
*                                                                               
UWR11    CLI   SRACC+3,C'N'                                                     
         BE    UWR15               NETWORK IS SPECIAL                           
*                                                                               
UWR13    GOTO1 DATCON,DMCB,(0,SRXPER+6),(9,REMOS)  MMM/YY BROADCAST             
         B     UWR19                                                            
*                                                                               
UWR15    CLC   SRXPER(6),SRXPER+6     FOR NETWORK IF START=END                  
         BNE   UWR17                                                            
         GOTO1 DATCON,DMCB,(0,SRXPER),(8,REMOS) MMMDD/YY                        
         B     UWR19                                                            
*                                                                               
UWR17    GOTO1 GETBROAD,DMCB,(1,SRXPER),WORK,GETDAY,ADDAY                       
         GOTO1 GETBROAD,DMCB,(1,SRXPER+6),WORK+12,GETDAY,ADDAY                  
         CLC   WORK(12),WORK+12         IS IT ONE BROADCAST MONTH               
         BE    UWR13                    IF IT IS DO IT LIKE SPOT                
         GOTO1 DATCON,DMCB,(0,SRXPER),(8,REMOS)    IF START NOT = END           
         GOTO1 DATCON,DMCB,(0,SRXPER+6),(8,WORK)  MMMDD-DD/YY                   
         MVI   REMOS+5,C'-'                                                     
         MVC   REMOS+6(5),WORK+3                                                
*                                                                               
UWR19    L     R3,AREMTAB          THE REMITTANCE TABLE                         
         USING BIND,R3                                                          
         MVC   DMCB+8(16),BININ    PARMS 3,4,5,6                                
         L     R2,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(X'01',REMITD),(R2)                                 
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                 TABLE FULL                                   
         DC    H'0'                                                             
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1              NOT FOUND - ADDED TO TABLE                   
         BE    *+14                                                             
         L     R1,DMCB             ADDR OF ENTRY FOUND IN TAB                   
         AP    REAMT-REMITD(L'REAMT,R1),REAMT                                   
         B     XIT                                                              
*                                                                               
UWR21    CLI   MODE,ACCLAST        LAST FOR THE ACCOUNT                         
         BNE   XIT                                                              
         BAS   RE,NEWPGE                                                        
         L     R3,AREMTAB          THE REMITTANCE TABLE                         
         USING BIND,R3                                                          
         USING REMITD,R5                                                        
         L     R2,BININ            NUMBER IN TABLE                              
         L     R5,BINTABLE         ADDR OF TABLE                                
         MVC   CLSAVE(L'RECLI),SPACES                                           
         ZAP   CLITOT,=P'0'                                                     
*                                                                               
UWR23    CLC   CLSAVE(L'RECLI),SPACES                                           
         BE    UWR25                                                            
         CLC   CLSAVE(L'RECLI),RECLI                                            
         BE    UWR25                                                            
         CP    CURLNE,MAXLNE                                                    
         BL    *+8                                                              
         BAS   RE,VDCK                                                          
         CURED CLITOT,(11,P+74),2,MINUS=YES                                     
         BAS   RE,PRTLNE           PRINT CLIENT TOTAL                           
         ZAP   CLITOT,=P'0'                                                     
*                                                                               
UWR25    CP    CURLNE,MAXLNE                                                    
         BL    *+8                                                              
         BAS   RE,VDCK                                                          
         MVC   P+2(L'RECLI),RECLI          CLIENT                               
         EDIT  REEST,(3,P+24),ZERO=NOBLANK ESTIMATE NUMBER                      
         MVC   P+38(L'REMOS),REMOS         ADV PERIOD                           
         MVC   P+48(L'REINV),REINV         INVOICE NUMBER                       
         MVC   CLSAVE(L'RECLI),RECLI                                            
         AP    CLITOT,REAMT                                                     
         CURED REAMT,(11,P+59),2,MINUS=YES                                      
         BAS   RE,PRTLNE           PRINT TRANSACTION                            
         LA    R5,RELEN(R5)        NEXT ENTRY                                   
         BCT   R2,UWR23                                                         
*                                                                               
         CP    CURLNE,MAXLNE                                                    
         BL    *+8                                                              
         BAS   RE,VDCK                                                          
         CURED CLITOT,(11,P+74),2,MINUS=YES                                     
         BAS   RE,PRTLNE           LAST CLT TOTAL                               
         L     R3,AREMTAB          THE REMITTANCE TABLE                         
         USING BIND,R3                                                          
         XC    BININ,BININ         CLEAR THE BINARY TABLE                       
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* NEW PRODUCTION - EXPENSE                                            *         
***********************************************************************         
         SPACE 1                                                                
PXN      NTR1  ,                                                                
         CLI   MODE,ACCFRST                                                     
         BNE   PXN3                                                             
         MVC   SUBTXT,SPACES                                                    
         MVC   ACCTXT,SPACES                                                    
         MVC   ACCTXT+33(L'TXT1),TXT1    CHECK TOTAL                            
         BAS   RE,NEWPGE                                                        
         L     R3,APDTAB                                                        
         USING BIND,R3                                                          
         XC    BININ,BININ         CLEAR THE BINARY TABLE                       
         B     XIT                                                              
*                                  PROCESS A TRANSACTION                        
PXN3     CLI   MODE,PROCTRNS                                                    
         BNE   PXN21                                                            
         LA    R5,RECWRK           BUILD DETAIL ENTRY                           
         USING PDD,R5                                                           
         MVC   RECWRK,SPACES       CLEAR WORK AREA                              
         LA    R1,PDBK             CLEAR PACKED FIELDS                          
         LA    R0,PDCNT                                                         
         ZAP   0(L'PDBK,R1),=P'0'                                               
         LA    R1,L'PDBK(R1)                                                    
         BCT   R0,*-10                                                          
         MVC   PDDTE,SRTDTE        DATE                                         
         MVC   PDINV(L'SRTREF),SRTREF INVOICE NUMBER                            
         ZAP   PDGRS,SRTAMT        AMOUNT                                       
         ZAP   PDCD,SRTCD          CASH DISCOUNT                                
         AP    PDGRS,PDCD          PLUS CD                                      
         ZAP   PDNET,SRTAMT        NET                                          
         MVI   NARRWRK,C' '                                                     
         MVC   NARRWRK+1(L'NARRWRK-1),NARRWRK                                   
*                                                                               
         CLI   SRTNRL,0            NARRATIVE                                    
         BE    PXN9                EXIT IF NO COMMENTS                          
         SR    R3,R3                                                            
         IC    R3,SRTNRL                                                        
         BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   NARRWRK(0),SRTNRR                                                
         MVC   PDDSP,NARRWRK                                                    
*                                                                               
PXN9     L     R3,APDTAB           ADD TO DETAIL TABLE                          
         USING BIND,R3                                                          
         MVC   DMCB+8(16),BININ    PARMS 3,4,5,6                                
         L     R2,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(X'01',PDD),(R2)                                    
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                 TABLE FULL                                   
         DC    H'0'                                                             
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1              NOT FOUND - ADDED TO TABLE                   
         BE    XIT                                                              
         L     R1,DMCB             ADDR OF ENTRY FOUND IN TAB                   
         LA    R1,PDBK-PDD(R1)     ADD CURRENT ITEM TO TABLE ENTRY              
         LA    R2,PDBK                                                          
         LA    R0,PDCNT                                                         
         AP    0(L'PDBK,R1),0(L'PDBK,R2)                                        
         LA    R1,L'PDBK(R1)                                                    
         LA    R2,L'PDBK(R2)                                                    
         BCT   R0,*-14                                                          
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
PXN21    CLI   MODE,ACCLAST        LAST FOR THE ACCOUNT                         
         BNE   XIT                                                              
         BAS   RE,NEWPGE                                                        
         L     R3,APDTAB           THE REMITTANCE TABLE                         
         USING BIND,R3                                                          
         L     R2,BININ            NUMBER IN TABLE                              
         L     R5,BINTABLE         ADDRESS OF TABLE                             
         USING PDD,R5                                                           
*                                                                               
PXN23    CP    CURLNE,MAXLNE                                                    
         BL    *+8                                                              
         BAS   RE,VDCK                                                          
         LA    R4,P                                                             
         USING PPD,R4                                                           
         GOTO1 DATCON,DMCB,(1,PDDTE),(8,PPDTE)                                  
         MVC   PPINV,PDINV         INVOICE                                      
         MVC   PPDSP,PDDSP         DESCRIPTION                                  
         LA    R3,PPGRS            GROSS                                        
         CURED PDGRS,(11,(R3)),2,MINUS=YES                                      
         CP    PDCD,=P'0'                                                       
         BE    PXN25                                                            
         LA    R3,PPCD             CASH DISCOUNT                                
         CURED PDCD,(9,(R3)),2,MINUS=YES                                        
*                                                                               
PXN25    LA    R3,PPNET            NET                                          
         CURED PDNET,(11,(R3)),2,MINUS=YES                                      
         BAS   RE,PRTLNE                                                        
         LA    R5,PDLNQ(R5)        NEXT ENTRY                                   
         BCT   R2,PXN23                                                         
*                                                                               
         CP    CURLNE,MAXLNE                                                    
         BL    *+8                                                              
         BAS   RE,VDCK                                                          
         BAS   RE,PRTLNE           SKIP LINE BEFORE TOTAL                       
         CP    CURLNE,MAXLNE                                                    
         BL    *+8                                                              
         BAS   RE,VDCK                                                          
         MVC   P+20(50),ACCTXT                                                  
         LA    R3,PPNET            CHECK TOTAL                                  
         CURED CHTOT,(11,(R3)),2,MINUS=YES                                      
         BAS   RE,PRTLNE                                                        
         B     XIT                                                              
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* HEAD A NEW REMITTANCE                                               *         
***********************************************************************         
         SPACE 1                                                                
NEWPGE   MVI   FORCEHED,C'Y'                                                    
         ZAP   CURLNE,=P'1'                                                     
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
* PRINT A REMITTANCE LINE                                             *         
***********************************************************************         
         SPACE 1                                                                
         DC    F'0'                                                             
PRTLNE   ST    RE,PRTLNE-4                                                      
         TM    FTPSW,FTPTRN        FILE TRANSFER                                
         BO    PRTLNX                                                           
         AP    CURLNE,=P'1'                                                     
         CLI   SPACING,2                                                        
         BNE   *+10                                                             
         AP    CURLNE,=P'1'                                                     
         GOTO1 ACREPORT                                                         
PRTLNX   L     RE,PRTLNE-4                                                      
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
* PRINT COMMENTS                                                      *         
***********************************************************************         
         SPACE 1                                                                
PRN      NTR1  ,                                                                
         LTR   R4,R4                                                            
         BNP   XIT                                                              
         LA    R2,PBLK                                                          
PRN3     MVC   P,0(R2)                                                          
         BAS   RE,PRTLNE                                                        
         LA    R2,132(R2)                                                       
         BCT   R4,PRN3                                                          
         B     XIT                                                              
***********************************************************************         
* OTHER ROUTINES                                                      *         
***********************************************************************         
         SPACE 2                                                                
FNDNXT   CLI   0(R5),C' '          FIND LAST CHARACTER                          
         JH    FNDNXT1                                                          
         BCTR  R5,0                                                             
         J     FNDNXT                                                           
FNDNXT1  LA    R5,1(R5)            R5 TO NEXT SPACE                             
         BR    RE                                                               
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* OPEN THE WORKER FILE                                                *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
OPNWK    NTR1  ,                                                                
         XC    WKID,WKID           BUILD WORKER KEY - CHECK FILE                
         LA    RF,WKID                                                          
         USING UKRECD,RF                                                        
         MVC   UKUSRID,ORIGINUM    USER ID                                      
         MVC   UKSYSPRG,=C'A55'    SYSTEM/PROGAM                                
         MVC   UKSUBPRG,QLEDGER    LEDGER TO SUB PROGRAM                        
         PACK  DUB(2),RCDATE+3(3)                                               
         MVC   UKDAY,DUB           DAY NUMBER (PWOS)                            
         MVC   UKCLASS,TYPCHK      CLASS C=DDS, S=SOON, L=LOCAL                 
         CLI   UKCLASS,TYPDDS      FOR SOON AND LOACAL                          
         BE    *+8                                                              
         MVI   UKFLAG,X'01'        ALLOW DUPLICATES                             
         XC    WKREC(96),WKREC                                                  
         LA    RE,WKREC+28                                                      
         USING WKRECD,RE                                                        
         CLI   UKCLASS,TYPDDS      FOR DDS NO RETENTION                         
         BE    OPNWK3                                                           
         OI    UKFLAG,X'10'        SET RETENTION                                
         MVC   WKRETN,=H'30'       30 DAYS                                      
         DROP  RF                                                               
*                                                                               
OPNWK3   LA    RF,WKOPEN                                                        
         GOTO1 WORKER,DMCB,(RF),AWKBUFF,WKID,WKREC                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RE                                                               
*                                                                               
         TM    RNSW,MICR+SOON                                                   
         BNO   XIT                                                              
         MVC   WKIDP,WKID          BUILD WORKER KEY - POSTING                   
         LA    RF,WKIDP                                                         
         USING UKRECD,RF                                                        
         XC    UKINDEX+L'UKKEY(L'UKINDEX-L'UKKEY),UKINDEX+L'UKKEY               
         MVI   UKCLASS,C'P'        POSTING FILE                                 
         MVI   UKFLAG,X'01'        ALLOW DUPLICATES FOR SOON                    
*                                                                               
         LA    RF,WKOPEN                                                        
         GOTO1 WORKER,DMCB,(RF),AWKBUFFP,WKIDP,WKREC                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA CONSTANTS                                                      *         
***********************************************************************         
         SPACE 1                                                                
SVREMPQK DS    CL7                                                              
SVUNIT   DS    CL1                                                              
SVLDGR   DS    CL1                                                              
SVWKID   DS    XL(L'WKIDP)                                                      
SVAWKB   DS    A                                                                
VSSB     DS    A                   V(SSB)                                       
*                                                                               
BOCLARO  DC    C'BO'                                                            
MCCANN   DC    C'MC'                                                            
CCUSA    DC    C'CC'                                                            
MINDSHRE DC    C'H7'                                                            
AMMIRATI DC    C'PU'                                                            
*                                                                               
WKOPEN   DC    CL8'OPEN'                                                        
ACCFIL   DC    C'ACCOUNT '                                                      
ACCDIR   DC    C'ACCDIR  '                                                      
DMDTFA   DC    C'DTFAD   '                                                      
CTFILE   DC    C'CTFILE  '                                                      
*                                                                               
FACWRK   DC    C'FACWRK '                                                       
FACADD   DC    C'ADD '                                                          
*                                                                               
XSPRM    DS    0F                  XSORT PARAMETERS                             
XSPRM1   DC    A(0)                                                             
XSPRM2   DC    F'0'                                                             
XSPRM3   DC    AL4(PTNLQ)                                                       
XSPRM4   DC    AL4(PTNKLQ)                                                      
XSPRM5   DC    F'0'                                                             
*                                                                               
         EJECT                                                                  
DICI     DCDDL  AC#AGY,8,L         AGENCY                                       
         DCDDL  AC#LGR,8,L         LEDGER                                       
         DCDDL  AC#REQ,6,L         REQUEST                                      
         DCDDL  AC#PAYEE,8,L       PAYEE                                        
         DCDDL  AC#VOID,8,L        VOID                                         
         DCDDL  AC#STA,8,L         STATION                                      
         DCDDL  AC#PRO,8,L         PRODUCT                                      
         DCDDL  AC#JOB,9,L         JOB                                          
*                                                                               
         DCDDL  AC#STOT,13,L       STATION TOTAL                                
         DCDDL  AC#CLITO,13,L      CLIENT TOTAL                                 
         DCDDL  AC#PUBT,18,L       PUBLICATION TOTAL                            
         DCDDL  AC#CCTL,21,L       CLIENT/CATEGORY TOTAL                        
         DCDDL  AC#PRDT,14,L       PRODUCT TOTAL                                
         DCDDL  AC#INVTO,18,L      TOTAL FOR INVOICE                            
         DCDDL  AC#PROTO,18,L      TOTAL FOR PRODUCT                            
         DCDDL  AC#CHKS,8,L        CHECKS                                       
         DCDDL  AC#ZRUN,30,L       ZERO RUN - NO CHECKS TO WRITE                
         DCDDL  AC#ASYS,15,L       ACCOUNT SYSTEM                               
         DCDDL  AC#TCSH,15,L       TOTAL CASH                                   
*                                                                               
         DCDDL  AC#CHKT,15,L       CHECK TOTAL                                  
         DCDDL  AC#CHKTS,18,L      CHECK TOTALS                                 
         DCDDL  AC#GDCKS,15,L      GOOD CHECKS                                  
         DCDDL  AC#VDCKS,15,L      VOID CHECKS                                  
         DCDDL  AC#TCHKS,15,L      TOTAL CHECKS                                 
         DCDDL  AC#NOADC,19,L      NO ADDRESS CHECKS                            
         DCDDL  AC#NOCHK,19,L      NO CHECKS FOR                                
         DCDDL  AC#TOFCK,19,L      TOTALS FOR CHECK                             
*                                                                               
         DCDDL  AC#CHQT,15,L       CHEQUE TOTAL                                 
         DCDDL  AC#CHQTS,18,L      CHEQUE TOTALS                                
         DCDDL  AC#GDCQS,15,L      GOOD CHEQUES                                 
         DCDDL  AC#VDCQS,15,L      VOID CHEQUES                                 
         DCDDL  AC#TCHQS,15,L      TOTAL CHEQUES                                
         DCDDL  AC#NOADQ,19,L      NO ADDRESS CHEQUES                           
         DCDDL  AC#NOCHQ,19,L      NO CHEQUES FOR                               
         DCDDL  AC#TOFCQ,19,L      TOTALS FOR CHEQUE                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD A POSTING RECORD                                              *         
***********************************************************************         
                                                                                
POST     CLI   RCPOSTNG,C'N'       TEST POSTING NEEDED                          
         BER   RE                                                               
*                                                                               
POST1    DS    0H                                                               
         NMOD1 0,*POST*                                                         
         L     RC,=A(LWS)                                                       
         LA    R5,AREA             R5 = POSTING HEADER                          
         USING PSHEADD,R5          DEBIT FIRST                                  
         MVI   PSHDEL,PSHDELQ      ELEMENT CODE                                 
         MVI   PSHDLEN,PSHEADL     AND LENGTH                                   
         LA    R2,AREA+PSHEADL     R2 = TRANSACTION(DEBIT) POSTING              
         USING TRNELD,R2                                                        
         TM    RNSW,FSTWK          IS IT FIRST TIME - FOR WORKER                
         BNO   POST25                                                           
         CLI   VOIDSW,C'Y'         DID WE COME FROM 'VOID'                      
         BE    POST13                                                           
         CLI   MODE,RUNLAST        IS IT END OF RUN                             
         BE    POST30                                                           
         CLI   MODE,ACCLAST        ACCOUNT LAST - CREDIT THE BANK               
         BE    POST13                                                           
         CLI   MODE,PROCTRNS       ARE WE WRITING DEBITS TO VENDOR              
         BE    POST3                                                            
         DC    H'0'                DON'T UNDERSTAND MODE                        
*                                                                               
POST3    MVC   PSHDACC,SRACC       ACCOUNT CODE NAME                            
         MVC   PSHDANAL,SPACES                                                  
         MVC   PSHDSBAC,SRCNTR     CONTRA ACCOUNT                               
         MVC   PSHDSBNM,SRCNME     CONTRA NAME                                  
*                                                                               
POST5    MVC   TRNEL(2),=X'4445'   BUILD DEBIT POSTING                          
         MVC   TRNDATE,SRTDTE                                                   
         MVC   TRNREF,SRTREF                                                    
         MVI   TRNTYPE,X'81'       TRANSACTION TYPE - CHECK                     
         MVI   TRNSTAT,X'86'       STATUS - DEBIT/HELD/APPROVED ?               
         TM    SRSTAT,SRSURG       URGENT OR URGENT AND CREDITS                 
         BNO   *+8                                                              
         OI    TRNSTAT,X'40'                                                    
         SR    R1,R1               BUMP THE SUB REF                             
         IC    R1,SRTSBR                                                        
         AH    R1,=H'1'            GET THE NEXT SUBREFERNCE                     
         STC   R1,TRNSUB                                                        
         ZAP   TRNAMNT,SRTAMT                                                   
         MVC   TRNBTCH,SPACES                                                   
         MVC   TRNBTCH(2),SRMNTH                                                
         MVC   TRNANAL,SRTANL                                                   
         MVC   TRNNARR(41),SPACES  FILLED IN BY CHECK REGISTER WITH             
         MVI   TRNNARR+41,0        CHECK NO,DATE,AMOUNT AND BANK A/C            
*                                                                               
         LA    RE,TRNELD                                                        
         SR    R0,R0                                                            
         IC    R0,TRNLN                                                         
         AR    RE,R0               BUMP TO NEXT AREA BEYOND 44 EL               
         USING MPYELD,RE           BUILD MANUAL CHECK ELEMENT                   
         XC    MPYELD(MPYLN2Q),MPYELD                                           
         MVI   MPYEL,MPYELQ                                                     
         MVI   MPYLN,MPYLN2Q                                                    
         MVC   MPYNO,SPACES                                                     
         ZAP   MPYAMNT,=P'0'                                                    
         MVC   MPYBNK,SPACES                                                    
         MVC   MPYSUB,SRTSBR                                                    
         MVI   MPYLN2Q(RE),0       SET END OF RECORD                            
         SR    R0,R0                                                            
         IC    R0,MPYLN                                                         
         AR    RE,R0                                                            
         DROP  RE                                                               
*                                                                               
         USING FFTELD,RE           ADD REFERENCE NUMBER ELEMENT                 
         XC    FFTELD(FFTDATA-FFTELD+L'TRNREF+1),FFTELD                         
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTDATA-FFTELD+L'TRNREF                                    
         MVI   FFTTYPE,FFTTKREF                                                 
         MVI   FFTDLEN,L'TRNREF                                                 
         MVC   FFTDATA(L'TRNREF),TRNREF                                         
         BAS   RE,PWK              ADD RECORD TO WORKER FILE                    
         AP    WRKAMT,TRNAMNT                                                   
         B     POSTX                                                            
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD THE BANK POSTING - HANDLE THE VOIDS                           *         
***********************************************************************         
         SPACE 1                                                                
POST13   MVC   PSHDACC,BANKEY                                                   
         MVC   PSHDSBAC,SRACC      PAYEE IS CONTRA FOR BANK POSTING             
         MVC   PSHDSBNM,SRANME     PAYEE NAME                                   
         MVC   PSHDANAL,SPACES                                                  
         MVI   TRNEL,X'44'                                                      
         MVC   TRNDATE,SRCDTE      CHECK DATE                                   
         CLI   SRCDTE,C' '                                                      
         BH    *+10                                                             
         MVC   TRNDATE,CHKDATE                                                  
         MVC   TRNREF,SPACES       CHECK NUMBER FROM THE REGISTER               
         MVI   TRNTYPE,3           TYPE 3 ?                                     
         MVI   TRNSTAT,0           CREDIT                                       
         MVI   TRNSUB,0                                                         
         MVC   TRNANAL,SPACES                                                   
         CLC   POOFFC,SPACES                                                    
         BNH   *+10                                                             
         MVC   TRNANAL,POOFFC      POSTING OFFICE (IF PRESENT)                  
         MVC   TRNBTCH+2(4),SPACES                                              
         MVC   TRNBTCH(2),SRMNTH   MONTH OF ACTIVITY                            
         CLI   SRMNTH,C' '                                                      
         BH    *+10                                                             
         MVC   TRNBTCH(2),MNTH                                                  
         CLI   VOIDSW,C'Y'         IS THIS A VOID REMITTANCE                    
         BE    POST17                                                           
         ZAP   TRNAMNT,CHTOT       PUT IN THE CHECK TOTAL                       
         MVI   TRNLN,X'24'         SET ELEMENT LENGTH                           
         MVI   TRNNARR+8,0                                                      
         MVC   TRNANAL,SRROFC      REQUESTING OFFICE(IF ANY)                    
         CLC   POOFFC,SPACES                                                    
         BNH   *+10                                                             
         MVC   TRNANAL,POOFFC      POSTING OFFICE (IF PRESENT)                  
         EDIT  SRREQ,(6,TRNNARR)   REQUEST NUMBER                               
         UNPK  TRNREF,CHNUM                                                     
         OI    TRNREF+5,X'F0'                                                   
         LR    R4,R2                                                            
         SR    R1,R1               ADD C.D. ELEMENT                             
         IC    R1,TRNLN                                                         
         AR    R4,R1                                                            
         CP    CHCSD,=P'0'         ANY CASH DISCOUNT                            
         BE    POST15              ALL DONE                                     
*                                                                               
         USING SCIELD,R4                                                        
         MVI   SCIEL,SCIELQ        ADD C.D. ELEMENT                             
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,C'D'                                                     
         ZAP   SCIAMNT,CHCSD                                                    
         MVI   SCIAMNT+6,0                                                      
         SR    R1,R1                                                            
         IC    R1,SCILN                                                         
         AR    R4,R1                                                            
         DROP  R4                                                               
*                                                                               
         USING CEDELD,R4                                                        
POST15   MVI   CEDEL,CEDELQ        NUMBER OF INVOICES                           
         MVI   CEDLN,CEDLN1Q                                                    
         MVC   CED#INV,SR#INV                                                   
         SR    R1,R1                                                            
         IC    R1,CEDLN                                                         
         AR    R4,R1                                                            
         MVI   0(R4),0                                                          
         B     POST23                                                           
         DROP  R4                                                               
*                                                                               
POST17   MVC   TRNNARR(15),VNARR  ***VOID***                                    
         MVC   PSHDSBAC,VNARR                                                   
         MVC   PSHDSBNM,SPACES                                                  
         ZAP   TRNAMNT,=P'0'                                                    
         MVI   TRNNARR+15,0                                                     
         MVI   TRNLN,X'2B'                                                      
*                                                                               
POST23   BAS   RE,PWK                                                           
         B     POSTX                                                            
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* OPEN THE WORKER FILE                                                *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
POST25   OI    RNSW,FSTWK                                                       
         CLI   TYPCHK,TYPDDS       DDS CHECKS                                   
         BE    POSTX                                                            
         LA    R2,AREA             ADD CHECK REGISTER DETAIL ELEMENTS           
         USING CRDELD,R2                                                        
         XC    0(CRDLNQ,R2),0(R2)                                               
         MVI   CRDEL,CRDELQ        SET ELEMENT CODE                             
         MVI   CRDLN,CRDLNQ        AND LENGTH                                   
         MVI   CRDTYPE,CRDFRST     DATA TYPE (FIRST NUMBER)                     
         MVC   CRDDATA,SPACES      DATA IS INITIALIZED TO SPACES                
         BAS   RE,PWK              ADD TO WORKER FILE                           
         MVI   CRDTYPE,CRDVOID     ADD RECORD FOR VOIDS                         
         BAS   RE,PWK                                                           
         MVI   CRDTYPE,CRDSKIP     ADD RECORD FOR SKIPS                         
         BAS   RE,PWK                                                           
         B     POSTX                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* WRITE TRAILER AND CLOSE WORKER FILE                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING PSSUBFD,R5                                                       
POST30   MVI   PSSBEL,PSSBELQ      ADD TRAILER RECORD                           
         MVI   PSSBLEN,PSSUBFL                                                  
         MVC   PSSBDESC(07),=C'CHECKS-'                                         
         MVC   PSSBDESC+07(7),IDABBR                                            
         MVC   PSSBRECS(12),WRKREC                                              
         MVI   PSSBCASH+6,0                                                     
         BAS   RE,PWK                                                           
*                                                                               
         LA    RF,WKCLOSE                                                       
         GOTO1 WORKER,DMCB,(RF),AWKBUFF,WKID,WKREC                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    RNSW,SOON           TEST SOON CHECKS                             
         BNO   POSTX                                                            
         TM    RNSW,MICR           MICR CHECKS - NEED CHECK FILE                
         BO    POSTX                                                            
         LA    RF,WKKEEP           KEEP CHECK FILE                              
         GOTO1 WORKER,DMCB,(RF),AWKBUFF,WKID,WKREC                              
         CLI   DMCB+8,0                                                         
         BE    POSTX                                                            
*                                                                               
POSTX    MVI   VOIDSW,C'N'                                                      
         B     PWKX                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* WRITING OUT THE WORKER RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
PWK      NTR1  ,                                                                
         LA    R2,AREA                                                          
         SR    R3,R3                                                            
         AP    WRKREC,=P'1'        COUNT THE RECORDS                            
*                                                                               
PWK3     IC    R3,1(R2)            FIND THE END OF THE RECORD                   
         AR    R2,R3                                                            
         CLI   0(R2),0                                                          
         BNE   PWK3                                                             
         LA    R1,WKREC            R1 = POSTING AREA                            
         XC    0(4,R1),0(R1)       CLEAR THE LENGTH                             
         LA    R2,1(R2)                                                         
         SR    R2,R1                                                            
         STH   R2,0(R1)            SET RECORD LENGTH                            
         LA    RF,WKADD                                                         
         GOTO1 WORKER,DMCB,(RF),AWKBUFF,WKID,WKREC                              
         TM    DMCB+8,X'C0'                                                     
         BZ    PWKX                                                             
*                                  ERROR ON ADD TO WORKER FILE                  
         MVC   P,SPACES                                                         
         MVC   P(42),=C'*WORKER FILE FULL - STOP ALL FILE MARKERS*'             
         TM    DMCB+8,X'80'                                                     
         BO    *+10                                                             
         MVC   P(42),=C'*WORKER FILE DISK ERROR - CANNOT ADD ID = '             
         L     R2,DMCB+8           ADDRESS OF KEY                               
         MVC   DOUBLE(2),0(R2)                                                  
         LH    R3,DOUBLE                                                        
         CVD   R3,DOUBLE                                                        
         UNPK  DOUBLE+2(6),DOUBLE                                               
         OI    DOUBLE+7,X'F0'                                                   
         MVC   P+42(3),DOUBLE+5                                                 
         MVC   P+45(23),=C', REPLY = ''OK'' FOR DUMP'                           
*                                                                               
PWK7     GOTO1 LOGIO,WORK,1,(68,P)                                              
         GOTO1 (RF),(R1),0,(2,DUB)                                              
         CLC   DUB(2),=C'OK'                                                    
         BNE   PWK7                                                             
         DC    H'0'                NOW DIE                                      
*                                                                               
PWKX     XIT1                                                                   
*                                                                               
WKFILE   DC    CL8'WKFILE'                                                      
WKADD    DC    CL8'ADD'                                                         
WKCLOSE  DC    CL8'CLOSE'                                                       
WKKEEP   DC    CL8'KEEP'                                                        
*                                                                               
VNARR    DC    CL15'   ***VOID***'                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        FOR THOSE SET UP TO USE AUTOMATIC MICR PRINTING              *         
*        AUTOMATICALLY RUN REGISTER AND EXIT PROGRAM                  *         
***********************************************************************         
                                                                                
RUNREG   DS    0D                                                               
         NMOD1 0,**RREG**                                                       
         L     RC,=A(LWS)                                                       
         MVC   QUNIT,SVUNIT             SET UP REQUEST RECORD FOR AC56          
         MVC   QLEDGER,SVLDGR                                                   
         MVC   QPROG,=C'56'                                                     
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,QSTART)                                
         MVI   QOPT1,C'L'                                                       
         MVC   QOPT2,TYPCHK                                                     
*                                                                               
         XC    PROGPROF,PROGPROF         GET PROFILE FOR AC56                   
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         XC    ACMPFKEY,ACMPFKEY                                                
         MVI   ACMPFSYS,C'A'                                                    
         MVI   ACMPFPGM,C'0'                                                    
         MVC   ACMPFPGM+1(2),=C'56'                                             
         MVC   ACMPFAGY,ALPHAID                                                 
         MVC   ACMPFUNL(1),SVUNIT                                               
         MVC   ACMPFUNL+1(1),SVLDGR                                             
         GOTO1 GETPROF,DMCB,ACMPFKEY,PROGPROF,(0,DATAMGR)                       
         DROP  RF                                                               
*                                                                               
         L     RF,VEXTRAS                SET UP REMOTE KEY PARAMETERS           
         USING RUNXTRAD,RF               TO REOPEN PRINT QUE FOR AC56           
         L     R5,ADMASTD                                                       
         USING MASTD,R5                                                         
         L     R4,REMOTEC                                                       
         USING REMOTED,R4                                                       
         MVC   REMOTPRG,=C'56'                                                  
         MVC   REMOTFRM,=C'1S  '                                                
         MVI   REMOTCLS,C'Q'                                                    
         MVC   SVREMPQK,MCREMPQK                                                
         MVC   SVWKID,WKIDP       POSTING FILE ID                               
         MVC   SVAWKB,AWKBUFFP    A(POST BUFFER)                                
*                                                                               
         MVC   DUB,=CL8'AC5601'          GET ADDRESS OF SPECS FOR AC56          
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    ACSPECS,4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   ACSPECS,4(R1)             STORE ADDRESS OF SPECS                 
*                                                                               
         MVC   DUB,=CL8'AC5602'          GET ADDRES OF AC56 OVERLAY             
         MVI   DUB+6,C' '                HANDY FOR A PATCH                      
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)                  STORE ADDRESS OF OVERLAY               
*                                                                               
         MVI   MODE,RUNFRST              SET MODE TO RUNFIRST                   
         L     R0,SVAWKB                                                        
         GOTO1 (RF),DMCB,(RA),C'AC55',SVWKID,(R0)                               
         MVI   MODE,LEDGFRST             SET MODE TO LEDGFRST                   
         XC    MCREMPQK,MCREMPQK         AND GO TO REGISTER                     
         GOTO1 (RF),DMCB,(RA)                                                   
         MVI   MODE,RUNLAST              RESET MODE AND EXIT                    
         MVC   MCREMPQK,SVREMPQK                                                
RREGXIT  XIT1                                                                   
         DROP  R4,R5,RF                                                         
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PRINT THE LINE BETWEEN THE REMITTANCE ADVICE AND CHECK              *         
***********************************************************************         
         SPACE 1                                                                
DLN      DS    0D                                                               
         NMOD1 0,**DLN**                                                        
         L     RC,=A(LWS)                                                       
         ZAP   DUB,DLNLNE          NO.OF LINES IN REMITTANCE ADVICE             
         SP    DUB,CURLNE          MAX - NO PRINTED                             
         UNPK  SKIP+1(3),DUB+6(2)  SKIP DOWN TO CORRECT PLACE                   
         OI    SKIP+3,X'F0'        IN BODY OF REPORT                            
         MVC   SKIP+0(2),=C'BL'                                                 
         GOTO1 PRINT,DMCB,P,SKIP                                                
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P+01(7),IDABBR                                                   
         EDIT  CHNUM,(6,P+21),FILL=0                                            
         CLI   NUMTYP,0                                                         
         BE    *+10                                                             
         MVC   P+21(1),NUMTYP                                                   
         CLI   MODE,RUNLAST                                                     
         BNE   DLN03                                                            
         MVC   P+28(7),=C'AGENCY='                                              
         MVC   P+35(2),ALPHAID                                                  
         MVC   P+38(7),=C'LEDGER='                                              
         L     R2,ADLEDGER                                                      
         MVC   P+45(2),1(R2)                                                    
         B     DLN13                                                            
*                                                                               
DLN03    MVC   P+28(4),=C'REQ='                                                 
         EDIT  SRREQ,(3,P+32),ALIGN=LEFT                                        
         CLC   SRROFC,SPACES                                                    
         BE    DLN05                                                            
         LR    RF,R0                                                            
         LA    R1,P+32(RF)                                                      
         MVI   0(R1),C','                                                       
         MVC   1(2,R1),SRROFC                                                   
*                                                                               
DLN05    MVC   P+9(L'SYNME),SYNME                                               
*                                                                               
DLN11    CP    RUNTOT,=P'0'        LINEUP CHEQUE                                
         BE    DLN13                                                            
         LA    R3,SRACC                                                         
         MVC   P+69(12),SRACC+3                                                 
         MVC   P+63(6),=C'PAYEE='                                               
         CLI   SRACC+2,C'Q'        P OR Q MEANS USE PUBED                       
         BH    DLN13                                                            
         CLC   SRACC+8(3),SPACES   REP                                          
         BE    DLN13                                                            
         MVC   P+70(12),SPACES                                                  
         MVC   WORK(15),SRACC                                                   
         CLI   WORK+12,C'*'        THE OFFICE NONESENSE                         
         BNE   *+10                                                             
         MVC   WORK+12(2),SPACES                                                
         PACK  DUB(6),WORK+4(11)                                                
         MVC   DUB+5(1),WORK+14    EDITION                                      
         GOTO1 PUBEDIT,DMCB,DUB,(C'S',P+70)                                     
         CLI   SRACC+12,C'*'                                                    
         BNE   DLN13                                                            
         LA    R2,P+71                                                          
         CLI   0(R2),C' '                                                       
         BE    *+12                                                             
         LA    R2,1(R2)                                                         
         B     *-12                                                             
         MVC   0(3,R2),SRACC+12    THE OFFICE CODE                              
*                                                                               
DLN13    DS    0H                                                               
*                                                                               
*&&DO                                                                           
         MVC   P,SPACES                                                         
         LA    R5,P+1                                                           
         MVC   0(L'IDABBR,R5),IDABBR   USER ID                                  
         LA    R5,L'IDABBR+1(R5)                                                
         MVC   0(L'LDGNME,R5),LDGNME   LEDGER NAME                              
         LA    R5,L'LDGNME-1(R5)                                                
         BRAS  RE,FNDNXT               R5 TO NEXT SPACE                         
         LA    R5,1(R5)                SET R5 TO NEXT AREA                      
         EDIT  CHNUM,(6,0(R5)),FILL=0  CHECK NUMBER                             
         CLI   NUMTYP,0                                                         
         BE    *+10                                                             
         MVC   0(1,R5),NUMTYP                                                   
         LA    R5,7(R5)                                                         
         CLI   MODE,RUNLAST                                                     
         BE    DLN05                                                            
*                                                                               
         MVC   0(L'AC@REQ,R5),AC@REQ   REQ=                                     
         LA    R5,L'AC@REQ-1(R5)                                                
         BRAS  RE,FNDNXT              R5= NEXT SPACE                            
         MVI   0(R5),C'='                                                       
         LA    R5,1(R5)                                                         
         EDIT  SRREQ,(3,0(R5)),ALIGN=LEFT                                       
         AR    R5,R0                                                            
         CLC   SRROFC,SPACES                                                    
         BE    DLN05                                                            
         MVI   0(R5),C','                                                       
         MVC   1(2,R5),SRROFC                                                   
         LA    R5,3(R5)                                                         
*                                                                               
DLN05    CLC   ALPHAID,MCCANN         SPECIAL FOR MCMCANN                       
         BNE   DLN07                                                            
         CLI   QLEDGER,C'S'                                                     
         BNE   DLN07                                                            
         MVC   P+1(L'IDABBR),SPACES                                             
         LA    R5,1(R5)                                                         
         MVC   0(24,R5),=C'MEDIA INVESTMENT SERVICE'                            
         LA    R5,25(R5)                                                        
*                                                                               
DLN07    CLI   MODE,RUNLAST                                                     
         BE    DLN13                                                            
         CP    RUNTOT,=P'0'        LINEUP CHECK                                 
         BE    DLN15                                                            
         LA    R5,1(R5)                                                         
         LA    R3,SRACC                                                         
         MVC   0(L'AC@PAYEE,R5),AC@PAYEE                                        
         LA    R5,L'AC@PAYEE-1(R5)                                              
         BRAS  RE,FNDNXT            R5=NEXT SPACE                               
         MVI   0(R5),C'='                                                       
         LA    R5,1(R5)                                                         
         MVC   0(12,R5),SRACC+3                                                 
         CLI   SRACC+2,C'Q'        P OR Q MEANS USE PUBED                       
         BH    DLN15                                                            
         LA    R5,1(R5)                                                         
         CLC   SRACC+8(3),SPACES   REP                                          
         BE    DLN15                                                            
         MVC   0(12,R5),SPACES                                                  
         MVC   WORK(15),SRACC                                                   
         CLI   WORK+12,C'*'        THE OFFICE NONESENSE                         
         BNE   *+10                                                             
         MVC   WORK+12(2),SPACES                                                
         PACK  DUB(6),WORK+4(11)                                                
         MVC   DUB+5(1),WORK+14    EDITION                                      
         GOTO1 PUBEDIT,DMCB,DUB,(C'S',0(R5))                                    
         CLI   SRACC+12,C'*'                                                    
         BNE   DLN15                                                            
         LA    R2,1(R5)                                                         
         CLI   0(R2),C' '                                                       
         BE    *+12                                                             
         LA    R2,1(R2)                                                         
         B     *-12                                                             
         MVC   0(3,R2),SRACC+12    THE OFFICE CODE                              
         B     DLN15                                                            
*                                                                               
DLN13    MVC   0(L'AC@AGY,R5),AC@AGY    RUNLAST                                 
         LA    R5,L'AC@AGY-1(R5)   AGENCY=XX                                    
         BRAS  RE,FNDNXT           R5 TO NEXT SPACE                             
         MVI   0(R5),C'='                                                       
         MVC   1(2,R5),ALPHAID                                                  
         LA    R5,4(R5)                                                         
         MVC   0(L'AC@LGR,R5),AC@LGR                                            
         LA    R5,L'AC@LGR-1(R5)   LEDGER=XX                                    
         BRAS  RE,FNDNXT           R5 TO NEXT SPACE                             
         MVI   0(R5),C'='                                                       
         L     R2,ADLEDGER                                                      
         MVC   1(2,R5),1(R2)                                                    
         LA    R5,4(R5)                                                         
*&&                                                                             
DLN15    GOTO1 ADSQUASH,DMCB,P+1,110                                            
         GOTO1 PRINT,DMCB,P,=C'BL01'                                            
         MVC   P,SPACES                                                         
DLNX     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WRITE A TAPE FOR BOCLARO                                            *         
***********************************************************************         
         SPACE 1                                                                
BOTAPE   DS    0D                                                               
         NMOD1 0,**BO**                                                         
         L     RC,=A(LWS)                                                       
         CLI   MODE,PROCTRNS                                                    
         BE    BOC2                                                             
         BAS   RE,BLDLAST                                                       
         LA    R1,BMTAPE                                                        
         CLOSE (BMTAPE)                                                         
         B     BOCXT                                                            
*                                                                               
BOC2     CLI   BOSW,C'Y'           FIRST TIME                                   
         BNE   BOC3                                                             
         LA    R1,BMTAPE                                                        
         OPEN  (BMTAPE,(OUTPUT))                                                
         MVI   BOSW,C'N'                                                        
         MVC   BMTPDTE(2),RCDATE        MM                                      
         MVC   BMTPDTE+2(2),RCDATE+3    DD                                      
         MVC   BMTPDTE+4(2),RCDATE+6    YY                                      
         MVC   BMLDTE(2),RCDATE                                                 
         MVC   BMLDTE+2(2),RCDATE+6                                             
         CLC   QSTART,SPACES                                                    
         BE    BOC3                                                             
         MVC   BMTPDTE(4),QSTART+2 MMDD                                         
         MVC   BMTPDTE+4(2),QSTART YY                                           
         EJECT                                                                  
***********************************************************************         
* BUILD THE RECORD AND WRITE IT OUT                                   *         
***********************************************************************         
         SPACE 1                                                                
BOC3     MVC   BMVEND,SPACES                                                    
         LA    R2,SRACC                                                         
         CLI   3(R2),C'N'          IS IT NETWORK                                
         BNE   BOC4                IF NOT MUST BE T OR R                        
         MVC   BMVEND+3(3),4(R2)   ACCOUNT CODE NXXX                            
         MVC   BMVEND(3),=C'NTV'   NTVXXX                                       
         MVC   BMACCT,=C'051200'                                                
         CLC   4(3,R2),=C'991'     IS IT A SPECIAL                              
         BL    BOC6                I.E N991, N992                               
         CLC   4(3,R2),=C'993'     OR N993                                      
         BH    BOC6                IF NOT SPECIAL WE'VE GOT IT                  
         MVC   BMVEND(3),=C'INT'   NOW IT'S INTXXX                              
         MVC   BMACCT,=C'051600'   AND THE ACCOUNT CHANGES                      
         B     BOC6                THE REST IS THE SAME                         
*                                                                               
BOC4     MVC   BMACCT,=C'051300'                                                
         CLI   3(R2),C'R'                                                       
         BNE   *+10                                                             
         MVC   BMACCT,=C'051400'                                                
         MVC   BMVEND,4(R2)                                                     
         CLI   BMVEND+4,C' '                                                    
         BE    BOC5                                                             
         CLI   BMVEND+4,C'T'                                                    
         BE    BOC5                                                             
         MVI   BMVEND+5,C'M'                                                    
*                                                                               
BOC5     MVC   BMRSTA,=CL7'NONE'                                                
         CLI   SRTYPE,X'21'        STATION                                      
         BE    BOC6                                                             
         MVC   BMVEND(3),=C'R14'   SPECIAL FOR A REP FOR TV                     
         MVC   BMVEND+3(3),4(R2)                                                
         MVC   BMRSTA,SRCNTR+4     STATION FOR A REP INVOICE                    
         CLI   BMRSTA+4,C' '                                                    
         BE    BOC6                                                             
         CLI   BMRSTA+4,C'T'                                                    
         BE    BOC6                                                             
         MVI   BMRSTA+5,C'M'                                                    
         MVC   BMVEND(3),=C'R09'   SPECIAL FOR A REP FOR RADIO                  
*                                                                               
BOC6     MVC   BMCR,SPACES                                                      
         CP    SRTAMT,=P'0'                                                     
         BH    BOC8                                                             
         MVC   BMCR,=C'CA'                                                      
         AP    BOCRS,SRTAMT                                                     
         B     BOC10                                                            
*                                                                               
BOC8     AP    BOCASH,SRTAMT                                                    
*                                                                               
BOC10    UNPK  BMAMT,SRTAMT                                                     
         OI    BMAMT+9,X'F0'                                                    
         GOTO1 DATCON,DMCB,(1,SRTDTE),(X'20',WORK)                              
         MVC   BMDOFI(4),WORK+2    MMDD                                         
         MVC   BMDOFI+4(2),WORK    YY                                           
         MVC   BMALLOC(2),WORK+2   MM                                           
         MVC   BMALLOC+2(2),WORK   YY                                           
         MVC   BMDISC+1(1),WORK+1  YEAR                                         
         MVI   BMDISC+0,C'1'                                                    
         CLI   SRTDTE+1,X'04'                                                   
         BL    BOC12                                                            
         MVI   BMDISC+0,C'2'                                                    
         CLI   SRTDTE+1,X'07'                                                   
         BL    BOC12                                                            
         MVI   BMDISC+0,C'3'                                                    
         CLI   SRTDTE+1,X'10'                                                   
         BL    BOC12                                                            
         MVI   BMDISC+0,C'4'                                                    
*                                                                               
BOC12    MVC   BMINV,=C'BM-DDS'                                                 
         OC    SRXPY(SRXPYL),SRXPY EXTRA PAYMENT ELEMENT                        
         BZ    BOC16                                                            
         MVC   BMINV,SPACES                                                     
         LA    R1,SRXINV                                                        
         LA    R2,SRXINV+10                                                     
         CLI   0(R1),C' '                                                       
         BNE   *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-12                                                             
         CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         SR    R2,R1                                                            
         CH    R2,=H'6'                                                         
         BL    *+14                                                             
         BCTR  R2,R0                                                            
         LA    R1,1(R1)                                                         
         B     *-14                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   BMINV(0),0(R1)                                                   
*                                                                               
BOC16    LA    R1,BMTAPE                                                        
         LA    R0,BMREC                                                         
         PUT   (1),(0)                                                          
         AP    BORECS,=P'1'                                                     
*                                                                               
BOCXT    XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* BUILD THE TRAILER                                                   *         
***********************************************************************         
         SPACE 1                                                                
BLDLAST  NTR1                                                                   
         UNPK  BMTNOTRN,BORECS                                                  
         UNPK  BMTCASH,BOCASH                                                   
         UNPK  BMTACR,BOCRS                                                     
         OI    BMTNOTRN+3,X'F0'                                                 
         OI    BMTCASH+10,X'F0'                                                 
         OI    BMTACR+10,X'F0'                                                  
         LA    R1,BMTAPE                                                        
         LA    R0,BMTID                                                         
         PUT   (1),(0)                                                          
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
* SOME CONSTANTS                                                      *         
***********************************************************************         
         SPACE 1                                                                
BOSW     DC    C'Y'                                                             
BOCRS    DC    PL6'0'                                                           
BOCASH   DC    PL6'0'                                                           
BORECS   DC    PL4'0'                                                           
*                                                                               
BMREC    DS    0CL80                                                            
BMVEND   DS    CL6                 PAYEE                                        
         DC    C'   XA1  '                                                      
BMTPDTE  DS    CL6                 DATE OF TAPE CREATION                        
BMDOFI   DS    CL6                 END DATE OF BROADCAST MONTH                  
BMRSTA   DS    CL7                 STATION-IF PAYEE IS A REP                    
BMINV    DS    CL6                                                              
         DC    C'  '                                                            
BMACCT   DC    CL6'051300'                                                      
BMDISC   DS    CL2                                                              
BMALLOC  DS    CL4                 BROADCAST Y/M                                
BMAMT    DS    CL10                                                             
BMCR     DS    CL2                 'C/A' IF AMOUNT IS MINUS                     
         DC    CL9' '                                                           
BMLDTE   DS    CL4                 MM/YY OF TAPE CREATION                       
         DC    C'20'                                                            
*                                                                               
BMTID    DS    0CL80                                                            
         DC    C'B20'                                                           
BMTNOTRN DS    CL4                                                              
BMTCASH  DS    CL11                                                             
BMTACR   DS    CL11                                                             
         DC    C'00000000000'                                                   
         DC    CL40' '                                                          
*                                                                               
BMTAPE   DCB   DDNAME=BMTAPE,          DOS SYS005                      X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00080,                                            X        
               BLKSIZE=00800,          DOS BLKSIZE=00800               X        
               MACRF=PM                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CREATE AN EDICT TRANSFER FILE                                       *         
***********************************************************************         
         SPACE 1                                                                
EDTRN    DS    0D                                                               
         NMOD1 0,*EDTR*,R9                                                      
         L     RC,=A(LWS)                                                       
         CLC   ALPHAID,=C'CC'      TEST COKE TRANSFER                           
         BE    CCXACF                                                           
         CLC   ALPHAID,=C'H7'      TEST MINDSHARE 820 TRANSMISSION              
         BE    MIND820                                                          
         CLI   MODE,PROCTRNS                                                    
         BNE   EDTRNX                                                           
         L     R4,ADBXAREA                                                      
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'215'                                                 
*        MVC   BOXWIDTH,=F'209'                                                 
         DROP  R4                                                               
         TM    FTPSW,FTPINT        HAS IT BEEN INITIALIZED                      
         BO    EDTRN03                                                          
         MVI   FTPWRK,C' '                                                      
         MVC   FTPWRK+1(L'FTPWRK-1),FTPWRK                                      
         MVC   FTPWRK+4(5),=C'*HDR*'                                            
         MVC   FTPWRK+9(6),=C'EDICT='                                           
         MVC   FTPWRK+15(L'IDABBR),IDABBR DESTINATION ID                        
         MVC   FTPWRK+34(2),=C'WP'                                              
         GOTO1 PRINT,DMCB,FTPWRK,=C'BL01'                                       
*                                                                               
         MVI   FTPWRK,C' '                                                      
         MVC   FTPWRK+1(L'FTPWRK-1),FTPWRK                                      
         MVC   FTPWRK(5),=C'++DDS'                                              
         MVC   FTPWRK+6(5),=C'ACCXX'                                            
         MVC   FTPWRK+9(2),ALPHAID                                              
         MVC   FTPWRK+11(3),=C'TRN'                                             
         GOTO1 PRINT,DMCB,FTPWRK,=C'BL01'                                       
         OI    FTPSW,FTPINT                                                     
*                                                                               
EDTRN03  MVI   FTPWRK,C' '                                                      
         MVC   FTPWRK+1(L'FTPWRK-1),FTPWRK                                      
         LA    R2,FTPWRK                                                        
         USING FTPD,R2                                                          
         MVC   FTPTTYP,=C'03'      PAYABLE                                      
         MVC   FTPRTYP,=C'01'      DETAIL                                       
         MVC   FTPUNL(14),SRACC+1  U/L ACCOUNT                                  
         GOTO1 DATCON,DMCB,(1,SRTDTE),(X'20',FTPDTE)                            
         MVC   FTPCLI,SRXCLI       CLIENT NAME                                  
         MVC   FTPSTA,SRCNTR+3     STATION (IF REP)                             
         MVC   FTPINV,SRXINV       INVOICE                                      
         CLI   FTPINV,C' '         IF NONE                                      
         BH    *+10                                                             
         MVC   FTPINV(6),SRTREF    USE REFERENCE                                
         MVC   FTPOFF,SRTANL       OFFICE                                       
*                                                                               
         MVI   FTPTSGN,C'+'                                                     
         CP    SRTAMT,=P'0'                                                     
         BNL   *+8                                                              
         MVI   FTPTSGN,C'-'                                                     
         ZAP   DUB,SRTAMT                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  FTPTOT,DUB          AMOUNT DUE                                   
*                                                                               
         MVI   FTPGSGN,C'+'                                                     
         CP    SRTGST,=P'0'                                                     
         BNL   *+8                                                              
         MVI   FTPGSGN,C'-'                                                     
         ZAP   DUB,SRTGST                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  FTPGST,DUB          GST                                          
*                                                                               
         MVI   FTPPSGN,C'+'                                                     
         CP    SRTPST,=P'0'                                                     
         BNL   *+8                                                              
         MVI   FTPPSGN,C'-'                                                     
         ZAP   DUB,SRTPST                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  FTPPST,DUB          PST                                          
*                                                                               
         MVI   FTPCSGN,C'+'                                                     
         CP    SRTCD,=P'0'                                                      
         BNL   *+8                                                              
         MVI   FTPCSGN,C'-'                                                     
         ZAP   DUB,SRTCD                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  FTPCD,DUB           CD                                           
*                                                                               
         ZAP   DUB,SRTAMT          AMOUNT                                       
         SP    DUB,SRTGST          LESS GST                                     
         SP    DUB,SRTPST          LESS PST                                     
         SP    DUB,SRTCD           LESS CD                                      
         MVI   FTPLSGN,C'+'                                                     
         CP    DUB,=P'0'                                                        
         BNL   *+8                                                              
         MVI   FTPLSGN,C'-'                                                     
         OI    DUB+7,X'0F'                                                      
         UNPK  FTPLGST,DUB                                                      
         MVC   FTPCLIC,SRTCLIC     CLIENT CODE                                  
*                                                                               
         CLI   SYEQU,MEDQ          TEST SPOT PRINT                              
         BNE   EDTRN10                                                          
         SR    R1,R1                                                            
         ICM   R1,3,SRXEST         ESTIMATE NUMBER                              
         BZ    EDTRN05                                                          
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FTPESTN,DUB+6(2)                                                 
*                                                                               
EDTRN05  CLI   TYCDE,50            TEST PRINT REP                               
         BNE   EDTRN07                                                          
         MVC   FTPPUB,SRCNTR       CONTRA IS PUB                                
         MVC   FTPPUBN,SRCNME      PUB NAME                                     
*                                                                               
EDTRN07  CLC   SVUNIT(2),=C'SP'    TEST PRINT                                   
         BE    *+14                                                             
         CLC   SVUNIT(2),=C'SQ'    TEST PRINT - CANADIAN                        
         BNE   EDTRN25                                                          
         MVC   WORK(6),FTPDTE                                                   
         CLI   SRXPER,C' '                                                      
         BNH   *+10                                                             
         MVC   WORK(6),SRXPER      GET LAST DAY OF INSERTION MONTH              
         CLI   SRXPER+6,C' '                                                    
         BNH   *+10                                                             
         MVC   WORK(6),SRXPER+6    YYMMDD                                       
         LA    R3,DAYS                                                          
*                                                                               
EDTRN08  DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(X'30',WORK),(X'20',FTPINSD),(1,0)                   
*                                                                               
         OC    SRXINVD,SRXINVD     TEST INVOICE DATE                            
         BZ    EDTRN25                                                          
         GOTO1 DATCON,DMCB,(2,SRXINVD),(X'20',FTPINVD)                          
         B     EDTRN25                                                          
*                                                                               
EDTRN10  CLI   SYEQU,PRDQ          TEST PRODUCTION                              
         BNE   EDTRN25                                                          
         MVC   FTPPRDC,SROPRD      PRODUCT CODE                                 
         MVC   FTPJOBC,SROJOB      JOB CODE                                     
         MVC   FTPXAC,SRXAC                                                     
*                                                                               
EDTRN25  GOTO1 PRINT,DMCB,FTPWRK,=C'BL01'                                       
         MVI   FTPWRK,C' '                                                      
         MVC   FTPWRK+1(L'FTPWRK-1),FTPWRK                                      
*                                                                               
         SR    R3,R3               FIRST NARRATIVE                              
         ICM   R3,1,SRTNRL         LENGTH OF NARRATIVE                          
         BZ    EDTRNX                                                           
         GOTO1 ADSQUASH,DMCB,SRTNRR,(R3)                                        
         L     R3,DMCB+4                                                        
         STC   R3,SRTNRL           SET NEW LENGTH                               
         MVC   FTPTTYP,=C'03'      PAYABLE                                      
         MVC   FTPRTYP,=C'09'      NARRATIVE                                    
         CH    R3,=Y(L'FTPNARR)                                                 
         BNH   *+8                                                              
         LH    R3,=Y(L'FTPNARR)                                                 
         BCTR  R3,0                                                             
         EX    R3,*+4                                                           
         MVC   FTPNARR(0),SRTNRR                                                
         GOTO1 PRINT,DMCB,FTPWRK,=C'BL01'                                       
         MVI   FTPWRK,C' '                                                      
         MVC   FTPWRK+1(L'FTPWRK-1),FTPWRK                                      
*                                                                               
         LA    RF,SRTNRR+1(R3)     SECOND NARRATIVE                             
         ICM   R3,1,SRTNRL         LENGTH OF NARRATIVE                          
         SH    R3,=Y(L'FTPNARR+1)                                               
         BM    EDTRNX                                                           
         MVC   FTPTTYP,=C'03'      PAYABLE                                      
         MVC   FTPRTYP,=C'09'      NARRATIVE                                    
         EX    R3,*+4                                                           
         MVC   FTPNARR(0),0(RF)                                                 
         GOTO1 PRINT,DMCB,FTPWRK,=C'BL01'                                       
         MVI   FTPWRK,C' '                                                      
         MVC   FTPWRK+1(L'FTPWRK-1),FTPWRK                                      
EDTRNX   XMOD1 1                                                                
*                                                                               
DAYS     DC    C'3130292827',X'FF'                                              
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
**********************************************************************          
* MINDSHARE 820 3040 BANK TRANSMISSION-PUT RECORDS TO TAPE           *          
**********************************************************************          
***********                                                                     
* ACCFRST *                                                                     
***********                                                                     
         USING E820D,R7                                                         
MIND820  L     R7,=A(E820WRK)                                                   
*                                                                               
         CLI   MODE,RUNLAST                                                     
         BE    MRUNLAST                                                         
*                                                                               
         CP    SRCAMT,=P'0'         NEVER PROCESS NEGATIVE CHECK                
         BNH   NOWXIT                                                           
*                                                                               
         CLI   MODE,ACCFRST                                                     
         BE    MACCFRST                                                         
         CLI   MODE,PROCTRNS                                                    
         BE    MPROCTRN                                                         
         CLI   MODE,ACCLAST                                                     
         BE    MACCLAST                                                         
         B     NOWXIT                                                           
*                                                                               
************                                                                    
* ACCFRST  *                                                                    
************                                                                    
*                                                                               
MACCFRST ZAP   INVCNT,=P'0'         INITIALIZE                                  
         ZAP   INVPAY,=P'0'                                                     
         ZAP   INVCD,=P'0'                                                      
         ZAP   PRDPAY,=P'0'                                                     
         ZAP   PRDCD,=P'0'                                                      
         MVC   E8INFOS,SPACES                                                   
         MVI   E8ADR1,C' '                     CLEAR ADDRESS AREA               
         MVC   E8ADR1+1(E8ADRQ-1),E8ADR1                                        
         MVI   E8MTC,C'S'            DEFAULT IS STANDARD                        
         TM    SRSTAT,SRMAIL         WANT SPECIAL?                              
         BZ    *+8                   YES                                        
         MVI   E8MTC,C'D'            NO, D IS SPECIAL DELIVERY                  
*                                                                               
         EDIT  SRCAMT,E8CHAMT,ZERO=NOBLANK             CHECK AMOUNT             
         ZAP   DUB2,CHNUM                      BUMP TO PROPER CHECK             
         AP    DUB2,=P'1'                      NUMBER FOR TRANSMISSION          
         EDIT  (P8,DUB2),E8CHNUM,FILL=0                CHECK NUMBER             
         GOTO1 DATCON,DMCB,(1,SRCDTE),(X'20',E8CHDTE)  CHECK DATE               
         MVC   E8ANME,SRANME                           PAYEE NAME               
*                                                                               
* ADDRESS                                                                       
*                                                                               
         LA    R2,SRADDR          POINT TO ADDRESS                              
         LA    R3,L'SRADDR        LENGTH OF ONE ADDRESS LINE                    
         LA    R4,SRADDR+(3*L'SRADDR) FOURTH ADDRESS LINE                       
*                                                                               
MINDA10  CR    R2,R4              POINTING TO FIRST LINE?                       
         BE    MINDA25            MUST BE AN ERROR                              
         GOTO1 ADFORM,DMCB,((R3),(R4)),(20,E8CITY),                    +        
               E8STATE,(9,E8ZIP),(3,E8CTRY)                                     
         TM    DMCB+3,X'01'       1 OR 3 MEANS ERROR                            
         BO    MINDA15                                                          
         CLC   E8CITY,SPACES                                                    
         BNE   MINDA20                                                          
MINDA15  OC    0(L'SRADDR,R4),0(R4)  IF ZERO, THEN NO ADDRESS LINE              
         BZ    *+8                                                              
         AHI   R3,L'SRADDR        SEND ANOTHER LINE                             
         AHI   R4,-L'SRADDR       BACK UP                                       
         B     MINDA10                                                          
*                                                                               
MINDA20  TM    DMCB+3,X'02'       2 MEANS INCORRECT STATE/ZIP COMBO             
         BO    MINDA25                                                          
         MVC   E8ADR1,0(R2)                                                     
         LA    R2,L'SRADDR(R2)                                                  
         CR    R2,R4                                                            
         BE    MINDA30                                                          
         MVC   E8ADR2,0(R2)                                                     
         B     MINDA30                                                          
*                                                                               
MINDA25  MVC   E8ADR1(17),=CL17'*INVALID ADDRESS*'                              
         MVC   E8CITY(17),=CL17'*INVALID ADDRESS*'                              
         MVC   E8STATE,=CL17'*INVALID ADDRESS*'                                 
         MVC   E8ZIP,=CL17'*INVALID ADDRESS*'                                   
         MVC   E8CTRY,=CL17'*INVALID ADDRESS*'                                  
*       ---------------------------------------------                           
*                                                                               
MINDA30  L     R4,=A(E8ATAB)        820 AGENCY TABLE                            
*                                                                               
MINDA31  CLI   0(R4),X'FF'          MAKE SURE VALID AGENCY TO OUTPUT            
         BE    MINDA34                                                          
         CLC   ALPHAID,0(R4)        MATCH AGENCY                                
         BNE   MINDA33                                                          
         CLC   SYLDG,2(R4)          MATCH LEDGER                                
         BE    MINDA34                  OR                                      
         CLC   SYEQU,2(R4)          MATCH EQUATE                                
         BE    MINDA34                                                          
MINDA33  LA    R4,L'E8ATAB(R4)                                                  
         B     MINDA31                                                          
MINDA34  MVC   E8BNKNO,3(R4)         SAVE ADDRESS OF AGENCY TABLE               
*                                                                               
         L     R4,=A(E8H7)          820 AGENCY TABLE                            
         TM    ALTXSW,H7XOPN        TEST MINDSHARE FILE OPENED                  
         BO    MINDA35                                                          
         BAS   RE,MOPN              OPEN IT                                     
         OI    ALTXSW,H7XOPN                                                    
MINDA35  BAS   RE,MINDWOF                                                       
         B     NOWXIT                                                           
*                                                                               
************                                                                    
* PROCTRNS *                                                                    
************                                                                    
*                                                                               
MPROCTRN AP    INVPAY,SRTAMT           ACCUMULATE INVOICE AMOUNT                
         AP    INVCD,SRTCD             AND CASH DISCOUNT                        
         AP    PRDPAY,SRTAMT           ACCUMULATE TOTAL AMOUNT                  
         AP    PRDCD,SRTCD             AND TOTAL CASH DISCOUNT                  
*                                                                               
         L     RF,ANXTSR               RF = NEXT SORT RECORD                    
         TM    SCS,SCSEOF              LAST RECORD                              
         BO    *+14                                                             
         CLC   SREC(SRKLQ-1),0(RF)     TEST SAME INV (-1 FOR SUBREF)            
         BE    NOWXIT                  EQUAL, JUST EXIT                         
*                                                                               
         BAS   RE,MINDGET              GET INVOICE INFORMATION                  
*                                                                               
         AP    INVCNT,=P'1'            BUMP SEQUENCE                            
         EDIT  INVCNT,E8RMRSEQ,ZERO=NOBLANK,FILL=0                              
*                                                                               
         L     R4,=A(RMRHED)           REMMITTANCE INFORMATION                  
         CLC   E8INFOS,E8INFO          MAKE SURE INFO HAS CHANGED               
         BE    MINDP10                                                          
         CLC   E8INFO2,SPACES          IS INFO2 SPACES?                         
         BNE   MINDP05                 NO, CONTINUE                             
         MVC   E8INFO2,E8INFO          YES, COPY INFO TO INFO2                  
         B     MINDP10                                                          
*                                                                               
MINDP05  BAS   RE,MINDWOF                                                       
         AP    INVCNT,=P'1'            BUMP SEQUENCE                            
         EDIT  INVCNT,E8RMRSEQ,ZERO=NOBLANK,FILL=0                              
*                                                                               
MINDP10  MVC   E8INFOS,E8INFO                                                   
*                                                                               
         L     R4,=A(RMRDET)           REMMITTANCE INFORMATION                  
         BAS   RE,MINDWOF                                                       
         B     NOWXIT                                                           
*                                                                               
***********                                                                     
* ACCLAST *                                                                     
***********                                                                     
*                                                                               
MACCLAST AP    INVCNT,=P'1'                                                     
         EDIT  INVCNT,E8RMRSEQ,ZERO=NOBLANK,FILL=0                              
         EDIT  PRDPAY,E8CHAMT,2,ZERO=NOBLANK,ALIGN=LEFT,COMMAS=Y                
         AP    PRDPAY,PRDCD                                                     
         EDIT  PRDPAY,E8CHGRSS,2,ZERO=NOBLANK,ALIGN=LEFT,COMMAS=Y               
*                                                                               
         L     R4,=A(RMRTOT)        TOTAL RECS FOR 820                          
         BAS   RE,MINDWOF                                                       
         B     NOWXIT                                                           
*                                                                               
***********                                                                     
* RUNLAST *                                                                     
***********                                                                     
*                                                                               
MRUNLAST TM    ALTXSW,H7XOPN       TEST MINDSHARE ACTIVITY                      
         BNO   NOWXIT                                                           
         CLOSE (EDIOUT)            CLOSE MINDSHARE FILE                         
         B     NOWXIT                                                           
*                                                                               
*********************************************************************           
* WRITE EDI OUTPUT FILE                                             *           
*********************************************************************           
MINDWOF  NTR1                                                                   
*                                                                               
MWOF10   L     R2,AIO                                                           
         USING E820HDRD,R2                                                      
         MVC   E820SET,=C'820'      SET                                         
         MVC   E820SPA,SPACES                                                   
         MVC   E820SEG(6),0(R4)     SEQMENT AND SEQUENCE                        
         MVC   E820ZRO,=C'00000'                                                
         LA    R3,E820LNQ                                                       
         STCM  R3,3,ERECLEN        INITIALIZE RECORD LENGTH                     
         LA    R2,E820DATA-E820HDRD(R2)                                         
         LA    R4,6(R4)                                                         
*                                                                               
MWOF20   SR    R3,R3               R3=LENGTH OF DATA                            
         LR    RE,R4               RE=START OF DATA                             
MWOF30   CLI   0(R4),ESC           TEST ESCAPE SEQUENCE                         
         BE    MWOF40                                                           
         CLI   0(R4),EOR           TEST END OF RECORD                           
         BE    MWOF40                                                           
         LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         B     MWOF30                                                           
*                                                                               
MWOF40   LTR   R3,R3               TEST ANY DATA TO MOVE                        
         BZ    MWOF50                                                           
         SR    R1,R1               UPDATE RECORD LENGTH                         
         ICM   R1,3,ERECLEN                                                     
         AR    R1,R3                                                            
         STCM  R1,3,ERECLEN                                                     
         LR    R0,R2               R0=DESTINATION                               
         LR    R1,R3               R1 & RF = LENGTH                             
         LR    RF,R3                                                            
         MVCL  R0,RE               DATA TO IO AREA                              
         AR    R2,R3               R2 TO NEXT AREA                              
*                                                                               
MWOF50   CLI   0(R4),EOR           TEST END OF RECORD                           
         BE    MWOF60                                                           
         BAS   RE,XTRA             EXTRACT SPECIAL DATA                         
         L     R2,ERECNXT          R2=NEXT DATA AREA                            
         LA    R4,4(R4)            BUMP R4 PASSED ESCAPE SEQUENCE               
         B     MWOF20                                                           
*                                                                               
MWOF60   L     R2,AIO                                                           
         XC    E820LN(4),E820LN                                                 
         MVC   E820LN,ERECLEN      SET LENGTH                                   
*                                                                               
         PUT   EDIOUT,E820HDRD                                                  
*                                                                               
         LA    R4,1(R4)                                                         
         CLI   0(R4),EOT           TEST END OF TABLE                            
         BNE   MWOF10                                                           
         B     NOWXIT                                                           
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*******************************************************************             
* GET INVOICE INFORMATION,NUMBER,CD,AND INVOICE DATE/PERIOD       *             
*******************************************************************             
MINDGET  NTR1                                                                   
*                                                                               
         MVC   E8INFO,SPACES                                                    
         MVC   E8INFO2,SPACES                                                   
         MVC   E8INVDTE,SPACES                                                  
         MVC   E8INVNO,SPACES                                                   
*                                                                               
         CLI   SYEQU,MEDQ              IS IT A MEDIA CHECK?                     
         BNE   MINDG95                  NO                                      
         CLI   SYLDG,C'P'              IS IT PRINT?                             
         BE    MINDG55                  YES                                     
         CLI   SYLDG,C'Q'                                                       
         BE    MINDG55                  YES                                     
*                                                                               
         LA    R4,SRACC+4              R4 TO STATION                            
         CLI   TYCDE,00                SPOT STATION?                            
         BE    *+8                      YES                                     
         LA    R4,SRCNTR+4             FOR REP STATION IN CONTRA                
         LA    R5,E8INFO              R5 TO OUTPUT AREA                         
         MVC   0(L'AC@STA,R5),AC@STA   'STATION'                                
         LA    R5,E8INFO+L'E8INFO-1                                             
         BRAS  RE,FNDNXT               R5 TO NEXT SPACE                         
         MVI   0(R5),C':'              REPLACE WITH COLON                       
         MVC   1(5,R5),0(R4)           STATION CALL LETTERS                     
         MVC   5(3,R5),=C'-AM'         AND ADD THE BAND                         
         CLI   4(R4),C'A'              -AM                                      
         BE    MINDG10                                                          
         MVI   6(R5),C'F'                                                       
         CLI   4(R4),C'F'              -FM                                      
         BE    MINDG10                                                          
         MVC   5(3,R5),=C'-TV'         OR -TV                                   
*                                                                               
MINDG10  OC    SRXPY(SRXPYL),SRXPY     EXTRA PAYMENT ELEMENT                    
         BZ    MINDG110                                                         
*                                                                               
         LA    R5,E8INFO2                                                       
         MVC   0(L'SRXCLI,R5),SRXCLI   CLIENT NAME                              
         LA    R5,E8INFO2+L'E8INFO2-1                                           
         BRAS  RE,FNDNXT                                                        
         MVC   1(L'SRXPRD,R5),SRXPRD   PRODUCT NAME                             
         LA    R5,E8INFO2+L'E8INFO2-1                                           
         BRAS  RE,FNDNXT                                                        
         AHI   R5,1                                                             
         EDIT  SRXEST,(3,(R5)),FILL=0  ESTIMATE                                 
*       -----------------------------------                                     
MINDG20  MVC   E8INVNO(L'SRXINV),SRXINV                                         
         CLI   SRXPER+6,C' '                                                    
         BNE   MINDG25             IF NO END DO MMMDD/YY                        
         GOTO1 DATCON,DMCB,(0,SRXPER),(8,E8INVDTE)                              
         B     MINDG50                                                          
*                                                                               
MINDG25  CLI   SRACC+3,C'N'                                                     
         BE    MINDG35             NETWORK IS SPECIAL                           
*                                                                               
MINDG30  GOTO1 DATCON,DMCB,(0,SRXPER+6),(9,E8INVDTE) MMM/YY BROADCAST           
         B     MINDG50                                                          
*                                                                               
MINDG35  CLC   SRXPER(6),SRXPER+6     FOR NETWORK IF START=END                  
         BNE   MINDG40                                                          
         GOTO1 DATCON,DMCB,(0,SRXPER),(8,E8INVDTE) MMMDD/YY                     
         B     MINDG50                                                          
*                                                                               
MINDG40  DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,SRXPER),(0,SRXPER)                                
         GOTO1 DATCON,DMCB,(0,SRXPER+6),(0,SRXPER+6)                            
         GOTO1 GETBROAD,DMCB,(1,SRXPER),WORK,GETDAY,ADDAY                       
         GOTO1 GETBROAD,DMCB,(1,SRXPER+6),WORK+12,GETDAY,ADDAY                  
         CLC   WORK(12),WORK+12         IS IT ONE BROADCAST MONTH               
         BE    MINDG30                  IF IT IS DO IT LIKE SPOT                
         GOTO1 DATCON,DMCB,(0,SRXPER),(8,E8INVDTE) IF START NOT = END           
         GOTO1 DATCON,DMCB,(0,SRXPER+6),(8,WORK)  MMMDD-DD/YY                   
         MVI   E8INVDTE+5,C'-'                                                  
         MVC   E8INVDTE+6(5),WORK+3                                             
*                                                                               
MINDG50  B     MINDG110                                                         
*       -----------------------------------                                     
*                                                                               
MINDG55  CLI   TYCDE,00                PRINT PUBLICATION                        
         BNE   MINDG60                 YES                                      
         MVC   E8INFO(L'SRANME),SRANME PRINT PUB NAME                           
         B     MINDG95                  YES                                     
*                                                                               
MINDG60  CLC   SRCNTR+1(11),SPACES                                              
         BE    MINDG75                                                          
         MVC   E8INFO(L'SRCNME),SRCNME  CONTRA ACCOUNT NAME                     
         LA    R5,E8INFO+L'E8INFO-1                                             
         BRAS  RE,FNDNXT                                                        
         PACK  DUB(6),SRCNTR+1(11)                                              
         MVC   DUB+5(1),SRCNTR+11   EDITION                                     
         GOTO1 PUBEDIT,DMCB,DUB,(C'S',1(R5))                                    
         CLC   SRCNTR+9(3),=C'ZZZ'                                              
         BNE   MINDG70                                                          
         LA    RF,17(R5)                                                        
         CLI   0(RF),C','                                                       
         BE    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   1(3,RF),=C'ALL'                                                  
*                                                                               
MINDG70  MVI   0(R5),C'('                                                       
         LA    R5,17(R5)                                                        
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         MVI   1(R5),C')'                                                       
*                                                                               
MINDG75  OC    SRXPY(SRXPYL),SRXPY       EXTRA PAYMENT ELEMENT                  
         BZ    MINDG95                                                          
*                                                                               
         LA    R5,E8INFO2                                                       
         MVC   0(L'SRXCLI,R5),SRXCLI     USE CLIENT NAME                        
         LA    R5,E8INFO2+L'E8INFO2-1                                           
         BRAS  RE,FNDNXT                                                        
         MVC   1(L'SRXPRD,R5),SRXPRD                                            
*                                                                               
         MVC   E8INVNO(L'SRXINV),SRXINV                                         
*                                                                               
         GOTO1 DATCON,DMCB,(0,SRXPER),(9,E8INVDTE)                              
         CLI   SRXPER+6,0                                                       
         BE    MINDG85                                                          
         MVI   E8INVDTE+6,C'-'                                                  
         GOTO1 (RF),(R1),(0,SRXPER+6),(9,E8INVDTE+7)                            
         B     MINDG90                                                          
*                                                                               
MINDG85  CLC   SRXPER+4(2),=C'00'                                               
         BE    MINDG90                                                          
         MVC   E8INVDTE,SPACES   SINGLE DATE- IF PAID THAT WAY                  
         GOTO1 (RF),(R1),,(8,E8INVDTE)                                          
*                                                                               
MINDG90  B     MINDG110                                                         
*       ---------------------------------------                                 
MINDG95  MVC   E8INFO,SRCNME                                                    
         CLI   SROPRD,0                OTHERS ELEMENT                           
         BE    MINDG100                                                         
         LA    R5,E8INFO2                                                       
         MVC   0(L'AC@PRO,R5),AC@PRO       C'PRODUCT='                          
         LA    R5,E8INFO2+L'E8INFO2-1                                           
         BRAS  RE,FNDNXT                                                        
         MVI   0(R5),C'='                                                       
         MVC   1(L'SROPRD,R5),SROPRD              PRODUCT CODE                  
         LA    R5,E8INFO2+L'E8INFO2-1                                           
         BRAS  RE,FNDNXT                                                        
         MVC   1(L'AC@JOB,R5),AC@JOB       C'JOB='                              
         LA    R5,E8INFO2+L'E8INFO2-1                                           
         BRAS  RE,FNDNXT                                                        
         MVI   0(R5),C'='                                                       
         MVC   1(L'SROJOB,R5),SROJOB              JOB CODE                      
*                                                                               
MINDG100 MVC   E8INVNO(L'SRTREF),SRTREF                                         
         GOTO1 DATCON,DMCB,(1,SRTDTE),(10,E8INVDTE)  INVOICE DATE               
*       -------------------------------------------                             
MINDG110 EDIT  INVCD,E8CD,2,ALIGN=LEFT,MINUS=YES,ZERO=BLANK                     
         EDIT  INVPAY,E8INVNET,2,ALIGN=LEFT,MINUS=YES,COMMAS=YES                
         AP    INVPAY,INVCD                                                     
         EDIT  INVPAY,E8INVGRS,2,ALIGN=LEFT,MINUS=YES,COMMAS=YES                
*                                                                               
         ZAP   INVPAY,=P'0'                                                     
         ZAP   INVCD,=P'0'                                                      
*                                                                               
MINDGX   B     NOWXIT                                                           
         EJECT                                                                  
*                                                                               
*******************************************************************             
* EXTRA DATA                                                      *             
*  R4=A(4 BYTE ESCAPE SEQUENCE)                                   *             
*  R2=A(OUTPUT AREA)                                              *             
*******************************************************************             
*                                                                               
XTRA     NTR1  ,                                                                
         ST    R2,ERECNXT                                                       
         SR    R1,R1                                                            
         IC    R1,1(R4)             R1=LENGTH OF OUTPUT DATA                    
         SR    R3,R3                                                            
         ICM   R3,3,2(R4)           R3=EQU OF DATA TO PROCESS                   
         BCTR  R3,0                                                             
         SLL   R3,2                                                             
         L     R5,=A(XDATA)         START OF XDATA TABLE                        
         AR    R5,R3                INDEX INTO XDATA TABLE                      
         L     RF,0(R5)             RF=A(SOURCE)                                
         TM    0(R5),X'80'                                                      
         BNO   XTRA5                                                            
         BASR  RE,RF                RF=A(ROUTINE)                               
         BCTR  R1,0                                                             
         B     XTRA7                                                            
*                                                                               
XTRA5    BCTR  R1,0                                                             
         EX    R1,*+4              DATA TO OUTPUT RECORD                        
         MVC   0(0,R2),SPACES                                                   
*                                                                               
         AR    RF,R7               ADD DISPLACEMENT OF DATA INTO E820W          
         CLI   0(RF),0             BINARY ZEROS?                                
         BE    XTRA7               YES, DON'T MOVE IT                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,0(R5)            RF=LENGTH OF XDATA                           
         BCTR  RE,0                                                             
         CR    RE,R1                                                            
         BL    *+6                                                              
         LR    RE,R1               USE SHORTEST LENGTH                          
         EX    RE,*+4                                                           
         MVC   0(0,R2),0(RF)                                                    
*                                                                               
XTRA7    LA    R2,1(R1,R2)                                                      
         ST    R2,ERECNXT          UPDATE A(NEXT BYTE)                          
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,ERECLEN        UPDATE LENGTH                                
         LA    R3,1(R1,R3)                                                      
         STCM  R3,3,ERECLEN                                                     
         B     NOWXIT                                                           
         EJECT                                                                  
**********************************************************************          
* OPEN MINDSHARE 820 EDI FILE                                        *          
**********************************************************************          
*                                                                               
MOPN     NTR1  ,                                                                
         GOTO1 DYNALLOC,DMCB,(0,DDPARM),DSPARM                                  
         OPEN  (EDIOUT,OUTPUT)                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     NOWXIT                                                           
*                                                                               
         DROP  R7                                                               
         EJECT                                                                  
*                                                                               
**********************************************************************          
* COKE TRANSFER                                                      *          
**********************************************************************          
         SPACE 1                                                                
CCXACF   CLI   MODE,ACCFRST                                                     
         BNE   CCXPTN                                                           
         ZAP   COKESEQ,=P'1'       ADD TO SEQUENCE NUMBER                       
         ZAP   COKETOT,=P'0'                                                    
         B     NOWXIT                                                           
*                                                                               
CCXPTN   CLI   MODE,PROCTRNS                                                    
         BE    *+12                                                             
         CLI   MODE,ACCLAST                                                     
         BNE   CCXRUNL                                                          
         TM    ALTXSW,CCXOPN       TEST COKE FILE OPEN                          
         BO    CCXPTN3                                                          
         BAS   RE,COPN             OPEN IT                                      
         OI    ALTXSW,CCXOPN                                                    
*                                                                               
CCXPTN3  CP    SRTAMT,=P'0'        TEST BOTTLER BOUGHT                          
         BE    NOWXIT                                                           
         MVI   FTPWRK,C' '                                                      
         MVC   FTPWRK+1(L'FTPWRK-1),FTPWRK                                      
         LA    R2,FTPWRK                                                        
         USING CCHD,R2                                                          
         MVC   CCHPE,=C'AP20'                                                   
         GOTO1 DATCON,DMCB,(4,RCDATE),(X'20',WORK)                              
         MVC   CCHSYDTE(4),WORK+2    MMDD                                       
         MVC   CCHSYDTE+4(2),WORK    YY                                         
*                                                                               
         OI    CHNUM+(L'CHNUM-1),X'0F' CONTROL NUMBER                           
         UNPK  CCHCNTL,CHNUM           LAST 4 OF CHECK                          
         MVC   CCHVEND,SRVEND          VENDOR CODE                              
         CLI   MODE,ACCLAST                                                     
         BE    CCXACL                                                           
*                                                                               
         MVC   CCHRTYP,=C'IH'                                                   
         OI    COKESEQ+(L'COKESEQ-1),X'0F' SEQUENCE NUMBER                      
         UNPK  CCHSEQN,COKESEQ                                                  
*                                                                               
*  NOTE: REFERENCE (TRNREF) IS LAST 6 BYTES OF SPOT PAY                         
*        INVOICE NUMBER, FOR COKE.  IT'S BUILT IN ACPAYXFR                      
*                                                                               
         MVC   CCHINVN(6),SRXCLI   INVOICE FIELD: NAME(6)                       
         MVC   CCHINVN+6(6),SRTREF                INVOICE#(6)                   
         BAS   RE,GETINV            GET LAST EIGHT                              
         CLC   WORK,SPACES                                                      
         BNH   *+10                                                             
         MVC   CCHINVN+6(8),WORK    LAST EIGHT OF INVOICE                       
                                                                                
         MVC   CCHINVN+14(2),SRCNTR+3             PRODUCT CODE                  
         OC    CCHINVN,SPACES                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(1,SRTDTE),(X'20',WORK)                              
         MVC   CCHDATE(4),WORK+2       MMDD                                     
         MVC   CCHDATE+4(2),WORK       YY                                       
         MVC   CCHVCHR,SPACES                                                   
         MVC   CCHVCHR(5),SRACC+3      ACN NUMBER                               
         MVC   CCHVCHR+6(3),SRACC+8    AGENCY                                   
*                                                                               
         MVI   CCHMPAYI,C' '       MULTIPAY INDICATOR                           
         MVI   CCHCINDI,C'I'       NORMAL                                       
         AP    COKETOT,SRTAMT                                                   
         CP    SRTAMT,=P'0'                                                     
         BNL   *+8                                                              
         MVI   CCHCINDI,C'C'       CREDIT                                       
*                                                                               
         UNPK  CCHPAYA,SRTAMT      PAYABLE AMOUNT                               
         OI    CCHPAYA+L'CCHPAYA-1,X'F0'                                        
         MVI   CCHSTAX,C'0'        TAX                                          
         MVC   CCHSTAX+1(L'CCHSTAX-1),CCHSTAX                                   
         MVC   CCHTERM,=C'000'     TERMS                                        
         MVC   CCHTEXT,SRTNRR      IN TEXT FIELD                                
         PUT   CCTAPE,CCHD         PUT HEADER                                   
*                                                                               
         MVC   CCHTERM,SPACES                                                   
         MVC   CCHTEXT,SPACES                                                   
         MVC   CCHRTYP,=C'IL'      ** DETAIL ** LINE 1                          
         MVC   CCHLINE,=C'000001'                                               
         MVC   CCDEXPN,=C'EXP'                                                  
         MVC   CCDCOMP,=C'0002'                                                 
*                                                                               
         LA    R1,GLACTAB              GET ACCOUNT BASED ON YEAR                
CCXPTN6  CLC   SRXMOS(1),0(R1)                                                  
         BE    CCXPTN7                                                          
         CLI   0(R1),X'FF'             TAKE DEFAULT                             
         BE    CCXPTN7                                                          
         LA    R1,L'GLACTAB(R1)                                                 
         B     CCXPTN6                                                          
*                                                                               
CCXPTN7  MVC   CCDACT,1(R1)            GL ACCOUNT                               
         MVC   CCDDPT,=C'0230'                                                  
         MVC   CCDPAKG,=C'0000'                                                 
         MVC   CCDMATL,=C'0000'                                                 
         BAS   RE,GETPRDN          GET PRODUCT RECORD                           
         MVC   CCDPRDN,COKEGLN                                                  
         UNPK  CCDGRSL1,SRTGRS     GROSS                                        
         OI    CCDGRSL1+L'CCDGRSL1-1,X'F0'                                      
         PUT   CCTAPE,CCHD         PUT DETAIL - LINE 1                          
*                                                                               
         MVC   CCHLINE,=C'000002'  ** DETAIL ** LINE 2                          
         MVC   CCDACT,=C'4412 '                                                 
         MVC   CCDDPT,COKEDPT                                                   
         MVC   CCDPRDN,COKEGLN                                                  
         ZAP   DUB,SRTAMT                                                       
         SP    DUB,SRTGRS                                                       
         UNPK  CCDCOML2,DUB                                                     
         NI    CCDCOML2+L'CCDCOML2-1,X'0F'                                      
         OI    CCDCOML2+L'CCDCOML2-1,X'D0'                                      
         PUT   CCTAPE,CCHD         PUT DETAIL - LINE 2                          
         AP    COKESEQ,=P'1'       ADD TO SEQUENCE NUMBER                       
         B     NOWXIT                                                           
*                                                                               
CCXACL   MVC   CCHRTYP,=C'CG'      ** CONTROL **                                
         UNPK  CCCAMT,COKETOT                                                   
         MVC   CCOPID,=C'000020'                                                
         PUT   CCTAPE,CCHD         PUT CONTROL TOTAL                            
         B     NOWXIT                                                           
*                                                                               
CCXRUNL  CLI   MODE,RUNLAST                                                     
         BNE   NOWXIT                                                           
         TM    ALTXSW,CCXOPN       TEST COKE ACTIVITY                           
         BNO   NOWXIT                                                           
         BAS   RE,CCLSE            CLOSE COKE FILE                              
         B     NOWXIT                                                           
*                                                                               
GLACTAB  DS    0XL5                 MOA/GL ACCOUNT                              
         DC    X'98',C'2868'        EVEN YEARS TO 2868                          
         DC    X'99',C'2869'        ODD YEARS TO  2869                          
         DC    X'A0',C'2868'                                                    
         DC    X'A1',C'2869'                                                    
         DC    X'A2',C'2868'                                                    
         DC    X'A3',C'2869'                                                    
         DC    X'A4',C'2868'                                                    
         DC    X'A5',C'2869'                                                    
         DC    X'A6',C'2868'                                                    
         DC    X'A7',C'2869'                                                    
         DC    X'A8',C'2868'                                                    
         DC    X'A9',C'2869'                                                    
         DC    X'FF',C'2862'        DEFAULT - BEFORE 98                         
         EJECT                                                                  
**********************************************************************          
* GET PRODUCT NUMBER                                                 *          
**********************************************************************          
         SPACE 1                                                                
GETPRDN  NTR1  ,                                                                
         L     R3,AIO                                                           
         USING ACTKEY,R3                                                        
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL        PRODUCT NUMBER IS IN 3P                  
         MVC   ACTKUNT(2),=C'3P'                                                
         MVC   ACTKACT(2),SRCNTR+3     PRODUCT CODE                             
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,(R3),(R3)                             
         CLI   DMCB+8,0                                                         
         BE    *+6                 CAN'T READ PRODUCT RECORD                    
         DC    H'0'                                                             
         SR    R0,R0                                                            
         LR    R4,R3                                                            
         AH    R4,DATADISP                                                      
*                                                                               
         USING FFTELD,R4                                                        
GETPRDN3 CLI   0(R4),0                                                          
         BE    NOWXIT                                                           
         CLI   0(R4),FFTELQ                                                     
         BNE   GETPRDN7                                                         
         CLI   FFTTYPE,FFTTGLNO    GL CODE                                      
         BNE   *+14                                                             
         MVC   COKEGLN,FFTDATA                                                  
         B     GETPRDN7                                                         
         CLI   FFTTYPE,FFTTCDPT    DEPT CODE                                    
         BNE   *+14                                                             
         MVC   COKEDPT,FFTDATA                                                  
         B     GETPRDN7                                                         
*                                                                               
GETPRDN7 IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GETPRDN3                                                         
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* GET THE LAST EIGHT OF THE SPOT INVOICE NUMBER                      *          
**********************************************************************          
                                                                                
GETINV   NTR1  ,                                                                
         MVC   WORK,SPACES                                                      
         LA    RF,SRXINV+L'SRXINV-1  HOW LONG IS INVOICE NUMBER?                
         LA    R1,L'SRXINV                                                      
GETINV1  CLI   0(RF),C' '          FIND LAST CHARACTER                          
         BH    GETINV2                                                          
         SH    RF,=H'1'                                                         
         BCT   R1,GETINV1                                                       
*                                                                               
GETINV2  LTR   R1,R1               TEST ANY INVOICE                             
         BZ    NOWXIT                                                           
         LA    RF,SRXINV                                                        
GETINV3  CH    R1,=H'8'            TEST 8 OR LESS                               
         BNH   GETINV4                                                          
         LA    RF,1(RF)            BUMP TO LAST EIGHT                           
         BCT   R1,GETINV3                                                       
GETINV4  BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   WORK(0),0(RF)                                                    
         B     NOWXIT                                                           
         EJECT                                                                  
**********************************************************************          
* OPEN COKE FILE                                                     *          
**********************************************************************          
         SPACE 1                                                                
COPN     NTR1  ,                                                                
         GOTO1 DYNALLOC,DMCB,DYDCB,DYDSN                                        
         OPEN  (CCTAPE,(OUTPUT))                                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LINKX MF=(E,PARMLST),SF=(E,LINKLST)                                    
         B     NOWXIT                                                           
         EJECT                                                                  
**********************************************************************          
* CLOSE COKE FILE / SUBMIT ADVANTIS JOB                              *          
**********************************************************************          
         SPACE 1                                                                
CCLSE    NTR1  ,                                                                
         CLOSE (CCTAPE)            CLOSE COKE FILE                              
         TM    RNSW2,RUNTST        RUN=TEST                                     
         BO    NOWXIT                                                           
         MVC   TAPEC2,DYDSN         ACCTAPE.A055CC1                             
         MVC   TAPEC4,RTGEN         UPDATE GENERATION NUMBER                    
         LA    R0,NUMCRD            NUMBER OF JCL CARDS                         
         LA    R3,JCL                                                           
*                                                                               
CCLS5    MVC   POWJCL,0(R3)                                                     
         GOTO1 POWWOW,DMCB,=C'PUT',=C'POWER',POWKEY,POWHEAD                     
         LA    R3,L'JCL(R3)                                                     
         BCT   R0,CCLS5                                                         
*                                                                               
NOWXIT   XIT1                                                                   
*                                                                               
COKESEQ  DC    PL3'0'                                                           
COKETOT  DC    PL6'0'                                                           
COKEDPT  DC    CL4'0000'                                                        
COKEGLN  DC    CL4'0000'                                                        
         EJECT                                                                  
**********************************************************************          
* POWWOW CONSTANTS AND STORAGE                                       *          
**********************************************************************          
         SPACE 1                                                                
JCL      DS    0CL80                                                            
JOBC     DC    CL80'//CCOLA5T   JOB   ''CNTL'',MSGLEVEL=(1,1),COND=((0,X        
               NE)),'                                                           
         DC    CL80'//     MSGCLASS=X,CLASS=A,NOTIFY=CCOL'                      
         DC    CL80'//*MAIN CLASS=ADVANTIS'                                     
         DC    CL80'//SS  EXEC  ADVANTIS'                                       
*                                                                               
TAPEC1   DC    C'//TAPE  DD  DSN='                                              
TAPEC2   DS    CL(L'DYDSN)                                                      
TAPEC3   DC    CL1'.'                                                           
TAPEC4   DC    CL8'G0000000'                                                    
TAPEC5   DC    CL(80-(*-TAPEC1))',DISP=OLD,UNIT=TAPE'                           
*                                                                               
         DC    CL80'//INMSG    DD *'                                            
         DC    CL80' SEND '                                                     
         DC    CL80'     ACCOUNT(CCBS) USERID(CCBSKOD)'                         
         DC    CL80'     FILEID(DD:TAPE) FORMAT(N)'                             
*                                                                               
CLASC1   DC    C'     CLASS('                                                   
CLASC2   DC    C'PAYABLES'                                                      
CLASC3   DC    CL(80-(*-CLASC1))')'                                             
*                                                                               
         DC    CL80'     MODE( ) '                                              
         DC    CL80'     CHARGE(1) DATATYPE(E);'                                
         DC    CL80'//'                                                         
NUMCRD   EQU   (*-JCL)/80                                                       
*                                                                               
POWKEY   DC    CL10' '                                                          
POWHEAD  DC    XL8'00'                                                          
POWJCL   DS    CL80                                                             
*                                                                               
DYDCB    DC    CL8'CCTAPE'                                                      
DYDSN    DC    CL16'ACCTAPE.AC055CC1'                                           
DYND     DC    CL5' '                                                           
*                                                                               
PARMLST  CALL  ,(DYDCB,RTDSN),MF=L                                              
LINKLST  LINKX EP=FRGETDSN,SF=L                                                 
*                                                                               
RTDSN    DC    CL17' '       COKE.PAY.XFR                                       
RTGEN    DC    CL8' '        G0001V00                                           
RTND     DC    CL19' '       SPARE                                              
*                                                                               
DDPARM   DC    CL8'EDIOUT'             MINDSHARE EDI 820 OUTPUT                 
DSPARM   DC    CL20'ACCTAPE.AC055H71'                                           
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
EDIOUT   DCB   DDNAME=EDIOUT,                                          X        
               DSORG=PS,                                               X        
               MACRF=(PM),                                             X        
               RECFM=VB,                                               X        
               LRECL=4004,                                             X        
               BLKSIZE=32760                                                    
*                                                                               
CCTAPE   DCB   DDNAME=CCTAPE,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00165,                                            X        
               BLKSIZE=00165,                                          X        
               MACRF=PM                                                         
         EJECT                                                                  
RELOTAB  DS    0A                                                               
         DC    V(ACLIST)                                                        
         DC    V(DATVAL)                                                        
         DC    V(GETBROAD)                                                      
         DC    V(NUMTOLET)                                                      
         DC    V(PUBEDIT)                                                       
         DC    V(POWWOW)                                                        
         DC    V(ADFORM)                                                        
         DC    A(MAINTAB)                                                       
         DC    A(POST)                                                          
         DC    A(POSTIO)                                                        
         DC    A(TRNIO)                                                         
         DC    A(IO)                                                            
         DC    A(PROFLS)                                                        
         DC    A(WKBUFF)                                                        
         DC    A(WKBUFFP)                                                       
         DC    A(0)                PTNTAB                                       
         DC    A(REMTAB)                                                        
         DC    A(PDTAB)                                                         
         DC    A(PVOID)                                                         
         DC    A(PNUL)                                                          
         DC    A(BOTAPE)                                                        
         DC    A(EDTRN)                                                         
         DC    A(SYTAB)                                                         
         DC    A(ETXT1)            US TEXTS                                     
         DC    A(CTXT1)            CANADIAN TEXTS                               
         DC    A(SORTS)            SORT OPTIONS                                 
         DC    A(SORTFLD)          SORT FIELDS                                  
         DC    A(SORTCRD)          A(SORTCRD)                                   
         DC    A(RECCRD)           A(RECCRD)                                    
         DC    A(NXTSR)            NEXT SORT RECORD                             
         DC    A(RUNREG)           RUN THE REGISTER                             
         DC    A(DLN)              DEADLINE                                     
         DC    X'FF'                                                            
         EJECT                                                                  
*                                    US SPELLING                                
ETXT1    DCDD  AC#CHKT,15,L       'CHECK TOTAL '                                
ETXT2    DCDD  AC#CHKTS,18,L      'CHECK TOTALS '                               
ETXT3    DCDD  AC#GDCKS,15,L      'GOOD CHECKS '                                
ETXT4    DCDD  AC#VDCKS,15,L      'VOID CHECKS '                                
ETXT5    DCDD  AC#TCHKS,15,L      'TOTAL CHECKS '                               
ETXT6    DCDD  AC#NOADC,19,L      'NO ADDRESS CHECKS '                          
ETXT7    DCDD  AC#NOCHK,19,L      'NO CHECKS FOR '                              
ETXT8    DCDD  AC#TOFCK,19,L      'TOTALS FOR CHECK '                           
ETXTX    EQU   *                                                                
*                                                                               
*                                    CANADIAN SPELLING                          
CTXT1    DCDD  AC#CHQT,15,L       'CHEQUE TOTAL'                                
CTXT2    DCDD  AC#CHQTS,18,L      'CHEQUE TOTALS'                               
CTXT3    DCDD  AC#GDCQS,15,L      'GOOD CHEQUES'                                
CTXT4    DCDD  AC#VDCQS,15,L      'VOID CHEQUES'                                
CTXT5    DCDD  AC#TCHQS,15,L      'TOTAL CHEQUES'                               
CTXT6    DCDD  AC#NOADQ,19,L      'NO ADDRESS CHEQUES'                          
CTXT7    DCDD  AC#NOCHQ,19,L      'NO CHEQUES FOR'                              
CTXT8    DCDD  AC#TOFCQ,19,L      'TOTALS FOR CHEQUE'                           
CTXTX    EQU   *                                                                
*                                                                               
*                                                                               
SYTAB    DS    0C                                                               
         DC    C'P',AL1(MEDQ),C'PRINT      ',AL3(PRNT-AC5502),AL1(0)            
         DC    C'Q',AL1(MEDQ),C'CANAD PRINT',AL3(PRNT-AC5502),AL1(0)            
         DC    C'S',AL1(MEDQ),C'SPOT       ',AL3(SPOT-AC5502),AL1(0)            
         DC    C'T',AL1(MEDQ),C'CANAD SPOT ',AL3(SPOT-AC5502),AL1(0)            
         DC    C'U',AL1(MEDQ),C'NETWORK    ',AL3(SPOT-AC5502),AL1(0)            
         DC    C'V',AL1(PRDQ),C'PRODUCTION ',AL3(PRO1-AC5502),AL1(1)            
         DC    C'V',AL1(PRDQ),C'PRODUCTION ',AL3(PROD-AC5502),AL1(0)            
         DC    C'W',AL1(PRDQ),C'CANAD PRODN',AL3(PRO1-AC5502),AL1(1)            
         DC    C'W',AL1(PRDQ),C'CANAD PRODN',AL3(PROD-AC5502),AL1(0)            
         DC    C'X',AL1(EXPQ),C'EXPENSE    ',AL3(EXP1-AC5502),AL1(1)            
         DC    C'X',AL1(EXPQ),C'EXPENSE    ',AL3(EXPN-AC5502),AL1(0)            
         DC    C'Y',AL1(EXPQ),C'EXPENSE    ',AL3(EXP1-AC5502),AL1(1)            
         DC    C'Y',AL1(EXPQ),C'EXPENSE    ',AL3(EXPN-AC5502),AL1(0)            
         DC    C'X',AL1(CEXQ),C'EXPENDITURE',AL3(CXPN-AC5502),AL1(0)            
         DC    C' ',AL1(EXPQ),C'EXPENSE    ',AL3(EXPN-AC5502),AL1(0)            
         DC    X'FF'                                                            
*                                                           PRINT               
PRNT     DC    AL1(50),AL3(PRR-AC5502),AL1(3),AL1(TYPCD)    REP                 
         DC    AL1(09),AL3(AOR-AC5502),AL1(6),AL1(0)        AOR                 
         DC    AL1(26),AL3(AOR-AC5502),AL1(6),AL1(0)        AOR                 
         DC    AL1(00),AL3(PRP-AC5502),AL1(3),AL1(TYPCD)    PUB                 
*                                                                               
*                                                           SPOT                
SPOT     DC    AL1(34),AL3(SPR-AC5502),AL1(2),AL1(0)        REP                 
         DC    AL1(35),AL3(UWR-AC5502),AL1(7),AL1(0)        UNWIRED REP         
         DC    AL1(09),AL3(AOR-AC5502),AL1(6),AL1(0)        AOR                 
         DC    AL1(26),AL3(AOR-AC5502),AL1(6),AL1(0)        AOR                 
         DC    AL1(00),AL3(SPS-AC5502),AL1(2),AL1(0)        STATION             
*                                                                               
PROD     DC    AL1(09),AL3(AOR-AC5502),AL1(6),AL1(0)        AOR                 
         DC    AL1(51),AL3(AOR-AC5502),AL1(6),AL1(0)        AOR                 
         DC    AL1(26),AL3(AOR-AC5502),AL1(6),AL1(0)        AOR                 
         DC    AL1(00),AL3(EXP-AC5502),AL1(4),AL1(TYCLT)    PRODUCTION          
*                                                                               
PRO1     DC    AL1(09),AL3(AOR-AC5502),AL1(6),AL1(0)        AOR                 
         DC    AL1(51),AL3(AOR-AC5502),AL1(6),AL1(0)        AOR                 
         DC    AL1(26),AL3(AOR-AC5502),AL1(6),AL1(0)        AOR                 
         DC    AL1(00),AL3(PXN-AC5502),AL1(8),AL1(TYCKT)    PRODUCTION          
*                                                                               
CXPN     DC    AL1(00),AL3(CXP-AC5502),AL1(5),AL1(TYCKT)    COKE EXP            
*                                                                               
EXPN     DC    AL1(00),AL3(EXP-AC5502),AL1(4),AL1(TYCLT)    EXPENSE             
*                                                                               
EXP1     DC    AL1(00),AL3(PXN-AC5502),AL1(8),AL1(TYCKT)    EXPENSE             
         EJECT                                                                  
REQ      EQU   1                   REQUEST NUMBER                               
ACO      EQU   2                   ACCOUNT CODE                                 
ACN      EQU   3                   ACCOUNT NAME                                 
AMT      EQU   4                   CHECK TOTAL                                  
CSD      EQU   5                   CASH DISCOUNT                                
*                                                                               
*              SORT OPTION TABLE                                                
*              1 BYTE OPTION CODE, 4 BYTES FIELD EQUATES                        
*                                                                               
SORTS    DS    0CL5                                                             
         DC    C' ',AL1(REQ,ACO,000,000)    DEFAULT                             
         DC    C'N',AL1(REQ,ACN,000,000)                                        
         DC    C'C',AL1(REQ,ACO,000,000)                                        
         DC    C'A',AL1(REQ,AMT,000,000)                                        
         DC    C'D',AL1(REQ,CSD,000,000)                                        
         DC    X'FF'                                                            
*                                                                               
*              SORT FIELDS                                                      
*                                                                               
SORTFLD  DS    0F                                                               
         DC    AL1(REQ),AL1(L'RCRQTOT),S(RCRQTOT)                               
         DC    AL1(ACO),AL1(L'PAYACC),S(PAYACC)                                 
         DC    AL1(ACN),AL1(L'PAYNME),S(PAYNME)                                 
         DC    AL1(AMT),AL1(L'CAMT),S(CAMT)                                     
         DC    AL1(CSD),AL1(L'CCSD),S(CCSD)                                     
         DC    X'FF'                                                            
*                                                                               
SORTCRD  DC    CL80'SORT FIELDS=(01,000,A),FORMAT=BI,WORK=1'                    
RECCRD   DC    CL80'RECORD TYPE=F,LENGTH=(000)'                                 
                                                                                
***********************************************************************         
* IO AND BUFFER AREAS                                                 *         
***********************************************************************         
         SPACE 1                                                                
LWS      DC    (AC55DX-AC55D)X'00' LOCAL STORAGE                                
*                                                                               
         DS    0D                                                               
MAINTAB  DS    0CL16                                                            
         DC    CL8'*REMTAB*',AL4(((REMAX*RELEN+7)/8*8)+8)                       
         DC    A(REMDATA)                                                       
         DC    CL8'*PDTAB*',AL4(((PDMX*PDLNQ+7)/8*8)+8)                         
         DC    A(PDDATA)                                                        
         DC    CL8'*PTNTAB*',AL4((((PTNMX+1)*PTNLQ+7)/8*8)+8)                   
         DC    X'80',AL3(APTNTAB-AC55D)                                         
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*        820 3040 AGENCY BANK ACCOUNT TABLE                           *         
***********************************************************************         
         DS    0D                                                               
E8ATAB   DS    0CL13                                                            
*                                                                               
         DC    CL2'H7',AL1(MEDQ),CL10'3299985459'                               
         DC    CL2'H7',AL1(PRDQ),CL10'3299986077'                               
         DC    XL2'FFFF',XL1'FF',CL10'??????????'                               
*                                                                               
***********************************************************************         
*        820 3040 OUPTUT DETAILS                                                
***********************************************************************         
*                                                                               
CHKAMTQ  EQU   1                    CHECK AMOUNT                                
CHKDTQ   EQU   2                    CHECK DATE                                  
CHKNOQ   EQU   3                    CHECK NUMBER                                
BNKNOQ   EQU   4                    BANK ACCOUNT NUMBER                         
RMRSEQQ  EQU   5                    RMR SEQUENCE NUMBER                         
REFHEDQ  EQU   6                    REFERENCE HEADER DETAILS                    
REFDETQ  EQU   7                    REFERENCE DETAILS                           
PERIODQ  EQU   8                    PERIOD                                      
INVNOQ   EQU   9                    INVOICE NUMBER                              
INVAMTQ  EQU   10                   NET PAYABLE - GROSS INVOICE AMT             
CSHDISQ  EQU   11                   CASH DISCOUNT                               
NETAMTQ  EQU   12                   NET AMOUNT - INVOICE LESS DISCOUNT          
TOTGRSQ  EQU   13                   GROSS AMOUNT FOR TOTAL CHECK                
PNAMEQ   EQU   14                   PAYEE NAME                                  
PADDR1Q  EQU   15                   PAYEE ADDRESS LINE 1                        
PADDR2Q  EQU   16                   PAYEE ADDRESS LINE 2                        
PCITYQ   EQU   17                   PAYEE CITY                                  
PSTATEQ  EQU   18                   PAYEE STATE                                 
PZIPQ    EQU   19                   PAYEE POSTAL CODE                           
MLTYPEQ  EQU   20                   MAILING TYPE CODE                           
PCTRYQ   EQU   21                   COUNTRY CODE                                
*                                                                               
XDROUT   EQU   X'80'                EXECUTE A ROUTINE                           
*                                                                               
XDATA    DS    0F                   THE ORDER MUST MATCH THE EQU                
         DC    AL1(L'E8CHAMT),AL3(E8CHAMT-E820D)   CHECK AMOUNT                 
         DC    AL1(L'E8CHDTE),AL3(E8CHDTE-E820D)   CHECK DATE                   
         DC    AL1(L'E8CHNUM),AL3(E8CHNUM-E820D)   CHECK NUMBER                 
         DC    AL1(L'E8BNKNO),AL3(E8BNKNO-E820D)   BANK ACCOUNT NUMBER          
         DC    AL1(L'E8RMRSEQ),AL3(E8RMRSEQ-E820D) RMR SEQUENCE NUMBER          
         DC    AL1(L'E8INFO),AL3(E8INFO-E820D)     RMR REFERENCE DATA           
         DC    AL1(L'E8INFO2),AL3(E8INFO2-E820D)   RMR REFERENCE DATA           
         DC    AL1(L'E8INVDTE),AL3(E8INVDTE-E820D) IVOICE DATE                  
         DC    AL1(L'E8INVNO),AL3(E8INVNO-E820D)   INVOICE NUMBER               
         DC    AL1(L'E8INVGRS),AL3(E8INVGRS-E820D) NET PAYABLE                  
         DC    AL1(L'E8CD),AL3(E8CD-E820D)         CASH DISCOUNT                
         DC    AL1(L'E8INVNET),AL3(E8INVNET-E820D) INVOICE LESS DISC            
         DC    AL1(L'E8CHGRSS),AL3(E8CHGRSS-E820D) GROSS FOR CHECK              
         DC    AL1(L'E8ANME),AL3(E8ANME-E820D)     PAYEE NAME                   
         DC    AL1(L'E8ADR1),AL3(E8ADR1-E820D)     PAYEE ADDRESS 1              
         DC    AL1(L'E8ADR2),AL3(E8ADR2-E820D)     PAYEE ADDRESS 2              
         DC    AL1(L'E8CITY),AL3(E8CITY-E820D)     CITY                         
         DC    AL1(L'E8STATE),AL3(E8STATE-E820D)   STATE                        
         DC    AL1(L'E8ZIP),AL3(E8ZIP-E820D)       POSTAL CODE                  
         DC    AL1(L'E8MTC),AL3(E8MTC-E820D)       MAILING TYPE CODE            
         DC    AL1(L'E8CTRY),AL3(E8CTRY-E820D)     MAILING TYPE CODE            
*                                                                               
E820WRK  DC    (E820DX-E820D)C' '                                               
***********************************************************************         
*       MINDSHARE EDI 820 3040 RECORD LAYOUT TABLE                              
***********************************************************************         
*                                                                               
E8H7     DS    0D                                                               
         DC    C'000'                         SEGMENT                           
         DC    C'000'                         SEQUENCE NUMBER                   
         DC    CL22' '                        DOCUMENT NAME                     
         DC    CL2' '                         FUNCTIONAL GROUP ID               
         DC    CL15'DDS'                      =C'DDS'                           
         DC    CL15'H7-CHECKS'                PARTNER'S PROFILE ID              
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'BPR'                                                           
         DC    C'002'                                                           
         DC    CL1'C'              TRANS. HANDLING CODE 'C'-CHECK               
         DC    CL1'2'                        NUMBER OF DECIMAL PLACES           
         DC    AL1(ESC),AL1(15),AL2(CHKAMTQ) CHECK AMOUNT                       
         DC    CL1'C'                        CREDIT/DEBIT FLAG CODE             
         DC    CL3'CHK'                      PAYMENT METHOD CODE                
         DC    CL10'PBC'                     PAYMENT FORMAT                     
         DC    CL2'01'                       DFI ID NO. QUALIFIER               
         DC    CL12'061112788'               DFI ID NUMBER                      
         DC    CL2'DA'                       ACCT. NO. QUAL. CODE               
         DC    AL1(ESC),AL1(35),AL2(BNKNOQ)  BANK ACCOUNT NUMBER                
         DC    CL10' '        NOT USED       - ORIG. CO. ID                     
         DC    CL9' '         NOT USED       - ORIG. CO. CODE                   
         DC    CL2' '         NOT USED       - DFI ID NO. QUALIFIER             
         DC    CL12' '        NOT USED       - DFI ID NUMBER                    
         DC    CL2' '         NOT USED       - ACCT. NO. QUAL CODE              
         DC    CL35' '        NOT USED       - ACCOUNT NUMBER                   
         DC    AL1(ESC),AL1(6),AL2(CHKDTQ)   CHECK DATE                         
         DC    AL1(EOR)                                                         
*                                                                               
         DC    C'TRN'                                                           
         DC    C'004'                                                           
         DC    CL1'1'                        TRACE TYPE CODE                    
         DC    AL1(ESC),AL1(30),AL2(CHKNOQ)  REFERENCE NUMBER                   
         DC    AL1(EOR)                                                         
*                                          REF-REFERENCE NUMBERS                
         DC    C'REF'                                                           
         DC    C'006'                                                           
         DC    CL2'ZZ'                        REF NO. QUALIFIER                 
         DC    CL1'S'                         REF NO.-CHECK STYLE TYPE          
         DC    CL2'03'                          PRINT SITE (PHOENIX)            
         DC    CL2'48'                          CHECK STYLE ID                  
         DC    CL4'0048'                        REMITTANCE FORMAT ID            
         DC    CL4'0108'                        0108 = MINDSHARE                
         DC    CL3'425'                         RETURN ADDRESS ID-CHK           
         DC    AL1(ESC),AL1(1),AL2(MLTYPEQ)     MAILING TYPE CODE               
         DC    CL1' '                           BLANK                           
         DC    CL3'425'                         RETURN ADDRESS ID-ENV           
         DC    CL4'0107'                        SIGNATURE #1 ID                 
         DC    CL4'    '                        SIGNATURE #2 ID-BLANK           
         DC    AL1(EOR)                                                         
*                                          REF-REFERENCE NUMBERS                
         DC    C'REF'                                                           
         DC    C'006'                                                           
         DC    CL2'CK'                        REF NO. QUALIFIER                 
         DC    AL1(ESC),AL1(30),AL2(CHKNOQ)   REFERENCE NUMBER                  
         DC    AL1(EOR)                                                         
*                                          N1-NAME                              
         DC    C'N1 '                                                           
         DC    C'008'                                                           
         DC    CL2'PE'                        ENTITY ID CODE                    
         DC    AL1(ESC),AL1(35),AL2(PNAMEQ)   NAME                              
         DC    AL1(EOR)                                                         
*                                          N2-ADDITIONAL NAME INFO              
*        DC    C'N2 '                                                           
*        DC    C'009'                                                           
*        DC    AL1(ESC),AL1(35),AL2(PNAMEQ)   NAME                              
*        DC    AL1(EOR)                                                         
*                                          N3-ADDRESS INFORMATION               
         DC    C'N3 '                                                           
         DC    C'010'                                                           
         DC    AL1(ESC),AL1(35),AL2(PADDR1Q)  ADDRESS INFROMATION               
         DC    AL1(ESC),AL1(35),AL2(PADDR2Q)  ADDRESS INFROMATION               
         DC    AL1(EOR)                                                         
*                                          N3-ADDRESS INFORMATION               
         DC    C'N4 '                                                           
         DC    C'011'                                                           
         DC    AL1(ESC),AL1(30),AL2(PCITYQ)   CITY ADDRESS                      
         DC    AL1(ESC),AL1(2),AL2(PSTATEQ)   STATE OR PROVINCE CODE            
         DC    AL1(ESC),AL1(9),AL2(PZIPQ)     POSTAL CODE                       
         DC    AL1(ESC),AL1(3),AL2(PCTRYQ)    COUNTRY CODE                      
         DC    AL1(EOR)                                                         
*                                          ENT-ENTITY                           
         DC    C'ENT'                                                           
         DC    C'014'                                                           
         DC    CL1'0'                         NUMBER OF DECIMAL PLACES          
         DC    CL6'1'                         ASSIGNED NUMBER                   
         DC    AL1(EOR)                                                         
*                                                                               
         DC    AL1(EOT)                        END OF TABLE                     
*                                                                               
RMRHED   DC    C'RMR'                      RMR-REMITT ADVICE ACC                
         DC    C'037'                                                           
         DC    CL2'ZZ'                        REF NO. QUALIFIER                 
         DC    AL1(ESC),AL1(6),AL2(RMRSEQQ)   SEQUENCE NUMBER                   
         DC    CL3'001'                       RMR DETAIL LINE                   
         DC    AL1(EOR)                                                         
*                                          REF-REFERENCE NUMBERS                
         DC    C'REF'                                                           
         DC    C'039'                                                           
         DC    CL2'ZZ'                        REF. NUMBER QUALIFIER             
         DC    CL30'0001'                     REF. NUMBER                       
         DC    AL1(ESC),AL1(36),AL2(REFHEDQ)  MEDIA / CLIENT                    
         DC    AL1(EOR)                                                         
*                                                                               
         DC    AL1(EOT)                        END OF TABLE                     
*                                          REF-REFERENCE NUMBERS                
RMRDET   DC    C'RMR'                      RMR-REMITT ADVICE ACC                
         DC    C'037'                                                           
         DC    CL2'ZZ'                        REF NO. QUALIFIER                 
         DC    AL1(ESC),AL1(6),AL2(RMRSEQQ)   SEQUENCE NUMBER                   
         DC    CL3'001'                       RMR DETAIL LINE                   
         DC    AL1(EOR)                                                         
*                                          REF-REFERENCE NUMBERS                
         DC    C'REF'                                                           
         DC    C'039'                                                           
         DC    CL2'ZZ'                        REF. NUMBER QUALIFIER             
         DC    CL30'0001'                     REF. NUMBER                       
         DC    AL1(ESC),AL1(36),AL2(REFDETQ)  MEDIA / CLI / PRD                 
         DC    AL1(EOR)                                                         
*                                          REF-REFERENCE NUMBERS                
         DC    C'REF'                                                           
         DC    C'039'                                                           
         DC    CL2'ZZ'                        REF. NUMBER QUALIFIER             
         DC    CL30'0002'                     REF. NUMBER                       
         DC    AL1(ESC),AL1(18),AL2(PERIODQ)  PERIOD                            
         DC    AL1(EOR)                                                         
*                                          REF-REFERENCE NUMBERS                
         DC    C'REF'                                                           
         DC    C'039'                                                           
         DC    CL2'ZZ'                        REF. NUMBER QUALIFIER             
         DC    CL30'0003'                     REF. NUMBER                       
         DC    AL1(ESC),AL1(20),AL2(INVNOQ)   INVOICE NUMBER                    
         DC    AL1(EOR)                                                         
*                                          REF-REFERENCE NUMBERS                
         DC    C'REF'                                                           
         DC    C'039'                                                           
         DC    CL2'ZZ'                        REF. NUMBER QUALIFIER             
         DC    CL30'0004'                     REF. NUMBER                       
         DC    AL1(ESC),AL1(14),AL2(INVAMTQ)  NET PAYABLE                       
         DC    AL1(EOR)                                                         
*                                          REF-REFERENCE NUMBERS                
         DC    C'REF'                                                           
         DC    C'039'                                                           
         DC    CL2'ZZ'                        REF. NUMBER QUALIFIER             
         DC    CL30'0005'                     REF. NUMBER                       
         DC    AL1(ESC),AL1(8),AL2(CSHDISQ)  CASH DISCOUNT                      
         DC    AL1(EOR)                                                         
*                                          REF-REFERENCE NUMBERS                
         DC    C'REF'                                                           
         DC    C'039'                                                           
         DC    CL2'ZZ'                        REF. NUMBER QUALIFIER             
         DC    CL30'0006'                     REF. NUMBER                       
         DC    AL1(ESC),AL1(14),AL2(NETAMTQ)  NET AMOUNT                        
         DC    AL1(EOR)                                                         
*                                                                               
         DC    AL1(EOT)                        END OF TABLE                     
*                                                                               
RMRTOT   DC    C'RMR'                                                           
         DC    C'037'                                                           
         DC    CL2'ZZ'                        REF NO. QUALIFIER                 
         DC    AL1(ESC),AL1(6),AL2(RMRSEQQ)   SEQUENCE NUMBER                   
         DC    CL3'002'                       RMR TOTAL LINE                    
         DC    AL1(EOR)                                                         
*                                          REF-REFERENCE NUMBERS                
         DC    C'REF'                                                           
         DC    C'039'                                                           
         DC    CL2'ZZ'                        REF. NUMBER QUALIFIER             
         DC    CL30'0004'                     REF. NUMBER                       
         DC    AL1(ESC),AL1(14),AL2(TOTGRSQ)  NET PAYABLE                       
         DC    AL1(EOR)                                                         
*                                          REF-REFERENCE NUMBERS                
         DC    C'REF'                                                           
         DC    C'039'                                                           
         DC    CL2'ZZ'                        REF. NUMBER QUALIFIER             
         DC    CL30'0006'                     REF. NUMBER                       
         DC    AL1(ESC),AL1(14),AL2(CHKAMTQ)  NET AMOUNT                        
         DC    AL1(EOR)                                                         
*                                                                               
         DC    AL1(EOT)                        END OF TABLE                     
         EJECT                                                                  
*                                                                               
EOT      EQU   X'FF'               END OF TABLE                                 
EOR      EQU   X'00'               END OF RECORD                                
ESC      EQU   X'01'               ESCAPE                                       
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
         DS    0D                                                               
         DC    CL8'*POSTIO*'                                                    
POSTIO   DS    2000C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'*TRNIO*'                                                     
TRNIO    DS    2100C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'**IO**'                                                      
IO       DS    2000C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'*PROFLS*'                                                    
PROFLS   DS    800CL22                                                          
*                                                                               
         DS    0D                                                               
         DC    CL8'*WKBUFF*'                                                    
WKBUFF   DC    5000X'00'                                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'*WKBUFFP'                                                    
WKBUFFP  DC    5000X'00'                                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'*NXTSR*'                                                     
NXTSR    DS    CL(SRLNQ)          NEXT SORT RECORD                              
*                                                                               
REMAX    EQU   70                                                               
         DS    0D                                                               
         DC    CL8'*REMTAB*'                                                    
REMTAB   DC    F'0'                NUMBER IN TAB                                
         DC    AL3(0)                                                           
         DC    AL1(RELEN)          RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(REKEY)          KEY LENGTH                                   
         DC    AL4(REMAX)          MAX IN TABLE                                 
REMDATA  DS    A                   A(TABLE)                                     
*                                                                               
PDMX     EQU   1000                PRODUCTION - EXPENSE DEATAIL                 
         DS    0D                                                               
         DC    CL8'*PDTAB*'                                                     
PDTAB    DC    F'0'                NUMBER IN TAB                                
         DC    AL3(0)                                                           
         DC    AL1(PDLNQ)          RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(PDKLNQ)         KEY LENGTH                                   
         DC    AL4(PDMX)           MAX IN TABLE                                 
PDDATA   DS    A                   A(TABLE)                                     
*                                                                               
*                                                                               
PVOID    DC    CL40'V       V  OOOOO   III  DDDDDD    '                         
         DC    CL40' V     V  O     O   I   D     D   '                         
         DC    CL40'  V   V   O     O   I   D     D   '                         
         DC    CL40'   V V    O     O   I   D     D   '                         
         DC    CL40'    V      OOOOO   III  DDDDDD    '                         
*                                                                               
PNUL     DC    CL40'NN    N    U     U    LL          '                         
         DC    CL40'N N   N    U     U    LL          '                         
         DC    CL40'N  N  N    U     U    LI          '                         
         DC    CL40'N   N N    U     U    LL          '                         
         DC    CL40'N    NN     UUUUU     LLLLLLL     '                         
         TITLE 'PROGRAM EQUATES AND DSECTS'                                     
*                                                                               
TOTCHAT  EQU   27                                                               
TOTFIGS  EQU   39                                                               
*                                                                               
*              SYSTEM EQUATES                                                   
*                                                                               
MEDQ     EQU   X'80'               MEDIA                                        
PRDQ     EQU   X'40'               PRODUCTION                                   
EXPQ     EQU   X'20'               EXPENSE                                      
CEXQ     EQU   X'10'               COKE EXPENDITURE                             
*                                                                               
PTNMX    EQU   3000                MAXIMUM NUMBER OF TRANSACTIONS               
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* DSECT FOR LOCAL STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
AC55D    DSECT                                                                  
*                                                                               
ATYPES   DS    0A                                                               
ACLIST   DS    V                                                                
DATVAL   DS    V                                                                
GETBROAD DS    V                                                                
NUMTOLET DS    V                                                                
PUBEDIT  DS    V                                                                
POWWOW   DS    V                                                                
ADFORM   DS    V                                                                
AMAINTAB DS    A                                                                
APOST    DS    A                                                                
APOSTIO  DS    A                                                                
ATRNIO   DS    A                                                                
AIO      DS    A                                                                
APROF    DS    A                                                                
AWKBUFF  DS    A                                                                
AWKBUFFP DS    A                                                                
APTNTAB  DS    A                                                                
AREMTAB  DS    A                                                                
APDTAB   DS    A                                                                
APVOID   DS    A                                                                
APNUL    DS    A                                                                
ABOCLARO DS    A                                                                
AEDTRN   DS    A                                                                
ASYSTAB  DS    A                                                                
AETXT1   DS    A                   A(US TEXT FIELDS)                            
ACTXT1   DS    A                   A(US TEXT FIELDS)                            
ASORTS   DS    A                   A(SORT OPTIONS)                              
ASORTFLD DS    A                   A(SORT FIELDS)                               
ASORTCRD DS    A                   A(SORTCRD)                                   
ARECCRD  DS    A                   A(RECCRD)                                    
ANXTSR   DS    A                   A(NEXT SORT RECORD)                          
ARUNREG  DS    A                   A(RUN THE REGISTER)                          
ADLN     DS    A                   A(DEADLINE)                                  
*                                                                               
MAINLEN  DS    F                   LENGTH OF GETMAIN AREA                       
MAINBGN  DS    A                   A(START OF GET MAIN)                         
*                                                                               
LUID     DS    XL8                 LUID                                         
PQID     DS    XL7                 PQID                                         
WKID     DS    XL(L'WKINDEX)       WORKER FILE ID - CHECK FILE                  
WKIDP    DS    XL(L'WKINDEX)       WORKER FILE ID - POSTING FILE                
*                                                                               
TYPCHK   DS    CL1                 TYPE OF CHECK RUN                            
TYPDDS   EQU   C'C'                DDS OVERNIGHT                                
TYPSOON  EQU   C'S'                SOON                                         
TYPLOCL  EQU   C'L'                LOCAL CHECKS                                 
*                                                                               
RNSW     DS    XL1                 RUN CONTROL                                  
RNACT    EQU   X'80'               ANY ACTIVITY                                 
SOON     EQU   X'40'               SOON                                         
PROF     EQU   X'20'               PROFILE LIST HAS BEEN BUILD                  
FSTWK    EQU   X'10'               WORKER FILE USED FIRST TIME                  
STACK    EQU   X'08'               STACKED REQUESTS                             
MICR     EQU   X'04'               MICR                                         
RERNY    EQU   X'02'               RERUN = Y                                    
RERNF    EQU   X'01'               RERUN = F                                    
ALL      EQU   X'FF'                                                            
*                                                                               
RNSW2    DS    XL1                 RUN CONTROL                                  
WRTNO    EQU   X'80'               RCWRITE=NO                                   
RUNTST   EQU   X'40'               RUN=TEST                                     
*                                                                               
RQSW     DS    XL1                 REQUEST CONTROL                              
RQURG    EQU   X'80'               URGENT                                       
RQAPP    EQU   X'40'               APPROVED                                     
RQCRD    EQU   X'20'               CREDITS                                      
RQACT    EQU   X'10'               ACTIVITY                                     
RQCSD    EQU   X'08'               CASH DISCOUNT ONLY                           
*                                                                               
LGSW     DS    XL1                 LEDGER CONTROL                               
APPRV    EQU   X'80'               USE APPROVAL SYSTEM                          
CANAD    EQU   X'40'               CANADIAN LEDGER                              
CEXPN    EQU   X'20'               COKE EXPENDITURE LEDGER                      
UNAUTH   EQU   X'08'               UNAUTHORIZED USER                            
*                                                                               
PRNTSW   DS    XL1                 PRINT CONTROL                                
NUMBER   EQU   X'80'               PRINT NUMBER ON CHECK                        
SHUTTLE  EQU   X'40'               SHUTTLE                                      
LASER    EQU   X'20'               LASER                                        
*                                                                               
MCSW     DS    XL1                 MODE CONTROL SWITCH                          
ACF      EQU   X'80'               ACCFRST                                      
SBF      EQU   X'40'               SUBACFRST                                    
*                                                                               
SCS      DS    XL1                 SORT CONTROL SWITCH                          
SCSACT   EQU   X'80'               SORT ACTIVITY                                
SCSREC   EQU   X'40'               RECORD IN SREC                               
SCSEOF   EQU   X'20'               EOF                                          
*                                                                               
FTPSW    DS    XL1                 FILE TRANSFER SWITCH                         
FTPTRN   EQU   X'80'               FILE TRANSFER                                
FTPINT   EQU   X'40'               FILE INITIALIZE                              
*                                                                               
ALTXSW   DS    XL1                 COKE TRANSFER SWITCH                         
CCXTRN   EQU   X'80'               COKE FILE TRANSFER                           
CCXOPN   EQU   X'40'               COKE FILE IS OPEN                            
H7XOPN   EQU   X'20'               MINDSHARE FILE IS OPEN                       
*                                                                               
SYENT    DS    0CL17               SYSTEM ENTRY                                 
SYLDG    DS    CL1                 LEDGER CODE                                  
SYEQU    DS    CL1                 EQUATE                                       
SYNME    DS    CL11                NAME                                         
SYTYP    DS    AL3                 TYPE TABLE                                   
SYFMT    DS    CL1                 FORMAT OPTION                                
*                                                                               
TYENT    DS    0CL6                TYPE TABLE                                   
TYCDE    DS    XL1                 TYPE CODE                                    
TYRTN    DS    AL3                 PROCESSING ROUTINE                           
TYSBP    DS    AL1                 SUB-PROGRAM CODE                             
TYSTA    DS    XL1                 STATUS                                       
TYCKT    EQU   X'20'               SUB-ROUTINE DOES CHECK TOTAL                 
TYPCD    EQU   X'10'               SPECIAL EDIT FOR PRINT C.D.                  
TYCLT    EQU   X'08'               PRINT A CLIENT TOTAL LINE                    
*                                                                               
SORTOPT  DS    CL(L'SORTS)         SORT OPTION                                  
*                                                                               
REQTAB   DS    XL84                                                             
*                                                                               
RNTOTS   DS    0PL6                RUN ACCUMULATORS                             
RUNTOT   DS    PL6                 TOTAL CHECKS                                 
WRKREC   DS    PL6                 WORKER RECORDS                               
WRKAMT   DS    PL6                 WORKER CASH                                  
VOIDS    DS    PL6                 NUMBER OF VOIDS                              
CHOKS    DS    PL6                 NUMBER OF GOOD CHECKS                        
CHUSED   DS    PL6                 NUMBER OF CHECKS USED                        
NOADD    DS    PL6                 NUMBER OF NO ADDRESS CHECKS                  
*                                                                               
ACTOTS   DS    0PL6                ACCOUNT ACCUMULATORS                         
CHTOT    DS    PL6                 CHECK AMOUNT                                 
CHCSD    DS    PL6                 CASH DISCOUNT                                
CHC1     DS    PL6                 NET PAYABLE                                  
CHC2     DS    PL6                 CASH DISCOUNT                                
PUBTOT   DS    PL6                 PUBLICATION TOTALS FOR PRINT                 
PUBC1    DS    PL6                                                              
PUBC2    DS    PL6                                                              
CLITOT   DS    PL6                                                              
STATOT   DS    PL6                                                              
PRDNET   DS    PL6                 PRODUCT TOTALS FOR COKE                      
PRDCD    DS    PL6                 (USED AS CD TOTAL FOR 820)                   
PRDPAY   DS    PL6                 (USED AS PAY TOTAL FOR 820)                  
ITEMS    DS    PL6                                                              
INVPAY   DS    PL6                 (USED AS CURRENT PAY FOR 820)                
INVCD    DS    PL6                 (USED AS CURRENT CD FOR 820)                 
INVCNT   DS    PL6                 (USED ALSO AS COUNT FOR 820)                 
*                                                                               
SBTOTS   DS    0PL6                SUBACCOUNT TOTALS                            
SUBTOT   DS    PL6                                                              
SUBC1    DS    PL6                                                              
SUBC2    DS    PL6                                                              
*                                                                               
TNTOTS   DS    0PL6                TRANSACTION TOTALS                           
INVNET   DS    PL6                                                              
SUBSAVE  DS    PL6                 LAST TRANSACTION AMOUNT                      
*                                                                               
NTNTOT   EQU   (*-TNTOTS)/6                                                     
NSBTOT   EQU   (*-SBTOTS)/6                                                     
NACTOT   EQU   (*-ACTOTS)/6                                                     
NRNTOT   EQU   (*-RNTOTS)/6                                                     
*                                                                               
CAMT     DS    PL6                 CHECK AMOUNT, COMPLIMENT                     
CCSD     DS    PL6                 CASH DISCOUNT, BINARY COMPLIMENT             
CD       DS    PL6                 TRANSACTION CASH DISCOUNT                    
*                                                                               
CHNUM    DS    PL4                 CURRENT CHECK NUMBER                         
CURLNE   DS    PL2                 CURRENT LINE NUMBER                          
DLNLNE   DS    PL2                 DEADLINE                                     
MAXLNE   DS    PL2                 MAXIMUM LINES ON REMITTANCE                  
ML1LNE   DS    PL2                 MAX ON REMITTANCE LESS ONE                   
ML2LNE   DS    PL2                 MAX ON REMITTANCE LESS TWO                   
ML3LNE   DS    PL3                 MAX ON REMITTANCE LESS THREE                 
*                                                                               
TXT1     DS    CL(ETXT2-ETXT1)     CHECK/CHEQUE TOTAL                           
TXT2     DS    CL(ETXT3-ETXT2)     CHECK/CHEQUE TOTALS                          
TXT3     DS    CL(ETXT4-ETXT3)     GOOD CHECKS/CHEQUES                          
TXT4     DS    CL(ETXT5-ETXT4)     VOID CHECKS/CHEQUES                          
TXT5     DS    CL(ETXT6-ETXT5)     TOTAL CHECKS/CHEQUES                         
TXT6     DS    CL(ETXT7-ETXT6)     NO ADDRESS CHECKS/CHEQUES                    
TXT7     DS    CL(ETXT8-ETXT7)     NO CHECKS/CHEQUES FOR                        
TXT8     DS    CL(ETXTX-ETXT8)     TOTALS FOR CHECK/CHEQUE                      
TXTLQ    EQU   *-TXT1                                                           
*                                                                               
PAYACC   DS    CL15                PAYEE ACCOUNT                                
PAYNME   DS    CL36                PAYEE ACCOUNT NAME                           
*                                                                               
WKREC    DS    F                                                                
AREA     DS    CL320                                                            
*                                                                               
NUMTRN   DS    H                   NUMBER OF TRANSACTIONS IN TABLE              
VOIDSW   DS    CL1                                                              
SKIP     DS    CL4                                                              
ENDATE   DS    CL3                                                              
BANKNAME DS    CL36                                                             
BANKEY   DS    CL20                                                             
SUBTXT   DS    CL50                                                             
ACCTXT   DS    CL50                                                             
NUMTYP   DS    CL1                                                              
*                                                                               
ASBRTN   DS    A                   A(SUB-ROUTINE FOR THIS ACCOUNT)              
ANXTRN   DS    A                   A(NEXT TRANSACTION IN TABLE)                 
SAVEACC  DS    CL49                                                             
FILTER   DS    CL1                                                              
*                                                                               
CLPR     DS    0CL6                CLIENT/PRODUCT                               
CLNT     DS    CL3                 CLIENT CODE                                  
PRDT     DS    CL3                 PRODUCT CODE                                 
*                                                                               
CLPRXF   DS    CL6                 CLIENT/PRODUCT EXCLUDE FILTER                
*                                                                               
CLNTF    DS    CL3                 CLIENT FILTER                                
PRDTF    DS    CL3                 PRODUCT FILTER                               
*                                                                               
MINCHK   DS    PL6                 MINIMUM CHECK                                
MAXCHK   DS    PL6                 MAXIMUN CHECK                                
*                                                                               
ACLIPRO  DS    A                   A(CLIENT PROFILE)                            
CNTFK    DS    CL25                CTFILE KEY                                   
OFFLIST  DS    CL25                                                             
THISBRD  DS    CL3                 FIRST DAY OF THIS BROADCAST MONTH            
LASTBRD  DS    CL3                 FIRST DAY OF LAST BROADCAST MONTH            
NEXTBRD  DS    CL3                 LAST DAY OF NEXT BROADCAST MONTH             
DATECK   DS    CL1                                                              
CHKDATE  DS    CL3                                                              
IDABBR   DS    CL7                 ORIGIN ID                                    
POWCODE  DS    CL2                 POWER CODE                                   
*                                                                               
DUB2     DS    D                                                                
MNTH     DS    CL2                                                              
TODAY1   DS    XL3                 PWOS                                         
TODAY2   DS    XL2                 COMPRESSED                                   
TODAY3   DS    XL3                 BINARY                                       
RQOFFC   DS    CL2                 REQUESTING OFFICE FILTER                     
POOFFC   DS    CL2                 POSTING OFFICE                               
OFFICE   DS    CL2                 TRANSACTION OFFICE                           
CLILIST  DS    CL45                                                             
LDGNME   DS    CL36                LEDGER NAME                                  
CHKTYPE  DS    CL1                 LAST TRNTYPE - TO GET OVERLAY                
PBLK     DS    5CL132                                                           
SVSTA    DS    CL5                                                              
CLINO    DS    CL3                                                              
DUBX     DS    D                                                                
*                                                                               
STATUS   DS    XL1                                                              
CHKAUTH  EQU   X'10'                                                            
*                                                                               
ELCODE   DS    XL1                                                              
PUBNO    DS    CL12                                                             
CLISAVE  DS    CL12                                                             
INVNO    DS    CL6                                                              
THISACC  DS    CL(TRNKCULC+5-TRNRECD)                                           
LASTACC  DS    CL(TRNKCULC+5-TRNRECD)                                           
RECWRK   DS    CL100               WORK AREA FOR TABLE RECORD                   
CLSAVE   DS    (L'RECLI)C          SAVE FOR CLIENT CHANGE TOTALS                
DIR      DS    CL8                                                              
ACDCB    DS    A                   A(DCB)                                       
MNACKEY  DS    CL42                MONACC KEY                                   
STKKEY   DS    XL42                STACK KEY                                    
*                                                                               
NARRWRK  DS    CL250                                                            
FTPWRK   DS    CL(FTPLNQ)                                                       
         EJECT                                                                  
SREC     DS    0CL(SRLNQ)          SORT RECORD                                  
SRVAR    DS    CL50                VARIABLE SORT FIELDS                         
SRREQ    DS    PL3                 REQUEST NUMBER                               
SRACC    DS    CL15                ACCOUNT CODE                                 
SRCLI    DS    CL3                 CLIENT CODE                                  
SRSEQ    DS    XL2                 CHECK SEQUENCE                               
SRACLQ   EQU   *-SREC              ACCOUNT LENGTH                               
SRKOFC   DS    CL2                 KEY OFFICE                                   
SRCNTR   DS    CL15                CONTRA ACCOUNT                               
SRCNLQ   EQU   *-SREC              CONTRA LENGTH                                
SRTDTE   DS    XL3                 TRANSACTION DATE                             
SRTREF   DS    CL6                 TRANSACTION REFERENCE                        
SRTSBR   DS    XL1                 TRANSACTION SUBREFERENCE                     
SRKLQ    EQU   *-SRVAR             SORT KEY LENGTH                              
*                                                                               
SRTAMT   DS    PL6                 TRANSACTION AMOUNT                           
SRTCD    DS    PL6                 TRANSACTION CASH DISCOUNT                    
SRTGST   DS    PL6                 TRANSACTION GST                              
SRTPST   DS    PL6                 TRANSACTION PST                              
SRTGRS   DS    PL6                 TRANSACTION GROSS                            
SRTANL   DS    CL2                 TRANSACTION OFFICE                           
SRTNRR   DS    CL200               NARRATIVE                                    
SRTNRL   DS    XL1                 NARRATIVE LENGTH                             
SRTCLIC  DS    CL3                 CLIENT CODE                                  
*                                                                               
SRANME   DS    CL36                ACCOUNT NAME                                 
SRANML   DS    XL1                 NAME LENGTH                                  
SRADDR   DS    4CL(L'ADRADD1)      ADDRESS                                      
SRVEND   DS    CL10                COKE VENDOR NUMBER                           
SRCNME   DS    CL36                CONTRA ACCOUNT NAME                          
SRCNML   DS    XL1                 CONTRA NAME LENGTH                           
*                                                                               
SROTH    DS    0C                  DATA FROM OTHER ELEMENT                      
SROPRD   DS    CL3                 PRODUCT                                      
SROEST   DS    CL3                 ESTIMATE                                     
SROJOB   DS    CL6                 JOB                                          
SROADV   DS    XL2                 MOS                                          
SROTHL   EQU   *-SROTH                                                          
*                                                                               
SRXPY    DS    0C                  DATA FROM EXTRA PAY ELEMENT                  
SRXCD    DS    PL6                 CASH DISCOUNT                                
SRXCLI   DS    CL20                CLIENT NAME                                  
SRXPRD   DS    CL20                PRODUCT NAME                                 
SRXINV   DS    CL14                INVOICE NUMBER                               
SRXPER   DS    CL17                PERIOD                                       
SRXEST   DS    XL2                 ESTIMATE NUMBER                              
SRXINVD  DS    XL2                 INVOICE DATE                                 
SRXPYL   EQU   *-SRXPY                                                          
*                                                                               
SRXMOS   DS    XL2                 MOS                                          
*                                                                               
SRMTD    DS    0C                  DATA FROM MEDIA TRANSFER ELEMENT             
SRMADV   DS    XL2                 MOS                                          
SRMTDL   EQU   *-SRMTD                                                          
*                                                                               
SRXAC    DS    CL12                EXPENSE ACCOUNT(PROD X-JOBS)                 
*                                                                               
SRADV    DS    XL2                 MONTH OF ADVERTISING (MEDIA MOS)             
SRROFC   DS    CL2                 REQUEST OFFICE                               
SRSTAT   DS    XL1                 REQUEST STATUS BYTE                          
SRSURG   EQU   X'80'               REQUEST FOR URGENT                           
SRSZRO   EQU   X'40'               ZERO REQUEST                                 
SRMAIL   EQU   X'20'               SPECIAL MAIL OPTION FOR 820 TRANS            
SROPT    DS    XL1                 OPTIONS                                      
SROWRD   EQU   X'80'               PRINT AMOUNT IN WORDS                        
SRMNTH   DS    CL2                 MONTH OF ACTIVITY                            
SRCDTE   DS    XL3                 CHECK DATE                                   
SRCAMT   DS    PL6                 CHECK AMOUNT                                 
SRCCD    DS    PL6                 CHECK CASH DISCOUNT                          
SRTYPE   DS    XL1                 ACCOUNT TYPE                                 
SR#INV   DS    XL2                 # OF INVOICES                                
SRSYENT  DS    CL(L'SYENT)         SYSTEM ENTRY                                 
SRTYENT  DS    CL(L'TYENT)         TYPE ENTRY                                   
SRLNQ    EQU   *-SRVAR                                                          
         EJECT                                                                  
***********************************************************************         
* DATA DICTIONARY ITEMS                                               *         
***********************************************************************         
         SPACE 1                                                                
DICO     DS    0C                                                               
         DSDDL PRINT=YES                                                        
         DS    XL1                                                              
AC55DX   EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* DSECT FOR A PAYABLE TRANSACTION TABLE ENTRY                         *         
***********************************************************************         
         SPACE 1                                                                
PTND     DSECT                                                                  
PTNREC   DS    0CL(PTNLQ)                                                       
PTNKEY   DS    0C                                                               
PTNCLI   DS    CL3                 CLIENT                                       
PTNCKEY  DS    0C                  CONTRA ACCOUNT                               
PTNOFF   DS    CL2                 OFFICE                                       
PTNCON   DS    CL15                CONTRA ACCOUNT                               
PTNDTE   DS    CL3                 DATE                                         
PTNREF   DS    CL6                 REFERENCE                                    
PTNSBR   DS    CL1                 SUBREFERENCE                                 
PTNCKLQ  EQU   *-PTNCKEY                                                        
PTNKLQ   EQU   *-PTNKEY                                                         
PTNCNM   DS    CL36                CONTRA ACCOUNT NAME                          
PTNCLN   DS    XL1                 LENGTH OF CONTRA NAME                        
PTNAMT   DS    PL6                 AMOUNT                                       
PTNCD    DS    PL6                 CASH DISCOUNT                                
PTNTAMT  DS    PL6                 CHECK TOTAL                                  
PTNTCD   DS    PL6                 CHECK TOTAL CD                               
PTNSEQ   DS    XL2                 CHECK SEQUENCE NUMBER                        
PTN#INV  DS    XL2                 # OF INVOICES                                
PTNLQ    EQU   *-PTNKEY                                                         
         EJECT                                                                  
***********************************************************************         
* DSECT FOR BINSRCH PARAMETERS                                        *         
***********************************************************************         
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT INTO RECORD                     
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAX NUMBER IN TABLE                          
BINTABLE DS    A                                                                
         SPACE 2                                                                
***********************************************************************         
* DSECT FOR REMITTANCE LINE TABLE                                     *         
***********************************************************************         
         SPACE 1                                                                
REMITD   DSECT                                                                  
RECLI    DS    CL20                CLIENT                                       
REEST    DS    CL2                 ESTIMATE                                     
REMOS    DS    CL11                MONTH OF SERVICE                             
REINV    DS    CL10                INVOICE NUMBER                               
REKEY    EQU   *-REMITD                                                         
REAMT    DS    PL6                 AMOUNT                                       
RENUM    EQU   (*-REAMT)/(L'REAMT)                                              
RELEN    EQU   *-REMITD                                                         
         SPACE 2                                                                
***********************************************************************         
* DSECT FOR PROFILE RECORDS                                           *         
***********************************************************************         
         SPACE 1                                                                
PRFD     DSECT                                                                  
PRFLEN   DS    0CL22                                                            
PRFCOMP  DS    CL1                 COMPANY                                      
PRFLEDG  DS    CL2                 UNIT/LEDGER                                  
PRFCLI   DS    CL3                 CLIENT                                       
PRFVAL   DS    0CL16               PROFILE VALUES                               
PRFXALL  DS    CL1                 Y= EXCLUDE FROM REQUEST FOR ALL              
PRFX2MTH DS    CL1                 Y=EXCLUDE 2 BROADCAST MONTHS                 
PRFX1MTH DS    CL1                 Y=EXCLUDE LAST BROADCAST MONTH               
PRFI1MTH DS    CL1                 Y=INCLUDE NEXT BROADCAST MONTH               
PRFPAIO  DS    CL1                 Y=PAY APPROVED ITEMS ONLY                    
         DS    CL11                SPARE                                        
         EJECT                                                                  
***********************************************************************         
* DSECT FOR THE NEW PRODUCTION - EXPENSE PRINT LINE                   *         
***********************************************************************         
         SPACE 1                                                                
PPD      DSECT                                                                  
         DS    CL1                                                              
PPDTE    DS    CL8                 DATE MM/DD/YY                                
         DS    CL3                                                              
PPINV    DS    CL15                INVOICE NUMBER                               
         DS    CL2                                                              
PPDSP    DS    CL20                DESCRIPTION                                  
         DS    CL1                                                              
PPGRS    DS    CL11                GROSS                                        
         DS    CL2                                                              
PPCD     DS    CL9                 DISCOUNT                                     
         DS    CL2                                                              
PPNET    DS    CL11                NET                                          
PPLNQ    EQU   *-PPD                                                            
         SPACE 2                                                                
***********************************************************************         
* DSECT FOR A DETAIL TABLE ENTRY                                      *         
***********************************************************************         
         SPACE 1                                                                
PDD      DSECT                                                                  
PDDTE    DS    XL3                 DATE                                         
PDINV    DS    CL15                INVOICE NUMBER                               
PDDSP    DS    CL20                DESCRIPTION                                  
PDKLNQ   EQU   *-PDD               KEY LENGTH                                   
PDBK     DS    0PL6                                                             
PDGRS    DS    PL6                 GROSS                                        
PDCD     DS    PL6                 DISCOUNT                                     
PDNET    DS    PL6                 NET                                          
PDCNT    EQU   (*-PDBK)/(L'PDBK)                                                
PDLNQ    EQU   *-PDD                                                            
         EJECT                                                                  
***********************************************************************         
* DSECT FOR A FTP RECORD                                                        
***********************************************************************         
         SPACE 1                                                                
FTPD     DSECT                                                                  
FTPTTYP  DS    CL2                 TRANSFER TYPE                                
FTPRTYP  DS    CL2                 RECORD TYPE                                  
FTPUNL   DS    CL2                 UNIT/LEDGER                                  
FTPACC   DS    CL12                ACCOUNT (PAYEE)                              
         DS    CL1                 N/D                                          
FTPDTE   DS    CL6                 DATE (YYMMDD)                                
         DS    CL1                 N/D                                          
FTPCLI   DS    CL20                CLIENT NAME                                  
         DS    CL1                 N/D                                          
FTPSTA   DS    CL9                 STATION (IF PAYEE IS REP)                    
         DS    CL1                 N/D                                          
FTPINV   DS    CL14                INVOICE                                      
         DS    CL1                 N/D                                          
FTPOFF   DS    CL2                 OFFICE                                       
         DS    CL1                 N/D                                          
FTPTOT   DS    CL12                TOTAL                                        
FTPTSGN  DS    CL1                 SIGN                                         
FTPGST   DS    CL12                GST                                          
FTPGSGN  DS    CL1                 SIGN                                         
FTPLGST  DS    CL12                LESS GST                                     
FTPLSGN  DS    CL1                 SIGN                                         
FTPPUB   DS    CL12                PUB CODE(IF REP)                             
         ORG   FTPPUB                                                           
FTPXAC   DS    CL12                EXPENSE ACCOUNT(IF X-JOB)                    
         DS    CL3                 N/D                                          
FTPCLIC  DS    CL3                 CLIENT CODE                                  
FTPPRDC  DS    CL3                 PRODUCT CODE                                 
FTPESTN  DS    CL6                 ESTIMATE NUMBER                              
         ORG   FTPESTN                                                          
FTPJOBC  DS    CL6                 JOB CODE                                     
FTPPUBN  DS    CL36                PUB NAME                                     
FTPPST   DS    CL12                PST                                          
FTPPSGN  DS    CL1                 SIGN                                         
FTPCD    DS    CL12                CASH DISCOUNT                                
FTPCSGN  DS    CL1                 SIGN                                         
FTPINSD  DS    CL6                 INSERTION DATE YYMMDD                        
FTPINVD  DS    CL6                 INVOICE DATE YYMMDD                          
FTPLAST  DS    0C                                                               
         ORG   FTPUNL                                                           
FTPNARR  DS    CL(FTPLAST-FTPUNL)   NARRATIVE                                   
FTPLNQ   EQU   *-FTPD                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT FOR A COKE TRANSFER RECORD                                              
***********************************************************************         
         SPACE 1                                                                
CCHD     DSECT                                                                  
CCHPE    DS    CL4                 PAY ENTITY(ALWAYS AP20)                      
CCHSYDTE DS    CL6                 MMDDYY - RUN DATE                            
CCHCNTL  DS    CL4                 CONTROL NUMBER                               
         DS    CL8                 FILLER                                       
CCHSEQN  DS    CL6                 SEQUENCE NUMBER                              
CCHLINE  DS    CL6                 LINE NUMBER                                  
CCHRTYP  DS    CL2                 RECORD TYPE IH                               
CCHVEND  DS    CL10                VENDOR NUMBER                                
CCHINVN  DS    CL16                INVOICE NUMBER                               
CCHDATE  DS    CL6                 INVOICE DATE                                 
CCHVCHR  DS    CL10                VOUCHER NUMBER                               
CCHMPAYI DS    CL1                 MULTIPAY INDICATOR(ALWAYS N)                 
CCHPAYA  DS    CL15                PAYABLE AMOUNT                               
CCHCINDI DS    CL1                 CREDIT INDICATOR(I OR C)                     
CCHSTAX  DS    CL15                SALES TAX                                    
CCHTEXT  DS    CL52                TEXT                                         
CCHTERM  DS    CL3                 TERMS(ALWAYS 000)                            
CCHLQ    EQU   *-CCHD              RECORD LENGTH                                
*                                                                               
         ORG   CCHVCHR             ** DETAIL RECORD **                          
*                                  LINE NUMBER 1=GROSS, 2=COMMISSION            
CCDEXPN  DS    CL3                 EXPENSE(ALWAYS EXP)                          
CCDCOMP  DS    CL4                 COMPANY(ALWAYS 0002)                         
CCDACT   DS    CL4                 ACCOUNT LINE1=2862 LINE2=4412                
CCDDPT   DS    CL4                 DEPT    LINE1=0230 LINE2=0631                
CCDPRDN  DS    CL4                 PRODUCT NUMBER (G/L BRAND)                   
CCDPAKG  DS    CL4                 PACKAGE CODE=0000                            
CCDMATL  DS    CL4                 MATERIAL CODE=0000                           
CCDGRSL1 DS    CL15                GROSS AMOUNT (LINE 1)                        
         ORG   CCDGRSL1                                                         
CCDCOML2 DS    CL15                COMMISSION (LINE 2) PAYABLE - GROSS          
*                                                                               
         ORG   CCHINVN             ** CONTROL RECORD **                         
CCCAMT   DS    CL15                CONTROL TOTAL                                
CCOPID   DS    CL6                 OPERATOR ID                                  
CCCLQ    EQU   *-CCHD                                                           
         ORG   CCHD+CCHLQ                                                       
         EJECT                                                                  
*                                                                               
***********************************************************************         
* EDI 820 DATA TRANSMISSION WORK AREA                                 *         
***********************************************************************         
*                                  820 RECORD DATA                              
E820D    DSECT                                                                  
*                                                                               
ERECLEN  DS    H                   LENGTH OF EDI 820 REC FIELD                  
ERECNXT  DS    F                   NEXT EDI 820 REC DATA AREA                   
*                                                                               
E8BNKNO  DS    CL10                BANK ACCOUNT NUMBER                          
E8CHNUM  DS    CL10                CHECK NUMBER                                 
E8CHDTE  DS    CL8                 CHECK DATE                                   
E8CHAMT  DS    CL15                CHECK AMOUNT                                 
E8CHGRSS DS    CL15                TOTAL INVOICE GROSS FOR CHECK                
*                                                                               
E8ANME   DS    CL35                PAYEE NAME                                   
E8ADR1   DS    CL(L'ADRADD1)       ADDRESS LINE 1                               
E8ADR2   DS    CL(L'ADRADD1)       ADDRESS LINE 2                               
E8CITY   DS    CL20                CITY                                         
E8STATE  DS    CL2                 STATE                                        
E8ZIP    DS    CL9                 ZIP OR POSTAL CODE                           
E8CTRY   DS    CL3                 COUNTRY CODE                                 
E8ADRQ   EQU   *-E8ADR1                                                         
*                                                                               
E8RMRSEQ DS    CL6                 RMR SEQUENCE NUMBER                          
E8INFO   DS    CL36                INFORMATION HEADER FIELD                     
E8INFO2  DS    CL60                INFORMATION FIELD                            
E8INFOS  DS    CL36                INFORMATION SAVE FIELD                       
E8MTC    DS    CL1                 MAILING TYPE CODE                            
E8INVDTE DS    CL18                INVOICE DATE                                 
E8INVNO  DS    CL20                INVOICE NUMBER                               
E8INVGRS DS    CL14                GROSS INVOICE AMOUNT                         
E8CD     DS    CL10                CASH DISCOUNT                                
E8INVNET DS    CL14                NET AMOUNT                                   
E820DX   EQU   *                                                                
         EJECT                                                                  
*                                                                               
***********************************************************************         
* DSECT FOR AN EDI 820 OUTPUT RECORD HEADER                                     
***********************************************************************         
*                                                                               
E820HDRD DSECT                                                                  
E820LN   DS    XL2                 LENGTH                                       
         DS    XL2                                                              
E820SET  DS    CL3                 SET (820)                                    
E820SPA  DS    CL3                 SPACES                                       
E820SEG  DS    CL3                 SEGEMENT                                     
E820SEQ  DS    CL3                 SEQUENCE                                     
E820ZRO  DS    CL5                 SPARE (00000)                                
E820LNQ  EQU   *-E820LN                                                         
E820DATA DS    0C                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACGENPOST                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACOFFALD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACOFFALD                                                       
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDSYSELD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSYSELD                                                       
         PRINT ON                                                               
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
* DMWRKRD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKRD                                                        
         PRINT ON                                                               
* DMWRKRK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKRK                                                        
         PRINT ON                                                               
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
* SRUPDD                                                                        
         PRINT OFF                                                              
       ++INCLUDE SRUPDD                                                         
         PRINT ON                                                               
* DDLANGEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDLANGEQUS                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'097ACREP5502S10/26/00'                                      
         END                                                                    
