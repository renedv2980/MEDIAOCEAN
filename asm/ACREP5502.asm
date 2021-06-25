*          DATA SET ACREP5502  AT LEVEL 033 AS OF 08/19/20                      
**PROCESS USING(WARN(15))                                                       
*PHASE AC5502A                                                                  
*INCLUDE ACLIST                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE NUMTOLET                                                               
*INCLUDE PUBEDIT                                                                
*INCLUDE RIGHT                                                                  
*INCLUDE DLFLD                                                                  
*INCLUDE PQPROF                                                                 
*--------------------------------------------------------------------*          
* MNAS 10/27/14 - DSSUP-3770 - MAINTAIN ORIGINAL TRNRED WHEN SEQ                
*                 ROLLS OVER 255                                                
*                                                                               
* THIS VERSION ENABLES MQ TESTING ON TST                                        
*--------------------------------------------------------------------*          
                                                                                
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
         MVC   UPSI,MCUPSI         RUN OPTIONS                                  
         MVC   VSSB,MCSSB          V(SSB)                                       
         MVC   PQID,MCREMPQK       PQ ID                                        
         MVC   VMQRPT,MCVMQRPT     MQ INTERFACE                                 
         MVC   MCUSRDMP(4),MAINBGN START OF STORAGE AREA                        
         STCM  R1,15,MCUSRDMP+4    SET END OF STORGE AREA                       
         MVC   XSPRM1,APTNTAB      A(TRANSACTION TABLE)                         
         MVI   RNSW,0                                                           
         MVI   RNSW2,0                                                          
         MVI   DTRSW,0             INIT FILE TRANSFER SWITCH                    
*MN SPEC-47579                                                                  
*                                                                               
         USING SSOOFF,RE           GET JOB NAME FOR UNIQUE FILE                 
         ICM   RE,15,MCSSB         IDENTIFIER                                   
         BZ    RNF10                                                            
         SR    R1,R1                                                            
         XC    DOUBLE,DOUBLE                                                    
         ICM   R1,3,SSOJOBNO       JOB NUMBER                                   
         CVD   R1,DOUBLE                                                        
         OI    DOUBLE+L'DOUBLE-1,X'0F'                                          
         UNPK  SVJOBNO,DOUBLE                                                   
         DROP  RE                                                               
RNF10    DS    0H                                                               
*                                                                               
*MN SPEC-47579                                                                  
* DSFTK-150                                                                     
         MVI   ACCSW1,0            RUNNING TRACK OF RUN TYPES/VENDORS           
* DSFTK-150                                                                     
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
RNF30    DS    0H                                                               
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         TM    MCAGCOPT,MCAGCUAT   BEING RUN OFF A UAT FILE?                    
         BZ    *+12                                                             
         OI    RNSW2,RUNUAT        CHECK RUN IS OFF A UAT FILE                  
         OI    RNSW2,RUNTST                                                     
         DROP  RF                                                               
*                                                                               
         MVC   DIR,ACCFIL          ASSUME ACCOUNT FILE                          
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
         ZAP   RECTOT,=P'2'        ACCOUNT FOR UNCOUNTED RECORDS                
         ZAP   RECTOTL,=P'2'       FOR FORMAT CIT300A ONLY                      
*DSFTK-137                                                                      
         ZAP   HDRTOT,=P'0'       HEADER RECORD COUNT                           
*DSFTK-137                                                                      
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
*                                                                               
         BRAS  RE,LOAD             LOAD EXTRA PHASES                            
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
RQF00    L     RF,REMOTEC          SET UP REPORT TITLE                          
         USING REMOTED,RF                                                       
         CLC   REMOTKEY(4),=C'AC55'  TEST OUTPUT IS REMOTE                      
         BNE   *+10                                                             
         MVC   REMOTKEY+4(1),QLEDGER                                            
         DROP  RF                                                               
         XC    MMOSSTR,MMOSSTR                                                  
         XC    MMOSEND,MMOSEND                                                  
         MVC   PRDLST,SPACES       PRODUCT LIST CODE FILTER                     
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         L     R3,ADQSTACK                                                      
         USING ACQD,R3                                                          
         CLI   ACQCONT1,C'C'                                                    
         BNE   RQF60                                                            
         CLI   ACQCONT2,C'C'       LOOK FOR PRODUCT LIST CODE FILTER            
         BNE   RQF60               AND MOS DATE FILTER IN TYPE FIELDS           
*                                                                               
         LHI   R2,8                4 ENTRIES TO CHECK                           
         LA    R4,ACQTYP1          POINT TO 1ST TYPE                            
RQF10    CLI   0(R4),ACQANAL       ANALYSIS FILTER?                             
         BE    RQF50                                                            
         CHI   R2,3                MOS DATE FILTER WILL ONLY BE FOUND           
         BNH   RQF20               IN 1ST TYPE FIELD                            
         CLI   0(R4),ACQDATE       DATE FILTER?                                 
         BNE   RQF20                                                            
         CLI   ACQDTTYP,ACQDTMOS                                                
         BE    RQF30                                                            
RQF20    LA    R4,L'ACQTYP1+L'ACQFLT1(R4)   BUMP TO NEXT ENTRY                  
         BCT   R2,RQF10                                                         
         B     RQF60                                                            
*                                                                               
RQF30    CLC   ACQDTSTR,SPACES                                                  
         BE    RQF40                                                            
         GOTO1 DATCON,DMCB,(0,ACQDTSTR),(1,MMOSSTR)                             
RQF40    MVC   MMOSEND,=X'FFFFFF'                                               
         CLC   ACQDTEND,SPACES                                                  
         BE    RQF20                                                            
         GOTO1 DATCON,DMCB,(0,ACQDTEND),(1,MMOSEND)                             
         B     RQF20                                                            
*                                                                               
RQF50    MVC   PRDLST,1(R4)        MOVE IN PRODUCT LIST CODE FILTER             
         OC    PRDLST,SPACES                                                    
*                                                                               
         USING NAMELD,R4                                                        
RQF60    L     R4,ADCMPNAM         GET COMPANY NAME                             
*                                                                               
         USING CHARECD,R5          CHECK AUTHORIZATION RECORD                   
         L     R5,AIO                                                           
         XC    CHAKEY,CHAKEY                                                    
         MVI   CHAKTYP,CHAKTYPQ    X'10'                                        
         MVC   CHAKCPY,RCCOMPFL    C                                            
         MVC   CHAKUNT(2),QUNIT                                                 
         MVC   KEYSAVE,CHAKEY      SAVE KEY FOR LATER COMPARE                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCFIL,AIO,AIO                               
         B     RQF62AB                                                          
*                                                                               
RQF62AA  GOTO1 DATAMGR,DMCB,DMRSEQ,ACCFIL,AIO,AIO    READ THE NEXT              
RQF62AB  CLI   8(R1),0                                                          
         BNE   RQF62D                                                           
         CLC   KEYSAVE(CHAKLDG-CHAKEY+1),CHAKEY                                 
         BNE   RQF62D                                                           
*                                                                               
         LR    RF,R5                                                            
         AH    RF,DATADISP                                                      
*                                                                               
RQF62A   CLI   0(RF),X'54'                                                      
         BE    RQF62C                                                           
         CLI   0(RF),0                                                          
         BE    RQF62AA                                                          
RQF62B   SR    R0,R0                                                            
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     RQF62A                                                           
*                                                                               
         USING OCNELD,RF                                                        
RQF62C   DS    0H                                                               
         CLC   ORIGINUM,OCNOFFID   MATCH ID                                     
         BNE   RQF62B                                                           
         B     RQF62E                                                           
         DROP  RF                                                               
*                                                                               
RQF62D   DS    0H                                                               
         MVI   FCRDACC,C'N'        UNAUTHORIZED USER (DON'T PROCESS)            
         MVI   FCRDTRNS,C'N'                                                    
         MVI   FCPRCDIR,C'N'                                                    
         TM    RNSW2,RUNTST                                                     
         BO    XIT                                                              
         BRAS  RE,EMLMISS                                                       
         B     XIT                                                              
*                                                                               
RQF62E   DS    0H                                                               
         DROP  R5                                                               
*                                                                               
         USING EDIDD,R5                                                         
         L     R5,AEDIWRK                                                       
*                                                                               
*DSFTK-137                                                                      
*        ZAP   EDIBLCNT,=P'0'                                                   
*                                                                               
*DSFTK-137                                                                      
         MVC   EDICKDLV,QOPT8              SPEC-40627                           
         MVC   EDICNM,SPACES                                                    
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   EDICNM(0),NAMEREC                                                
*                                                                               
         GOTO1 =A(ADDR),DMCB,ADCMPADD      GET THE COMPANY ADDRESS              
         GOTOR FTRADDR,EDICADD1            FORMAT COMPANY ADDRESS               
         MVC   EDIMAIL,BYTE                SAVE OFF MAIL INDICATOR              
         MVC   EDICADDL,WORK         SAVE OFF # OF STREET ADDR LINES            
         DROP  R4,R5                                                            
*                                                                               
RQF65    MVC   SVUNIT,QUNIT                                                     
         MVC   SVLDGR,QLEDGER                                                   
         CLC   QAPPL,SPACES        SEQUENCE FOR STACKED REQUESTS                
         BE    RQF70                                                            
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
RQF70    MVC   RQOFFC,QOFFICE      SET UP THE OFFICE FILTER                     
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
         BE    RQF80                                                            
         GOTO1 DATCON,DMCB,(0,QSTART),(1,CHKDATE)                               
         MVC   MNTH(1),QSTART+1    USE CHECK DATE FOR MOS                       
         MVC   MNTH+1(1),QSTART+3                                               
         CLI   QSTART+2,C'1'                                                    
         BNE   RQF80                                                            
         MVI   MNTH+1,C'A'                                                      
         CLI   QSTART+3,C'0'                                                    
         BE    RQF80                                                            
         MVI   MNTH+1,C'B'                                                      
         CLI   QSTART+3,C'1'                                                    
         BE    RQF80                                                            
         MVI   MNTH+1,C'C'                                                      
*                                                                               
RQF80    CLC   QEND,SPACES         ANY DATE FILTERING                           
         BE    RQF90                                                            
         GOTO1 DATCON,DMCB,(0,QEND),(1,ENDATE)                                  
*                                                                               
RQF90    MVI   RQSW,0                                                           
         CLI   QOPT6,C'P'          PUT TO PQ INSTEAD OF DATA SET/TAPE           
         BNE   *+8                 FOR EDI FLAT FILES?                          
         OI    RQSW,RQPQ                                                        
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
         CLC   PRDLST,SPACES       ANY PRODUCT LIST CODE FILTER?                
         BH    *+10                YES SO DON'T HONOR PRODUCT FILTER            
         MVC   PRDTF,QSELECT+3     PRODUCT FILTER                               
*                                                                               
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
LGF00    CLI   FCRDACC,C'N'        STACKED REQUEST                              
         BE    XIT                                                              
         L     R4,ADLDGNAM         GET LEDGER NAME                              
         USING NAMELD,R4                                                        
         MVC   LDGNME,SPACES                                                    
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,3                                                             
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
         L     RF,AETXT1           US TEXT FIELDS                               
         TM    LGSW,CANAD          IS IT CANADIAN                               
         BNO   *+8                                                              
         L     RF,ACTXT1           CANADIAN TEXT FIELDS                         
         MVC   TXT1(TXTLQ),0(RF)   MOVE TEXT FIELDS                             
         MVI   BYTE,0                                                           
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
LGF05    BRAS  RE,OFNM             CHECK NUMBER FROM OFFICE CHECK               
         BAS   RE,IDNM             GET AGENCY LOGO                              
         CLI   TYPCHK,C' '         TYPE OF CHECK RUN                            
         BNH   LGF09               UNAUTHORIZED USER                            
         CLI   RCPOSTNG,C'N'                                                    
         BE    LGF05A                                                           
         GOTO1 =A(OPNWK),DMCB      OPEN WORKER FILE                             
*                                                                               
LGF05A   SR    RF,RF                                                            
         IC    RF,PROGPROF+3       MINIMUM AMOUNT                               
         CVD   RF,DUB                                                           
         MP    DUB,=P'100'         X 100                                        
         ZAP   MINCHK,DUB          MINIMUM                                      
         CP    MINCHK,=P'0'                                                     
         BH    *+10                                                             
         ZAP   MINCHK,=P'001'      DEFAULT MINIMUM IS $1                        
         ZAP   MAXCHK,=PL6'99999999999'                                         
         ICM   RF,1,PROGPROF+4     MAXIMUM AMOUNT                               
         BZ    LGF06                                                            
         CVD   RF,DUB                                                           
         MP    DUB,=P'1000000'     PROFILE VALUE X $10,000.00                   
         ZAP   MAXCHK,DUB                                                       
*                                                                               
LGF06    L     RF,REMOTEC          SET UP REPORT TITLE                          
         USING REMOTED,RF                                                       
         TM    DTRSW,DTR820        EDI820 DATA TRANSFER?                        
         BZ    *+8                                                              
         MVI   REMOTCLS,C'G'       MUST BE CLASS G                              
         TM    RNSW,SOON           ONLY FOR SOON CHECKS                         
         BZ    LGFX                                                             
                                                                                
         TM    PRNTSW,WEBWSP                                                    
         BZ    LGF07                                                            
         MVC   REMOTKEY(2),LASRCODE                                             
         MVC   REMOTKEY+2(1),QLEDGER                                            
         B     LGF08                                                            
                                                                                
LGF07    MVC   REMOTFNO(2),POWCODE                                              
         MVC   REMOTFNO+2(1),QLEDGER                                            
         MVI   REMOTFNO+3,C'$'                                                  
         DROP  RF                                                               
*                                                                               
LGF08    L     R4,ADLEDGER         GET LEDGER RECORD                            
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
PAC00    MVI   FCRDTRNS,C'N'       DEFAULT IS NO CHECK                          
         MVI   FCPRCDIR,C'N'                                                    
         ZAP   CHTOT,=P'0'         CLEAR ACCOUNT ACCUMULATORS                   
         ZAP   CHCSD,=P'0'                                                      
         ZAP   ITEMS,=P'0'         COUNT TRANS PER CHECK SPEC-40627             
                                                                                
         BRAS  RE,RDSC             GET BANK INFO FROM SC ACCOUNT                
         BRAS  RE,RDBNKC           GET BANK INFO FROM CFILE BANK REC            
* DSFTK-150                                                                     
         USING EDIDD,R1                                                         
         L     R1,AEDIWRK                                                       
         CLC   EDIEFADV(5),=CL5'PCARD'  FROM BANK RECORD                        
         BNE   *+8                                                              
         OI    ACCSW1,PCRDINCL     SET PCARD SETUP BEING PROCESSED              
         CLC   EDIEFADV(5),=CL5'CCARD'  FROM BANK RECORD                        
         BNE   *+8                                                              
         OI    ACCSW1,CCRDINCL     SET CCARD SETUP BEING PROCESSED              
         DROP  R1                                                               
* DSFTK-150                                                                     
                                                                                
         ICM   R4,15,ADACCNAM      MUST HAVE PAYEE NAME                         
         BZ    XIT                                                              
         USING NAMELD,R4                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,3                                                             
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
         CLI   PROGPROF+2,C'Y'                                                  
         BE    PAC05               SKIP TEST BELOW                              
         CP    ABLCR,ABLDR         CREDITS MUST BE GREATER THAN                 
         BNH   XIT                 DEBITS IF WE ARE TO WRITE A CHECK            
         DROP  R2                                                               
*                                                                               
         USING ACTRECD,R2                                                       
PAC05    L     R2,ADACC                                                         
         MVI   VTYPE,VREG          DEFAULT TO REGULAR VENDOR                    
*                                                                               
         TM    RNSW2,RUNUAT        UAT FILE IS PRINTED CHECKS ONLY              
         BO    PAC50                                                            
*                                                                               
         LA    R1,ACTKEY+ACCORFST                                               
         SR    R0,R0                                                            
*                                                                               
         USING VBPFELD,R1                                                       
PAC10    CLI   0(R1),0                                                          
         BE    PAC30                                                            
         CLI   0(R1),VBPELQ        80 ELEMENT                                   
         BE    PAC14                                                            
         IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     PAC10                                                            
*                                                                               
PAC14    DS    0H                                                               
         TM    VBPPAYM,VBPCCRD         IS THIS A CCARD VENDOR                   
         BZ    PAC15                                                            
         TM    ACCSW1,EDIINCL          IS CHECK ID SETUP FOR EDI820             
         BZ    PAC14C                                                           
         OI    MIXERRM,MIXMIXC         CANNOT BE BOTH - ERROR VENDOR            
         NI    DTRSW,X'FF'-DTREFT      TURN OFF EFT DATA SWITCH                 
         B     PAC19                                                            
*                                                                               
PAC14C   TM    ACCSW1,CCRDINCL         IF I HAVE A CCARD VENDOR ON A            
         BZ    PAC14H                  NON-CCARD SETUP THIS IS ERROR            
                                                                                
PAC14E   TM    ACCSW1,EFTINCL                                                   
         BZ    PAC14G                                                           
         OI    MIXERRM,MIXMIXE         CANNOT BE BOTH - ERROR VENDOR            
         NI    DTRSW,X'FF'-DTREFT      TURN OFF EFT DATA SWITCH                 
         B     PAC19                                                            
*                                                                               
PAC14G   TM    ACCSW,EFTON                                                      
         BO    PAC14K                                                           
PAC14H   OI    MIXERRM,MIXMISC     MISSING CCARD SETUP                          
         NI    DTRSW,X'FF'-DTREFT      TURN OFF EFT DATA SWITCH                 
         B     PAC19                                                            
*                                                                               
PAC14K   OI    DTRSW,DTREFT        SET EFT SWITCH                               
         MVI   VTYPE,VCCARD        FLAG THIS VENDOR AS CCARD                    
         B     PAC50                                                            
*                                                                               
PAC15    DS    0H                                                               
* DSFTK-150                                                                     
         TM    VBPPAYM,VBPPCRD         IS THIS A PCARD VENDOR                   
         BZ    PAC17                                                            
         TM    ACCSW1,EDIINCL          IS CHECK ID SETUP FOR EDI820             
         BZ    PAC15C                                                           
         OI    MIXERRM,MIXMIXP         CANNOT BE BOTH - ERROR VENDOR            
         NI    DTRSW,X'FF'-DTREFT      TURN OFF EFT DATA SWITCH                 
         B     PAC19                                                            
*                                                                               
PAC15C   TM    ACCSW1,PCRDINCL         IF I HAVE A PCARD VENDOR ON A            
         BZ    PAC15H                  NON-PCARD SETUP THIS IS ERROR            
                                                                                
PAC15E   TM    ACCSW1,EFTINCL                                                   
         BZ    PAC15G                                                           
         OI    MIXERRM,MIXMIXE         CANNOT BE BOTH - ERROR VENDOR            
         NI    DTRSW,X'FF'-DTREFT      TURN OFF EFT DATA SWITCH                 
         B     PAC19                                                            
*                                                                               
PAC15G   TM    ACCSW,EFTON                                                      
         BO    PAC15K                                                           
PAC15H   OI    MIXERRM,MIXMISP     MISSING PCARD SETUP                          
         NI    DTRSW,X'FF'-DTREFT      TURN OFF EFT DATA SWITCH                 
         B     PAC19                                                            
*                                                                               
PAC15K   OI    DTRSW,DTREFT        SET NON-EFT/NON PCARD SWITCH                 
         MVI   VTYPE,VPCARD        FLAG THIS VENDOR AS PCARD                    
         MVC   HALF2(1),VBPPCTY    SAVE PCARD SWIPE TYPE                        
         B     PAC50                                                            
* DSFTK-150                                                                     
*                                                                               
PAC17    TM    VBPPAYM,VBPEFT      EFT VENDOR?                                  
         BO    *+12                                                             
         OI    DTRSW,DTRNEFT       SET NON-EFT/NON PCARD SWITCH                 
         B     PAC50                                                            
*                                                                               
         TM    ACCSW,EFTON         IS EFT BANK SETUP FOUND?                     
         BO    *+16                                                             
         NI    DTRSW,X'FF'-DTREFT      TURN OFF EFT DATA SWITCH                 
         OI    MIXERRM,MIXMISS     MISSING EFT SETUP FOR EFT VENDOR             
         B     PAC19                                                            
*                                                                               
         TM    ACCSW1,PCRDINCL     CHECK FOR PREEXISTING PCARD VENDOR           
         BZ    *+16                                                             
         NI    DTRSW,X'FF'-DTREFT      TURN OFF EFT DATA SWITCH                 
         OI    MIXERRM,MIXMIXE     ERROR MIX OF PCARD AND EFT VENDORS           
         B     PAC19                                                            
*                                                                               
         TM    ACCSW1,CCRDINCL     CHECK FOR PREEXISTING PCARD VENDOR           
         BZ    *+16                                                             
         NI    DTRSW,X'FF'-DTREFT      TURN OFF EFT DATA SWITCH                 
         OI    MIXERRM,MIXMIXE     ERROR MIX OF PCARD AND EFT VENDORS           
         B     PAC19                                                            
*                                                                               
         TM    DTRSW,DTR820        CHECK FOR PREEXISTING 820 SETUP              
         BZ    *+16                                                             
         NI    DTRSW,X'FF'-DTREFT      TURN OFF EFT DATA SWITCH                 
         OI    MIXERRM,MIXMIX       SET MIX OF EFT/820 ERROR MSG                
         B     PAC19                                                            
*                                                                               
         MVI   VTYPE,VEFT          FLAG THIS VENDOR AS EFT                      
         OI    DTRSW,DTREFT        SET EFT DATA SWITCH                          
         OI    ACCSW1,EFTINCL      SET EFT VENDORS BEING PROCESSED              
         B     PAC50                                                            
*                                  GO TO PROCESS THIS VENDOR                    
PAC19    L     RF,ADACC                                                         
         LA    R1,MIXERRT                                                       
PAC20    OC    0(12,R1),0(R1)                                                   
         BZ    PAC25                                                            
         CLC   0(12,R1),3(RF)       ELIMINATE DUPLICATES FROM TABLE             
         BE    XIT                  AND THE RUN                                 
         LA    R1,12(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         BNE   PAC20                                                            
         BE    XIT                  TABLE IS FULL - EXIT                        
PAC25    MVC   0(12,R1),3(RF)                                                   
         B     XIT                                                              
*                                                                               
PAC30    OI    DTRSW,DTRNEFT       NO 80 ELEM MEANS NOT EFT                     
*                                                                               
PAC50    XC    NUMTRN,NUMTRN       CLEAR TRANSACTION COUNT                      
*                                                                               
PACOKX   MVI   FCRDTRNS,C'Y'       OK TO PROCESS A CHECK                        
         MVI   FCPRCDIR,C'Y'       GET DIRECTORY MODE                           
PACXIT   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS A TRANSACTION DIRECTORY                                     *         
***********************************************************************         
         USING TRNRECD,R3                                                       
PTD00    L     R3,4(R1)                                                         
         CLI   TRNKSTYP,X'81'      TEST CHECK ITEMS                             
         BE    PTDNO                                                            
         TM    TRNKSTAT,X'20'      TEST REVERSED                                
         BO    PTDNO                                                            
*                                                                               
         MVC   CLNT,TRNKCULC+12    GET THE CLIENT CODE                          
         MVC   PRDT,TRNKREF        PRODUCT CODE                                 
                                                                                
         CLI   TRNKSTYP,45         CL#0381591N - CHECKS BY CLIENT IS            
         BE    PTD04               SKIPPING TYPE 45'S SJ(CLI) CONTRA            
         CLI   TRNKSTYP,26                                                      
         BNE   PTD05                                                            
                                                                                
PTD04    CLC   TRNKCUNT(2),=C'SJ'  MEDIA TY 26 FROM SJ CONTRA                   
         BE    PTD09                                                            
PTD05    TM    SYEQU,PRDQ+EXPQ     IS IT PRODUCTION OR EXPENSE                  
         BZ    *+16                                                             
PTD09    MVC   CLNT,TRNKCULC+3     CLIENT IS IN CONTRA                          
         MVC   PRDT,SPACES         DON'T HAVE PRODUCT - YET                     
                                                                                
         CLI   TRNKSTYP,X'09'      TEST CHECK ITEMS                             
         BNE   *+10                                                             
         MVC   PRDT,SPACES                                                      
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
PTN11    EQU   *                                                                
         GOTO1 =A(GETCD),DMCB      THIS WILL GET ITEM CD                        
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
*                                                                               
         XC    LOCMMOS,LOCMMOS                                                  
         XC    WORK,WORK                                                        
         MVI   ELCODE,GDAELQ        GET MEDIA MOS ELEMENT                       
         L     R4,ADTRANS                                                       
         BAS   RE,FIRSTEL                                                       
         BNE   PTN27E1                                                          
         USING GDAELD,R4                                                        
PTN27D   CLI   GDATYPE,GDAMMOS                                                  
         BE    PTN27E                                                           
         BAS   RE,NEXTEL                                                        
         BNE   PTN27E1                                                          
         B     PTN27D                                                           
PTN27E   MVC   LOCMMOS,GDADATE      SAVE DATE TO ADD TO DEBIT POSTING           
         MVC   WORK(L'GDADATE),GDADATE                                          
         B     PTN27F                                                           
*                                                                               
PTN27E1  MVI   ELCODE,MBIELQ                                                    
         L     R4,ADTRANS                                                       
         BAS   RE,FIRSTEL                                                       
         BNE   PTN27F                                                           
         USING MBIELD,R4                                                        
         MVC   WORK(L'MBIMOS),MBIMOS                                            
                                                                                
PTN27F   OC    MMOSEND,MMOSEND      IF NOTHING IN MMOSEND,NO FILTERING          
         BZ    PTN27H               REQUESTED                                   
         OC    WORK,WORK            AM FILTERING, BUT NO DATE FOUND             
         BZ    XIT                                                              
PTN27G   CLC   MMOSSTR(2),WORK      IF WITHIN RANGE KEEP IT,IF NOT XIT          
         BH    XIT                                                              
         CLC   MMOSEND(2),WORK                                                  
         BL    XIT                                                              
PTN27H   EQU   *                                                                
         XC    SVEST,SVEST                                                      
         MVI   ELCODE,X'46'                                                     
         L     R4,ADTRANS                                                       
         BAS   RE,FIRSTEL                                                       
         BNE   PTN28               SKIP CHECK IF NO X'46'                       
         USING XPYELD,R4                                                        
         CLC   XPYEST,SPACES       ANY ESTIMATE?                                
         BE    PTN27I              NO                                           
         OC    XPYEST,XPYEST                                                    
         BZ    PTN27I                                                           
         LH    RE,XPYEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SVEST(3),DUB           UNPACK THE ESTIMATE                       
PTN27I   CLC   QBILGRUP,SPACES     FILTERING ON ESTIMATE NUMBER                 
         BE    PTN28               NO                                           
         PACK  DUB,QBILGRUP        ESTIMATE NUMBER                              
         CVB   R1,DUB                                                           
         STH   R1,HALF             STORE BINARY IN ESTNUM                       
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
* DC                                                                            
PTN31    CLC   PRDLST,SPACES       FILTERING ON A LIST OF PRODUCTS?             
         BE    PTN80               NO                                           
*                                  FIRST READ THE LIST RECORD                   
         GOTO1 ACLIST,DMCB,(C'R',AIO),(RCCOMPFL,PRDLST),DATAMGR                 
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                  NOW PROCESS THE LIST RECORD                  
         MVC   WORK,SPACES         BUILD SJ ACCT INTO WORK TO PASS              
         MVC   WORK(2),=C'SJ'      TO ACLIST                                    
         MVC   WORK+2(L'CLPR),CLPR                                              
         GOTO1 ACLIST,DMCB,(C'S',AIO),(0,WORK),0                                
         CLI   DMCB,X'00'                                                       
         BNE   *+6                                                              
         DC    H'0'                BAD LIST RECORD                              
         CLI   DMCB,C'E'                                                        
         BE    FLTRNO              EXCLUDE THIS CLIENT                          
* DC                                                                            
PTN80    ICM   R5,15,ANXTRN        NEXT TABLE ENTRY                             
         BNZ   *+8                                                              
         L     R5,APTNTAB          FIRST TIME, START TABLE                      
         USING PTND,R5                                                          
         XC    PTNKEY(PTNLQ),PTNKEY                                             
         CLI   PROGPROF+8,C'Y'     CHECKS BY CLIENT                             
         BNE   PTN90                                                            
         TM    SYEQU,PRDQ+MEDQ+EXPQ       PRODUCTION, MEDIA OR EXPENSE          
         BZ    PTN90                                                            
         MVC   PTNCLI,CLNT         CLIENT                                       
*                                                                               
PTN90    MVC   PTNCKEY(PTNCKLQ),TRNKOFF   OFFICE/CONTRA/DATE/REF/SBR            
         ZAP   PTNAMT,TRNAMNT      AMOUNT                                       
         ZAP   PTNCD,CD            C.D.                                         
         ZAP   PTNTAMT,=P'0'                                                    
         ZAP   PTNTCD,=P'0'                                                     
         MVC   PTNCNM,SPACES       CONTRA ACCOUNT NAME                          
         MVI   PTNCLN,0            SET CONTRA NAMELENGTH TO ZERO                
         MVC   PTNMMOS,LOCMMOS                                                  
         MVC   PTNEST,SVEST                                                     
         CLI   TRNTYPE,37          TYPE 37                                      
         BE    PTN92                                                            
         CLI   TRNTYPE,45          AND 45 CAN'T HELP DECIDE                     
*SPEC-9755, SPEC-13241                                                          
*        BE    PTN92               WHICH REMITTANCE TO USE                      
         BNE   PTN91               WHICH REMITTANCE TO USE                      
         TM    DTRSW,DTREFT        IF NOT EFT RUN SKIP                          
         BZ    *+10                                                             
*SPEC-9755, SPEC-13241                                                          
PTN91    MVC   PTNTYP,TRNTYPE      SAVE TYPE TO HELP DETERMINE                  
PTN92    L     R4,ADSUBAC                                                       
         CLI   0(R4),X'43'                                                      
         BNE   PTN100                                                           
         USING CACELD,R4                                                        
         SR    R1,R1                                                            
         IC    R1,CACLN                                                         
         SHI   R1,18                                                            
         BM    PTN100                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PTNCNM(0),CACNAME                                                
         AHI   R1,1                LENGTH OF NAME                               
         STC   R1,PTNCLN                                                        
         DROP  R3,R4                                                            
*                                                                               
PTN100   LA    R5,PTNLQ(R5)                                                     
         ST    R5,ANXTRN                                                        
         XC    PTNREC,PTNREC       CLEAR NEXT ENTRY                             
         LH    R0,NUMTRN           UPDATE TRANSACTION COUNT                     
         AHI   R0,1                                                             
         STH   R0,NUMTRN                                                        
         AP    CHTOT,TRNAMNT       ADD AMOUNT TO CHECK TOTAL                    
         AP    ITEMS,=P'1'         KEEP ITEM COUNT  SPEC-40627                  
         AP    CHCSD,CD            ADD C.D. TO CHECK C.D.                       
         B     XIT                 WHICH REMITTANCE FORM TO USE                 
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ACCOUNT LAST                                                        *         
***********************************************************************         
ACL00    OC    NUMTRN,NUMTRN                                                    
         BZ    XIT                                                              
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
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PAYNME(0),NAMEREC                                                
         AHI   R1,1                GET LENGTH OF NAME                           
         STC   R1,SRANML                                                        
         MVC   SRANME,PAYNME       ACCOUNT NAME TO SORT RECORD                  
         GOTO1 =A(ADDR),DMCB,ADACCADD      ADD THE ADDRESS                      
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
         USING PTND,R5                                                          
         L     R5,APTNTAB          R5 = TRANSACTION TABLE                       
ACL05    CLI   0(RE),0             LAST ENTRY ?                                 
         BE    ACL06               TAKE THE DEFAULT                             
         CLC   PTNTYP,0(RE)        MATCH TYPE                                   
         BE    ACL06                                                            
         LA    RE,L'TYENT(RE)                                                   
         B     ACL05                                                            
*                                                                               
ACL06    MVC   TYENT,0(RE)                                                      
         MVC   CHKTYPE,PTNTYP                                                   
         MVC   SRTYPE,CHKTYPE      ACCOUNT TYPE                                 
         MVC   SRSYENT,SYENT       SYSTEM ENTRY                                 
         MVC   SRTYENT,TYENT       TYPE ENTRY                                   
         SR    R3,R3               CHECK SEQUENCE NUMBER                        
*                                                                               
ACL07    LA    R0,1                ITEM COUNT                                   
         AHI   R3,1                INCREMENT SEQUENCE                           
         LR    R1,R5               SAVE START OF SEARCH                         
         ZAP   CHTOT,PTNAMT        GET CHECK TOTAL                              
         ZAP   CHCSD,PTNCD                                                      
*                                                                               
ACL09    LA    RF,PTNLQ(R5)        LOOK TO NEXT                                 
         OC    0(PTNLQ,RF),0(RF)   END OF TABLE                                 
         BZ    ACL11                                                            
         CLC   PTNCLI,PTNCLI-PTNREC(RF) SAME CLIENT                             
         BNE   ACL11                                                            
         CHI   R0,800              MAX ITEMS ON CHECK                           
         BNL   ACL11                                                            
         LR    R5,RF                                                            
         AHI   R0,1                ITEM COUNT                                   
         AP    CHTOT,PTNAMT        ADD TO TOTAL                                 
         AP    CHCSD,PTNCD                                                      
         B     ACL09                                                            
*                                                                               
ACL11    OC    PTNCLI,PTNCLI       NO CHECKS FOR THIS RUN IF                    
         BNZ   ACL12               - ANY CHECKS ARE < 0                         
         CP    CHTOT,=P'0'         TEST CHECK TOTAL                             
         BL    ACL110                                                           
*                                                                               
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
         BZ    ACL110              END OF TABLE                                 
         ZAP   CHTOT,PTNTAMT                                                    
         ZAP   CHCSD,PTNTCD                                                     
         CP    CHTOT,MINCHK        TEST AMOUNT LOWER THAN MINIMUM               
         BL    ACL105              TEST FOR MAXIMUM AMOUNT                      
         CP    CHTOT,MAXCHK        TEST IF HIGHER THAN MAXIMUM AMT              
         BH    ACL105              SKIP IF OVER MAXIMUM                         
         TM    DTRSW,DTR820        IF EDI820 CHK RUN OR VENDOR IS               
         BO    *+12                EFT AND BANK INFO NOT FOUND ON SC            
         CLI   VTYPE,VEFT          THEN DON'T MARK USED OR ADD SORT REC         
         BNE   ACL20                                                            
* DSFTK-150                                                                     
* I THINK NOT HAVING A PCARD CHECK HERE WILL BE OKAY - A MISSING BANK           
* SETUP WOULD HAVE BEEN DETECTED AT ACC FIRST                                   
* DSFTK-150                                                                     
         TM    DTXSW,EDIFAIL                                                    
         BO    ACL105                                                           
*                                                                               
ACL20    OI    RQSW,RQACT          TURN ON ACTIVITY                             
         L     R3,ATRNIO                                                        
         USING TRNRECD,R3                                                       
         L     RF,ADACC                                                         
         MVC   TRNKCULA,0(RF)      ACCOUNT                                      
         MVC   TRNKOFF(PTNCKLQ),PTNCKEY TRANACTION KEY                          
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,ATRNIO,ATRNIO                         
         CLI   8(R1),0             READ DIRECTORY TO GET DISK ADDRESS           
         BE    *+6                 FOR PASSIVE READ                             
         DC    H'0'                                                             
         MVC   FULL,TRNKDA         SAVE D/A FOR PASSIVE READ                    
*                                                                               
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,ATRNIO,ATRNIO                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T READ TRANSACTION                       
         MVC   TRNKEY+ACCOUSED(2),TODAY2 MARK IT USED                           
         LA    R1,TRNKEY+ACCORFST                                               
         SR    R0,R0                                                            
*                                                                               
ACL21    CLI   0(R1),0                                                          
         BE    ACL35                                                            
         CLI   0(R1),TRNELQ        44 ELEMENT                                   
         BE    ACL25                                                            
         CLI   0(R1),TRSELQ        60 ELEMENT                                   
         BE    ACL30                                                            
ACL21NX  IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     ACL21                                                            
*                                                                               
* 44 ELEMENT                                                                    
         USING TRNELD,R1                                                        
ACL25    MVC   SVOFF,TRNOFFC                                                    
         B     ACL21NX                                                          
         DROP  R1                                                               
*                                                                               
* 60 ELEMENT                                                                    
         USING TRSELD,R1                                                        
ACL30    OI    TRSSTAT,TRSSACHQ    SET CHECK WRITTEN BY AC55                    
         NI    TRSSTAT,X'FF'-TRSSVOID NOT VOID                                  
         XC    TRSVOID,TRSVOID                                                  
         ZAP   TRSUREQ,RCRQTOT     REQUEST NUMBER                               
         CLI   TYPCHK,TYPSOON                                                   
         BNE   *+10                                                             
         AP    TRSUREQ,=P'100'     ADD 100 TO DISTINQUISH SOON                  
         MVC   TRSUSER,ORIGINUM    ORIGIN NUMBER                                
         DROP  R1                                                               
*                                                                               
ACL35    TM    RNSW2,WRTNO         NOT WRITING TO FILE                          
         BO    ACL40                                                            
         GOTO1 DATAMGR,DMCB,DMWRT,ACCFIL,(R3),(R3)                              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         USING AAVPASD,R2          AUTO APPROVE VENDOR PASSIVE PTR              
ACL40    L     R2,AIO                                                           
         MVC   AAVPKEY,SPACES                                                   
         MVI   AAVPTYP,AAVPTYPQ    X'24'                                        
         MVI   AAVPSUB,AAVPSUBQ    X'01'                                        
         MVC   AAVPCPY,TRNKCPY                                                  
         MVC   AAVPCLT,TRNKCACT+9  LAST 3 CHAR OF CONTRA IS CLIENT              
         MVC   AAVPPRD,TRNKREF     1ST 3 CHAR OF REFERENCE IS PRODUCT           
         MVC   AAVPEST,PTNEST      ESTIMATE                                     
         OC    AAVPEST,SPACES                                                   
         MVC   AAVPMOS,PTNMMOS     MOS                                          
         LA    RF,LDSYTAB          TABLE OF LEDGER/SYSTEM                       
ACL50    CLI   0(RF),X'FF'         IF LEDGER NOT IN TABLE THAN NO               
         BE    ACL100              PASSIVES FOR THAT LEDGER.                    
         CLC   TRNKLDG,0(RF)                                                    
         BE    ACL60                                                            
         LA    RF,2(RF)                                                         
         B     ACL50                                                            
*                                                                               
ACL60    MVC   AAVPSYS,1(RF)       SYSTEM                                       
         CLI   AAVPSYS,C'S'        IF IT'S SPOT CHECK TO SEE IF IT'S            
         BNE   *+16                REALLY NET BY CHECKING THE 1ST CHAR          
         CLI   TRNKACT,C'N'        OF THE ACCOUNT FOR 'N' (SSN)                 
         BNE   *+8                                                              
         MVI   AAVPSYS,C'N'                                                     
         MVC   AAVPOFF,SVOFF       OFFICE                                       
         MVC   AAVPACCT,TRNKLDG    ACCOUNT                                      
         MVC   AAVPKDA,FULL        DISK ADDRESS                                 
         MVI   AAVPFLG1,0          WANT ALL TYPES OF ESTIMATES                  
         MVC   TEMPKEY(AAVPFLG1-AAVPKEY),0(R2)                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,AIO,AIO                               
         CLC   0(AAVPFLG1-AAVPKEY,R2),TEMPKEY                                   
         BNE   ACL100                                                           
         MVC   AAVPUSED,TODAY2     MARK THE PASSIVE W/ USED DATE                
*                                                                               
         TM    RNSW2,WRTNO         NOT WRITING TO FILE                          
         BO    ACL100                                                           
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,(R2),(R2)                              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2,R3                                                            
*                                                                               
ACL100   BAS   RE,BSR              BUILD THE SORT RECORD                        
ACL105   LA    R5,PTNLQ(R5)        R5 = NEXT ITEM                               
         B     ACL19                                                            
*                                                                               
ACL110   B     XIT                                                              
         DROP  R5                                                               
*                                  TABLE OF LEDGER AND SYSTEM                   
LDSYTAB  DC    C'SS'               LEDGER S - SYS SPOT (BUT MAYBE NET)          
         DC    C'TS'               LEDGER T - SYS SPOT                          
         DC    C'PP'               LEDGER P - SYS PRINT                         
         DC    C'QP'               LEDGER Q - SYS PRINT                         
         DC    C'UN'               LEDGER U - SYS NET                           
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* REQUEST LAST                                                        *         
***********************************************************************         
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
         AHI   R1,1                                                             
         STC   R1,0(RF)                                                         
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCFIL,STKKEY,AIO                            
         L     R3,AIO                                                           
         CLC   0(SRMKACT-SRMKEY,R3),STKKEY TEST SAME STACK                      
         BNE   XIT                 ALL DONE                                     
         MVC   STKKEY,0(R3)        SAVE NEW KEY                                 
         MVC   QRECORD,SPACES                                                   
         MVC   QRECORD2,SPACES                                                  
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         L     RE,ADQSTACK                                                      
         USING ACQD,RE                                                          
         ST    RE,FULL                                                          
         LR    R1,RE                                                            
         LA    R0,4                                                             
         MVC   0(L'ACQCARD1,R1),SPACES                                          
         LA    R1,L'ACQCARD1(R1)                                                
         BCT   R0,*-10                                                          
         DROP  RF                                                               
*                                                                               
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
         L     RE,FULL                                                          
         LA    RF,QRECORD          SET REQUEST DETAILS                          
         CLI   RQCSEQ,1                                                         
         BNE   *+16                                                             
         MVC   0(L'RQCCARD,RF),RQCCARD                                          
         MVC   0(L'RQCCARD,RE),RQCCARD                                          
         LA    RF,QRECORD2                                                      
         AHI   RE,L'ACQCARD1                                                    
         CLI   RQCSEQ,2                                                         
         BNE   *+16                                                             
         MVC   0(L'RQCCARD,RF),RQCCARD                                          
         MVC   0(L'RQCCARD,RE),RQCCARD                                          
         AHI   RE,L'ACQCARD1                                                    
         CLI   RQCSEQ,3                                                         
         BNE   *+10                                                             
         MVC   0(L'RQCCARD,RE),RQCCARD                                          
         AHI   RE,L'ACQCARD1                                                    
         CLI   RQCSEQ,4                                                         
         BNE   *+10                                                             
         MVC   0(L'RQCCARD,RE),RQCCARD                                          
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
RNL00    LA    R0,NACTOT           CLEAR ACCOUNT ACCUMULATORS                   
         LA    R1,ACTOTS                                                        
         ZAP   0(L'ACTOTS,R1),=P'0'                                             
         LA    R1,L'ACTOTS(R1)                                                  
         BCT   R0,*-10                                                          
         TM    LGSW,UNAUTH         TEST UNAUTHORIZED STATUS                     
         BO    GETSX               END SORT                                     
*                                                                               
         L     R1,=A(MIXERRT)                                                   
         OC    0(12,R1),0(R1)                                                   
         BZ    RNL01                                                            
*                                                                               
         TM    MIXERRM,MIXMISS     MISSING EFT BANK SETUP                       
         BZ    *+8                                                              
         BRAS  RE,EMLMEFT                                                       
*                                                                               
         TM    MIXERRM,MIXMISP     MISSING PCARD BANK SETUP                     
         BZ    *+8                                                              
         BRAS  RE,EMLMEFT                                                       
*                                                                               
         TM    MIXERRM,MIXMISC     MISSING CCARD BANK SETUP                     
         BZ    *+8                                                              
         BRAS  RE,EMLMEFT                                                       
*                                                                               
         TM    MIXERRM,MIXMIX      MIX OF EFT AND EDI820                        
         BZ    *+8                                                              
         BRAS  RE,EMLMIX                                                        
*                                                                               
         TM    MIXERRM,MIXMIXP     MIX OF PCARD AND EDI820                      
         BZ    *+8                                                              
         BRAS  RE,EMLMIX                                                        
*                                                                               
         TM    MIXERRM,MIXMIXC     MIX OF CCARD AND EDI820                      
         BZ    *+8                                                              
         BRAS  RE,EMLMIX                                                        
*                                                                               
         TM    MIXERRM,MIXMIXE     MIX OF PCARD AND EFT                         
         BZ    *+8                                                              
         BRAS  RE,EMLMIX                                                        
*                                                                               
RNL01    GOTO1 APOST               OPEN THE WORKER FILE                         
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
*                                                                               
         BRAS  RE,RUNTOTS                                                       
         CP    NOADD,=P'0'         CHECKS WITHOUT AN ADDRESS                    
         BE    RNL05                                                            
         BAS   RE,PRTLNE                                                        
*                                                                               
         MVC   P+TOTCHAT(L'TXT6),TXT6 NO ADDRESS-CHECKS                         
         EDIT  NOADD,(3,P+45)                                                   
         BAS   RE,PRTLNE                                                        
*                                                                               
RNL05    OC    REQTAB,REQTAB       ANY NO-CHECK REQUESTS                        
         BZ    RNL12                                                            
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
RNL12    DS    0H                                                               
         BAS   RE,VDCK             VOID AND POST THE LAST CHECK                 
RNL13    CLI   TYPCHK,TYPSOON      DDS AND LOCAL CHECKS                         
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
         L     RF,ADLEDGER                                                      
*                                                                               
         USING CHARECD,R4          CHECK AUTHORIZATION RECORD                   
         L     R4,AIO                                                           
         XC    CHAKEY,CHAKEY                                                    
         MVI   CHAKTYP,CHAKTYPQ    X'10'                                        
         MVC   CHAKCULA,0(RF)      C/U/L                                        
         MVC   KEYSAVE,CHAKEY      SAVE KEY FOR LATER COMPARE                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCFIL,AIO,AIO                               
         B     RNL19AB                                                          
*                                                                               
RNL19AA  GOTO1 DATAMGR,DMCB,DMRSEQ,ACCFIL,AIO,AIO    READ THE NEXT              
RNL19AB  CLI   8(R1),0                                                          
         BNE   RNL26                                                            
         CLC   KEYSAVE(CHAKLDG-CHAKEY+1),CHAKEY                                 
         BNE   RNL26                                                            
*                                                                               
         LR    RF,R4                                                            
RNL19    AH    RF,DATADISP                                                      
*                                                                               
RNL20    CLI   0(RF),X'54'                                                      
         BE    RNL22                                                            
         CLI   0(RF),0                                                          
         BE    RNL19AA                                                          
RNL21    SR    R0,R0                                                            
         IC    R0,1(RF)            AND FIND OFFICE CHECK ELEMENT                
         AR    RF,R0                                                            
         B     RNL20                                                            
*                                                                               
         USING OCNELD,RF                                                        
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
         DROP  R4,RF                                                            
*                                                                               
RNL26    DS    0H                                                               
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
         TM    DTRSW,DTRANY        FOR EDI820 OR FLATFILE                       
         BO    RNL30               DON'T CHECK FOR EFT                          
         TM    DTRSW,DTRNEFT       IF WHOLE CHECK RUN IS EFT DON'T              
         BO    RNL30               DO THE VOIDS                                 
         L     RF,REMOTEC          SET UP REPORT TITLE                          
         USING REMOTED,RF                                                       
         MVI   REMOTCLS,C'G'       MUST BE CLASS G                              
         B     RNL31                                                            
         DROP  RF                                                               
*                                                                               
RNL30    BAS   RE,VDCK                                                          
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
         BNO   RUN43                                                            
         CP    RUNTOT,=P'0'        TEST ZERO RUN                                
         BNE   RUN43                                                            
         MVC   P,SPACES                                                         
         LA    RE,DICO           NEED ADDRESSABILITY TO THE DDICT ENTRY         
         AHI   RE,AC@ZRUN-DICO                                                  
         MVC   P+1(L'AC@ZRUN),0(RE)      ZERO RUN - NO CHECKS TO WRITE          
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
*                                                                               
RUN43    TM    DTRSW,DTRANY+DTREFT DATA TRANSFER?(EDI820, CHECKS DATA           
         BZ    RUN43A              FILE OR EFT)                                 
         L     RF,AEDTRN                                                        
         BASR  RE,RF                                                            
*                                                                               
RUN43A   TM    RNSW,MICR+SOON      TEST MICR/SOON                               
         BNO   RUN44                                                            
*DSFTK-195                                                                      
         TM    DTRSW,DTR820        MICR IS ON FOR EDI820                        
         BZ    RUN43C                                                           
         L     RF,REMOTEC          SET UP REPORT TITLE                          
         USING REMOTED,RF                                                       
         MVI   REMOTCLS,C'G'       MUST BE CLASS G                              
         MVC   REMOTPRG,=C'55'         **WHAT DO WE CALL IT?                    
         MVC   REMOTJID(3),=C'A55'                                              
         MVC   P+5(36),=CL20'EDI820 TRANSFER : FILE TRANSFER SENT'              
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
RUN43C   DS    0H                                                               
*DSFTK-195                                                                      
         GOTO1 ARUNREG             RUN THE REGISTER                             
*                                                                               
RUN44    TM    RNSW,SOON           TEST SOON                                    
         BNO   *+8                                                              
         BRAS  RE,FACOUT           WRITE FACWK FILES                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD AND PUT THE SORT RECORD                                       *         
***********************************************************************         
         USING PTND,R5                                                          
BSR      NTR1  ,                                                                
         XC    SRVAR,SRVAR                                                      
         ZAP   SRREQ,RCRQTOT       REQUEST NUMBER                               
         MVC   SRVTYP,VTYPE        VENDOR TYPE (EFT OR NON-EFT)                 
         MVC   SRMNTH,MNTH         MONTH OF SERVICE                             
         MVC   SRCDTE,CHKDATE      CHECK DATE                                   
         MVC   SRROFC,RQOFFC       OFFICE REQUEST                               
* DSFTK-150                                                                     
         MVC   SRTSWIPE,HALF2      PCARD SWIPE TYPE                             
* DSFTK-150                                                                     
         USING ACTRECD,R2                                                       
         L     R2,ADACC                                                         
         LA    R1,ACTKEY+ACCORFST                                               
         SR    R0,R0                                                            
         USING OATELD,R1                                                        
BSR02    CLI   0(R1),0                                                          
         BE    BSR02D                                                           
         CLI   0(R1),OATELQ        8C ELEMENT                                   
         BE    BSR02B                                                           
BSR02A   IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     BSR02                                                            
                                                                                
BSR02B   CLI   OATSUB,OATSUB6Q                                                  
         BNE   BSR02A                                                           
         MVC   SRTSWIFT,OATSWIFT                                                
         MVC   SRTINTB#,OATIBAN                                                 
         MVC   SRTINTNM,OATIBNM                                                 
         MVC   SRTINTA1,OATIADL1                                                
         MVC   SRTINTA2,OATIADL2                                                
         MVC   SRTINTA3,OATIADL3                                                
*                                                                               
BSR02D   DS    0H                                                               
         ZAP   SRCAMT,CHTOT        CHECK TOTAL                                  
         ZAP   SRTRNCT,ITEMS       TRANSACTION COUNT SPEC-40627                 
*MN SPEC-44274 SPEC-40627                                                       
         CLI   PROGPROF+8,C'Y'                                                  
         BNE   BSR02E                                                           
         MVC   HALF,PTN#INV                                                     
         LH    RE,HALF                                                          
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         ZAP   SRTRNCT,DUB                                                      
BSR02E   DS    0H                                                               
*MN SPEC-44274 SPEC-40627                                                       
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
         EX    R3,*+8                                                           
         B     *+10                                                             
BSR13    MVC   0(0,RF),0(0)        DATA TO SORT FIELD                           
         LA    RF,1(R3,RF)         RF TO NEXT OUTPUT FIELD                      
         LA    R4,1(R4)            NEXT SORT OPTION FIELD                       
         BCT   R0,BSR05                                                         
*                                                                               
BSR15    L     R3,ATRNIO           A(TRANSACTION RECORD)                        
         USING TRNRECD,R3                                                       
         MVC   SRCLI,PTNCLI        CLIENT                                       
         MVC   SRSEQ,PTNSEQ        SEQUENCE                                     
         MVC   SR#INV,PTN#INV      NUMBER OF ITEMS                              
         MVC   SRTMMOS,PTNMMOS     MEDIA MONTH OF SERVICE                       
         MVC   SRACC,TRNKCULA      ACCOUNT                                      
         MVC   SRKOFC,TRNKOFF      OFFICE                                       
         MVC   SRCNTR,TRNKCULC     CONTRA                                       
         MVC   SRTDTE,TRNKDATE     DATE                                         
         MVC   SRTREF,TRNKREF      REFERENCE                                    
         MVC   SRTSBR,TRNKSBR      SUBREFRENCE                                  
         MVC   CLNT,TRNKCULC+12    GET THE CLIENT CODE                          
*                                                                               
         CLI   TRNKSTYP,26                                                      
         BNE   *+14                                                             
         CLC   TRNKCUNT(2),=C'SJ'  MEDIA TY 26 FROM SJ CONTRA                   
         BE    BSR16                                                            
*                                                                               
         TM    SYEQU,PRDQ+EXPQ     IS IT PRODUCTION OR EXPENSE                  
         BZ    *+10                                                             
BSR16    MVC   CLNT,TRNKCULC+3                                                  
         MVC   SRTCLIC,CLNT        CLIENT CODE                                  
         LA    R2,TRNKEY+ACCORFST  TRANSACTION ELEMENT                          
         USING TRNELD,R2                                                        
         ZAP   SRTAMT,TRNAMNT      PAYABLE AMOUNT                               
         GOTO1 =A(GETCD),DMCB                                                   
         ZAP   SRTCD,CD            CASH DISCOUNT                                
         MVC   SRTANL,TRNANAL      OFFICE FROM TRANSACTION                      
         MVI   SRTNRL,0            NARRATIVE LENGTH                             
         MVI   SRTNRR,C' '                                                      
         MVC   SRTNRR+1(L'SRTNRR-1),SRTNRR                                      
         SR    R1,R1                                                            
         IC    R1,TRNLN                                                         
         SH    R1,=Y(TRNLN1Q)                                                   
         BNP   BSR17                                                            
                                                                                
         TM    DTRSW,DTRFLAT       DDS' FLAT FILE TRANSFER?                     
         BO    *+12                YES ALWAYS PASS THE NARRATIVE                
         CLI   PROGPROF+9,C'N'                                                  
         BE    BSR17                                                            
                                                                                
         STC   R1,SRTNRL           NARRATIVE LENGTH                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRTNRR(0),TRNNARR   AND NARRATIVE                                
         CLC   SRTNRR(L'SPACES),SPACES                                          
         BNE   *+8                                                              
         MVI   SRTNRL,0                                                         
*                                                                               
BSR17    MVC   SRCNME,PTNCNM       CONTRA NAME                                  
         MVC   SRCNML,PTNCLN       AND LENGTH                                   
         MVC   SRTYPE,CHKTYPE      ACCOUNT TYPE                                 
*SPEC-9755, SPEC-13241                                                          
         TM    DTRSW,DTRNEFT       IF NOT EFT RUN SKIP                          
         BZ    *+10                                                             
         MVC   SRTYPE,PTNTYP       ACCOUNT TYPE                                 
*SPEC-9755, SPEC-13241                                                          
         BRAS  RE,SETL             SET ELEMENT DATA                             
         MVI   SRSTAT,0                                                         
         CLC   SROEST,SPACES       FILL IN ESTIMATE AFTER SETL IF               
         BH    *+10                NOT ALREADY FILLED IN                        
         MVC   SROEST,PTNEST                                                    
         TM    RQSW,RQURG          URGENT                                       
         BNO   *+8                                                              
         OI    SRSTAT,SRSURG                                                    
         DROP  R2                                                               
*                                                                               
         L     R2,ADACCSTA         LOAD RSTELD ELEMENT                          
         USING RSTELD,R2                                                        
         CLI   RSTLN,RSTLN3Q       IS THIS NEW LENGTH?                          
         BL    BSR20               NO                                           
         TM    RSTMAIL,RSTMAIOV    IS THIS SPECIAL MAILING?                     
         BZ    *+8                                                              
         OI    SRSTAT,SRMAIL       SET IT IN THE SORT                           
*SPEC-44172                                                                     
         TM    RSTMAIL,RSTMAOV2    OV DIRECT TO VENDOR                          
         BZ    *+8                                                              
         OI    SRSTAT,SRMAIL2      SET IT IN THE SORT                           
*SPEC-44172                                                                     
         DROP  R2                                                               
*                                                                               
BSR20    GOTO1 ADSORTER,DMCB,=C'PUT',SREC                                       
         OI    SCS,SCSACT          SET ACTIVITY SWITCH                          
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* GET SORT RECORDS AND PRINT THE CHECKS                               *         
***********************************************************************         
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
         AHI   RF,7                                                             
         STC   RF,RCSUBPRG                                                      
                                                                                
GETS08   SR    RF,RF                                                            
         ICM   RF,7,TYRTN          DISPLACEMENT TO OVERLAY                      
         AR    RF,RB                                                            
* DSFTK-150                                                                     
         CLI   SRVTYP,VPCARD                                                    
         BE    GETS09                                                           
* DSFTK-150                                                                     
         CLI   SRVTYP,VCCARD                                                    
         BE    GETS09                                                           
         CLI   SRVTYP,VEFT         IS THIS AN EFT VENDOR?                       
         BE    *+12                                                             
         TM    DTRSW,DTRANY        DATA TRANSFER? (EDI820 OR DATA FILE)         
         BZ    *+8                                                              
GETS09   L     RF,AEDTRN           A(EDICT TRANSFER)                            
         ST    RF,ASBRTN           A(SUB-ROUTINE)                               
         MVI   MODE,ACCFRST                                                     
         BASR  RE,RF               PASS ACCFRST                                 
* DSFTK-150                                                                     
         CLI   SRVTYP,VPCARD                                                    
         BE    GETS10                                                           
* DSFTK-150                                                                     
         CLI   SRVTYP,VCCARD                                                    
         BE    GETS10                                                           
         CLI   SRVTYP,VEFT         IS THIS AN EFT VENDOR?                       
         BNE   *+12                                                             
GETS10   TM    DTXSW,EDIFAIL       EFT/820 MAY FAIL IF BANK INF MISSING         
         BO    GETS30              IN THAT CASE DON'T PROCESS                   
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
*                                                                               
GETS15   AP    RUNTOT,SRTAMT                                                    
* DSFTK-150                                                                     
         CLI   SRVTYP,VPCARD                                                    
         BE    GETS16                                                           
* DSFTK-150                                                                     
         CLI   SRVTYP,VCCARD                                                    
         BE    GETS16                                                           
         CLI   SRVTYP,VEFT         IF EFT VENDOR ADD TOTAL HERE TOO             
         BNE   *+14                AND DON'T ADD CASH TO SUBTOT                 
GETS16   AP    EFTTOT,SRTAMT                                                    
         B     *+10                                                             
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
SBL      NTR1  ,                                                                
* DSFTK-150                                                                     
         CLI   SRVTYP,VPCARD                                                    
         BE    SBL17                                                            
* DSFTK-150                                                                     
         CLI   SRVTYP,VCCARD                                                    
         BE    SBL17                                                            
         CLI   SRVTYP,VEFT         IS THIS AN EFT VENDOR?                       
         BE    SBL17                                                            
         TM    DTRSW,DTRANY        DATA TRANSFER (EDI820 OR DATA FILE)          
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
                                                                                
         CLI   PROGPROF+10,C'Y'                                                 
         BE    *+8                                                              
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
ABL      NTR1  ,                                                                
         MVI   MODE,ACCLAST        RESET MODE TO ACCLAST                        
         L     RF,ASBRTN           GO TO OVERLAY                                
         BASR  RE,RF                                                            
* DSFTK-150                                                                     
         CLI   SRVTYP,VPCARD                                                    
         BE    ABL21                                                            
* DSFTK-150                                                                     
         CLI   SRVTYP,VCCARD                                                    
         BE    ABL21                                                            
         CLI   SRVTYP,VEFT         IS THIS AN EFT VENDOR?                       
         BE    ABL21                                                            
         TM    DTRSW,DTRANY        DATA TRANSFER (EDI820 OR DATA FILE)          
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
LNUP     TM    RNSW,RNACT          ANY RUN ACTIVITY                             
         BZR   RE                  SKIP LINE-UPS                                
         NTR1  ,                                                                
         TM    DTRSW,DTRNEFT       ANY NON-EFT VENDORS IN THIS RUN?             
         BO    *+12                YES                                          
         TM    DTRSW,DTRANY+DTREFT DATA TRANSFER? (EDI820 OR DATA FILE)         
         BNZ   LNUP03                                                           
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
         TM    DTRSW,DTRNEFT       ANY NON-EFT VENDORS IN THIS RUN?             
         BO    *+12                YES                                          
         TM    DTRSW,DTRANY+DTREFT DATA TRANSFER? (EDI820 OR DATA FILE)         
         BNZ   XIT                                                              
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
PCK      NTR1  ,                                                                
* DSFTK-150                                                                     
         CLI   SRVTYP,VPCARD                                                    
         BE    PCK00                                                            
* DSFTK-150                                                                     
         CLI   SRVTYP,VCCARD                                                    
         BE    PCK00                                                            
         CLI   SRVTYP,VEFT         IS THIS AN EFT VENDOR?                       
         BNE   *+14                                                             
PCK00    AP    EFTCK#,=P'1'        UPDATE EFT CHECK # TOTAL                     
         B     *+12                                                             
         TM    DTRSW,DTRANY        DATA TRANSFER? (EDI820 OR DATA FILE)         
         BZ    PCK01                                                            
         AP    CHNUM,=P'1'                                                      
         CP    CHTOT,=P'0'         ONLY WANT POSITIVE CHECKS                    
         BNH   XIT                                                              
* DSFTK-150                                                                     
         CLI   SRVTYP,VPCARD                                                    
         BE    PCK31                                                            
* DSFTK-150                                                                     
         CLI   SRVTYP,VCCARD                                                    
         BE    PCK31                                                            
         CLI   SRVTYP,VEFT         IS THIS AN EFT VENDOR?                       
         BE    PCK31               THEN ALREADY ADDED COUNT TO EFTCK#           
         AP    CHOKS,=P'1'         KEEP COUNT OF CHECKS FOR EDI TOO             
         B     PCK31                                                            
*                                                                               
PCK01    CP    CHTOT,=P'0'         DON'T PRINT CHECK FOR ZERO OR MINUS          
         BH    PCK03                                                            
         BAS   RE,VDCK                                                          
         B     XIT                                                              
*                                                                               
PCK03    AP    CHOKS,=P'1'                                                      
         AP    CHNUM,=P'1'                                                      
         GOTO1 ADLN,0                                                           
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
         MVC   0(255,R2),SPACES                                                 
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
         L     R2,APDRFT                                                        
         TM    RNSW2,RUNUAT                                                     
         BZ    *+14                                                             
         MVC   P(L'PDRFT),0(R2)                                                 
         LA    R2,L'PDRFT(R2)                                                   
*                                                                               
         GOTO1 PRINT,DMCB,P,=C'BL01'                                            
         MVC   P,SPACES                                                         
         CLI   BYTE,2                                                           
         BL    PCK23                                                            
*                                                                               
         TM    RNSW2,RUNUAT                                                     
         BZ    *+14                                                             
         MVC   PSECOND(L'PDRFT),0(R2)                                           
         LA    R2,L'PDRFT(R2)                                                   
*                                                                               
         GOTO1 (RF),(R1),PSECOND                                                
         MVC   PSECOND,SPACES                                                   
*                                                                               
PCK23    LA    R0,4                PRINT THE ADDRESS                            
         LA    R4,SRADDR                                                        
*                                                                               
PCK25    OC    0(L'SRADDR,R4),0(R4)                                             
         BZ    PCK30                                                            
         MVC   0(L'SRADDR,R5),0(R4)                                             
*                                                                               
         TM    RNSW2,RUNUAT                                                     
         BZ    PCK27                                                            
         CLI   0(R2),X'FF'                                                      
         BE    PCK27                                                            
         MVC   P(L'PDRFT),0(R2)                                                 
         LA    R2,L'PDRFT(R2)                                                   
*                                                                               
PCK27    DS    0H                                                               
         GOTO1 PRINT,DMCB,P,=C'BL01'                                            
         MVC   P,SPACES                                                         
         LA    R4,L'SRADDR(R4)                                                  
         BCT   R0,PCK25                                                         
*                                                                               
PCK30    DS    0H                                                               
         TM    RNSW2,RUNUAT                                                     
         BZ    PCK30B                                                           
PCK30A   CLI   0(R2),X'FF'                                                      
         BE    PCK30B                                                           
         MVC   P(L'PDRFT),0(R2)                                                 
         LA    R2,L'PDRFT(R2)                                                   
         GOTO1 PRINT,DMCB,P,=C'BL01'                                            
         MVC   P,SPACES                                                         
         B     PCK30A                                                           
*                                                                               
PCK30B   DS    0H                                                               
PCK31    GOTO1 APOST                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VOID A CHECK                                                        *         
***********************************************************************         
VDCK     NTR1  ,                                                                
         TM    DTRSW,DTRANY        FILE TRANSFER? (EDI820 OR DATA FILE)         
         BO    XIT                                                              
         AP    VOIDS,=P'1'         COUNT VOIDS                                  
         AP    CHNUM,=P'1'         INCREMENT THE CHECK NUMMER                   
         GOTO1 ADLN,0              PRINT THE DEADLINE AT END OF REMIT.          
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
         USING TRNRECD,R3                                                       
FLTR     NTR1  ,                                                                
         GOTO1 =A(CLPF),DMCB       GET CLIENT PROFILE                           
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
* BUILD THE PROFILE TABLE                                             *         
***********************************************************************         
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
* GET ID NAME                                                         *         
***********************************************************************         
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
IDNM05   CLI   0(R2),0                                                          
         BE    XIT                                                              
         CLI   0(R2),X'02'                                                      
         BE    IDNM15                                                           
         CLI   0(R2),X'30'                                                      
         BE    IDNM20                                                           
         CLI   0(R2),X'36'                                                      
         BE    IDNM35                                                           
*                                                                               
IDNM10   IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     IDNM05                                                           
*                                                                               
         USING CTDSCD,R2                                                        
IDNM15   SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         SHI   R1,3                                                             
         CHI   R1,6                                                             
         BNH   *+8                                                              
         LA    R1,6                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IDABBR(0),CTDSC                                                  
         B     IDNM10                                                           
*                                                                               
         USING CTDSTD,R2                                                        
         USING EDIDD,R5                                                         
IDNM20   L     R5,=A(EDIWRK)                                                    
         MVC   POWCODE,CTDSTPOW      USE POWER CODE                             
         MVC   LASRCODE,CTDSTLFC                                                
         OC    CTDSTLFC,CTDSTLFC                                                
         BNZ   *+10                                                             
         MVC   LASRCODE,CTDSTPOW                                                
*                                                                               
         MVC   EDIDIDN,CTDSTNAM    IDI DESTINATION NAME                         
         MVC   EDIIDAD1,CTDSTADD   IDI DESTINATION ADDRESS LINE 1               
         MVC   EDIIDAD2,CTDSTAD2   IDI DESTINATION ADDRESS LINE 2               
         LA    R0,2                SET FOR TWO TURNS FOR 2 LINES                
         LA    R3,L'EDIIDAD1       LENGTH OF ONE ADDRESS LINE                   
         LA    R4,EDIIDAD2         LAST ADDR LINE (2ND)                         
*                                                                               
IDNM25   GOTO1 ADFORM,DMCB,((R3),(R4)),(25,EDIICTY),                   +        
               EDIIST,(10,EDIIZIP),(2,EDIICTRY),(30,EDIICNME)                   
         TM    0(R1),X'80'        80=SERIOUS ERROR                              
         BO    IDNM30                                                           
         CLC   EDIICTY,SPACES                                                   
         BE    IDNM30                                                           
         MVC   0(L'EDIIDAD1,R4),SPACES    CLEAR CURRENT LINE                    
         B     IDNM10                                                           
IDNM30   CLC   0(L'EDIIDAD1,R4),SPACES IF ZERO, THEN NO ADDRESS LINE            
         BNH   *+8                                                              
         AHI   R3,L'EDIIDAD1      SEND ANOTHER LINE                             
         AHI   R4,-L'EDIIDAD1     BACK UP                                       
         BCT   R0,IDNM25                                                        
         B     IDNM10                                                           
         DROP  R2,R5                                                            
*                                                                               
         USING CTORGD,R2                                                        
         USING EDIDD,RF                                                         
IDNM35   L     RF,=A(EDIWRK)                                                    
         LA    RE,CTORGNAM+L'CTORGNAM-1 POINT TO END OF ORIGIN NAME             
         LA    R1,L'CTORGNAM                                                    
IDNM40   CLI   0(RE),C' '                                                       
         BH    IDNM45                                                           
         BCTR  RE,0                                                             
         BCT   R1,IDNM40                                                        
         B     IDNM10                                                           
*                                                                               
IDNM45   BCTR  R1,0                                                             
         EXMVC R1,EDIORGN,CTORGNAM                                              
         MVC   EDIIADDR,CTORGADD                                                
         B     IDNM10                                                           
         DROP  R2,RF                                                            
         EJECT                                                                  
***********************************************************************         
* GET SYSTEM NUMBER                                                   *         
***********************************************************************         
SYS      NTR1  ,                                                                
         XC    HALF,HALF                                                        
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
         MVC   HALF,SYSNAME+3      THIS IS THE ACC FILE NUMBER                  
         B     XIT                                                              
*                                                                               
SYS5     IC    R0,SYSLEN           BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     SYS3                                                             
         DROP  R1,R2,RF                                                         
         EJECT                                                                  
***********************************************************************         
* SPOT TYPE 13                                                        *         
***********************************************************************         
S13      NTR1  ,                                                                
         LA    RF,SPS              STATION IF CONTRA STARTS W/SPACES            
         CLC   SRCNTR+4(8),SPACES                                               
         BE    *+8                                                              
         LA    RF,SPR              REP IF CONTRA IS NOT SPACES                  
         BASR  RE,RF                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SPOT STATION                                                        *         
***********************************************************************         
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
STATOTL  NTR1  ,                                                                
         CP    CURLNE,ML3LNE       MUST HAVE 3 LEFT                             
         BL    *+12                                                             
         BAS   RE,VDCK             VOID THE CHECK                               
         B     *+8                                                              
         BAS   RE,PRTLNE           BLANK LINE                                   
         CURED STATOT,(11,P+74),2,MINUS=YES                                     
         LA    RE,DICO           NEED ADDRESSABILITY TO THE DDICT               
         AHI   RE,AC@STOT-DICO                                                  
         MVC   P+53(L'AC@STOT),0(RE)           STATION TOTAL                    
         ZAP   STATOT,=P'0'                                                     
         BAS   RE,PRTLNE           PRINT THE TOTAL                              
         BAS   RE,PRTLNE           AND SKIP A LINE                              
         B     XIT                                                              
***********************************************************************         
* SET MIDLINES                                                        *         
***********************************************************************         
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
         EX    R3,*+8                                                           
         B     *+10                                                             
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
         ZAP   DUB,MAXLNE                                                       
         CP    DOUBLE,DUB          TEST ENOUGH LINES FOR NARRATIVE              
         BL    *+8                                                              
         BAS   RE,VDCK             NOT ENOUGH VOID IT                           
         BRAS  RE,PRN              PRINT NARRATIVE                              
         BAS   RE,PRTLNE           SKIP A LINE                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SET STATION CALL LETTERS                                            *         
*                                                                     *         
* R4 = STATION CALL LETTERS                                           *         
* R2 = PRINT AREA (HEADS OR MIDS)                                     *         
***********************************************************************         
SETSTA   LR    R0,RE                                                            
         MVC   1(L'AC@STA,R2),AC@STA       'STATION'                            
         LA    R5,L'AC@STA(R2)                                                  
         ST    R5,FULL                                                          
         BRAS  RE,FNDNXT           R5 TO NEXT SPACE                             
         L     R5,FULL                                                          
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
SETSTAX  DS    0H                                                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT TYPE 13                                                       *         
***********************************************************************         
P13      NTR1  ,                                                                
         LA    RF,PRP              PUB IF CONTRA STARTS W/SPACES                
         CLC   SRCNTR+4(8),SPACES                                               
         BE    *+8                                                              
         LA    RF,PRR              REP IF CONTRA IS NOT SPACES                  
         BASR  RE,RF                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT PUBLICATION                                                   *         
***********************************************************************         
PRP      NTR1  ,                                                                
         CLI   MODE,ACCFRST        FIRST FOR THE ACCOUNT                        
         BNE   PRP7                                                             
         MVC   SUBTXT,SPACES                                                    
         MVC   ACCTXT,SPACES                                                    
         LA    RE,DICO           NEED ADDRESSABILITY TO THE DDICT ENTRY         
         AHI   RE,AC@CLITO-DICO                                                 
         MVC   SUBTXT+2(L'AC@CLITO),0(RE)       'CLIENT TOTALS'                 
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
PRR      NTR1  ,                                                                
         CLI   MODE,ACCFRST        FIRST FOR THE ACCOUNT                        
         BNE   PRR9                                                             
         MVC   SUBTXT,SPACES                                                    
         MVC   ACCTXT,SPACES                                                    
         LA    RE,DICO           NEED ADDRESSABILITY TO THE DDICT ENTRY         
         AHI   RE,AC@CLITO-DICO                                                 
         MVC   SUBTXT+2(L'AC@CLITO),0(RE)       'CLIENT TOTALS'                 
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
PUBTOTP  NTR1  ,                                                                
         LA    RF,PRTLNE                                                        
         CP    CURLNE,ML2LNE       MUST HAVE 2 LINES                            
         BL    *+8                                                              
         LA    RF,VDCK                                                          
         BASR  RE,RF               BLANK OR NEW PAGE                            
         LA    RE,DICO           NEED ADDRESSABILITY TO THE DDICT ENTRY         
         AHI   RE,AC@PUBT-DICO                                                  
         MVC   P+22(L'AC@PUBT),0(RE)     'PUBLICATION TOTALS'                   
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
NEWCLI   NTR1  ,                                                                
         CP    CURLNE,ML1LNE                                                    
         BL    *+8                                                              
         BAS   RE,VDCK                                                          
         OC    SRXCLI,SRXCLI      EXTRA PAYMENT ELEMENT                         
         BZ    *+10                                                             
         MVC   P+1(20),SRXCLI     USE CLIENT NAME                               
         CLC   ALPHAID,=C'H7'     FOR MINDSHARE/CLIENT MFC/OFFICE FM            
         BNE   NEWCLI5            OVERRIDE THE CLIENT NAME FOR NOW              
         CLC   SRTCLIC,=C'MFC'                                                  
         BNE   NEWCLI5                                                          
         CLC   SRKOFC,=C'FM'                                                    
         BNE   NEWCLI5                                                          
         MVC   P+1(24),=C'FORD COMMERCIAL DIVISION'                             
NEWCLI5  BAS   RE,PRTLNE                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SETUP PUBLICATION DETAIL  LINE                                      *         
***********************************************************************         
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
         CHI   R3,112              GREATER THAN 4 X 28 - BIGGER CHOP            
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
         BRAS  RE,PRN              PRINT NARRATIVE                              
         BAS   RE,PRTLNE           BLANK LINE                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRODUCTION / EXPENSE                                                *         
***********************************************************************         
EXP      NTR1  ,                                                                
         CLI   MODE,ACCFRST        FIRST FOR ACCOUNT                            
         BNE   EXP3                                                             
         ZAP   CURLNE,=P'0'                                                     
         ZAP   INVPAY,=P'0'                                                     
                                                                                
         XC    LASTPRD,LASTPRD                                                  
         XC    LASTJOB,LASTJOB                                                  
                                                                                
         MVC   SUBTXT,SPACES                                                    
         MVC   ACCTXT,SPACES                                                    
         LA    RE,DICO           NEED ADDRESSABILITY TO THE DDICT ENTRY         
         AHI   RE,AC@CCTL-DICO                                                  
         MVC   SUBTXT+00(L'AC@CCTL),0(RE)    'CLIENT/CATEGORY TOTAL'            
         MVC   ACCTXT+00(L'TXT8),TXT8        TOTALS FOR CHECK                   
         XC    CLISAVE,CLISAVE                                                  
         MVC   CLIPRJB,SPACES                                                   
         B     XIT                                                              
*                                                                               
EXP3     CLI   MODE,PROCTRNS       PROCESS TRANSACTION                          
         BNE   EXP33                                                            
         CP    CURLNE,=P'0'                                                     
         BNE   EXP5                                                             
         BAS   RE,NEWPGE                                                        
         BRAS  RE,CLIOUT           FIRST CLIENT FOR 2ND & SUBSQ. CHECKS         
         BAS   RE,PRTLNE                                                        
         LA    R0,5                CLEAR THE PRINT BLOCK                        
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
EXP7     EQU   *                                                                
         CLC   CLISAVE,SRCNTR+3    SAME CLIENT                                  
         BE    EXP11                                                            
         CP    CURLNE,ML2LNE                                                    
         BL    *+8                                                              
         BAS   RE,VDCK                                                          
         CLI   PROGPROF+10,C'Y'                                                 
         BNE   *+8                                                              
         BAS   RE,INVPRT                                                        
         BRAS  RE,CLIOUT                                                        
         BAS   RE,PRTLNE                                                        
                                                                                
         XC    LASTPRD,LASTPRD                                                  
         XC    LASTJOB,LASTJOB                                                  
EXP11    EQU   *                                                                
         CLI   PROGPROF+10,C'Y'                                                 
         BNE   EXP11H                                                           
         CLC   ALPHAID,=C'BD'          BDNY  SPECIAL(NO NAME)                   
         BNE   EXP11B                                                           
         CLI   QLEDGER,C'X'                                                     
         BE    EXP11F                                                           
EXP11B   CLC   LASTPRD,SROPRD                                                   
         BNE   EXP12                                                            
         CLC   LASTJOB,SROJOB                                                   
         BNE   EXP12                                                            
EXP11F   CLC   SRTREF,INVNO                                                     
         BNE   EXP12                                                            
                                                                                
         CLI   SRTNRL,0            IS THERE A NARRATIVE                         
         BE    EXP11H                                                           
         CLC   SVNARRWK,SPACES                                                  
         BNH   EXP11H                                                           
         CLC   SVNARRWK+31(L'SRTNRR),SRTNRR                                     
         BNE   EXP12                                                            
                                                                                
EXP11H   EQU   *                                                                
         CLC   SRTREF,INVNO                                                     
         BE    EXP15                                                            
         CP    INVCNT,=P'1'                                                     
         BH    EXP12                                                            
         MVC   P,SPACES                                                         
         BAS   RE,PRTLNE                                                        
         B     EXP13                                                            
*                                                                               
EXP12    EQU   *                                                                
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
EXP16A   CLI   SRTNRL,0            IS THERE A NARRATIVE                         
         BE    EXP17                                                            
         SR    R3,R3                                                            
         IC    R3,SRTNRL                                                        
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   NARRWRK+31(0),SRTNRR                                             
         MVC   SVNARRWK,NARRWRK                                                 
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
         ST    R5,FULL                                                          
         BRAS  RE,FNDNXT                                                        
         L     R5,FULL                                                          
         MVI   0(R5),C'='                                                       
         MVC   1(6,R5),SROPRD       PRODUCT CODE                                
         LA    R5,8(R5)                                                         
         LA    RE,DICO           NEED ADDRESSABILITY TO THE                     
         AHI   RE,AC@JOB-DICO    DATA DICTIONARY                                
         MVC   0(L'AC@JOB,R5),0(RE)     C'JOB='                                 
         LA    R5,L'AC@JOB-1(R5)                                                
         ST    R5,FULL                                                          
         BRAS  RE,FNDNXT                                                        
         L     R5,FULL                                                          
         MVI   0(R5),C'='                                                       
         MVC   1(6,R5),SROJOB       PRODUCT CODE                                
         MVC   LASTJOB,SROJOB                                                   
         MVC   LASTPRD,SROPRD                                                   
                                                                                
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
         GOTO1 CHOPPER,DMCB,((R4),NARRWRK),(38,PBLK+01),(C'P',4)                
                                                                                
         MVC   CLIPRJB,PBLK                                                     
                                                                                
         SR    R3,R3                                                            
         IC    R3,DMCB+11          NUMBER OF LINES                              
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
EXP29    LA    R3,1                                                             
         CVD   R3,DUB                                                           
         AP    DUB,CURLNE                                                       
         CP    DUB,MAXLNE          WILL IT ALL FIT                              
         BNH   *+8                                                              
         BAS   RE,VDCK             IF NOT GO TO NEXT CHECK                      
         LA    R5,PBLK                                                          
         LA    R0,5                                                             
                                                                                
         CLI   PROGPROF+10,C'Y'                                                 
         BE    XIT                                                              
                                                                                
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
EXP33A   ZAP   DOUBLE,INVPAY                                                    
         AP    DOUBLE,INVCD                                                     
                                                                                
         CLI   PROGPROF+10,C'Y'                                                 
         BE    EXP34                                                            
                                                                                
         CP    DOUBLE,SUBC1        NET PAYABLE VS NET PAYABLE                   
         BE    EXP35                                                            
         CP    INVCNT,=P'1'        IF ONLY 1 LINE - DON'T BOTHER                
         BH    EXP34                                                            
         MVC   P,SPACES                                                         
         BAS   RE,PRTLNE                                                        
         B     EXP35                                                            
*                                                                               
EXP34    EQU   *                                                                
         BAS   RE,INVPRT                                                        
*                                                                               
EXP35    ZAP   INVCNT,=P'0'                                                     
         ZAP   INVPAY,=P'0'                                                     
         ZAP   INVCD,=P'0'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT INVOICE TOTAL LINE                                            *         
***********************************************************************         
INVPRT   NTR1                                                                   
         CP    INVPAY,=P'0'                                                     
         BNE   *+14                                                             
         CP    INVCD,=P'0'                                                      
         BE    XIT                                                              
         CP    CURLNE,ML3LNE       MUST HAVE 3 LINES                            
         BL    *+12                                                             
         BAS   RE,VDCK                                                          
         B     *+8                                                              
*                                                                               
         CLI   PROGPROF+10,C'Y'                                                 
         BE    *+8                                                              
         BAS   RE,PRTLNE           SKIP A LINE                                  
*                                                                               
         CLI   PROGPROF+10,C'Y'                                                 
         BNE   INVP2                                                            
         MVC   P(L'CLIPRJB),CLIPRJB                                             
         B     INVP3                                                            
*                                                                               
INVP2    LA    RE,DICO         NEED ADDRESSABILITY TO THE DDICT ENTRY           
         AHI   RE,AC@INVTO-DICO                                                 
         MVC   P+20(L'AC@INVTO),0(RE)        '* TOTAL FOR INVOICE *'            
INVP3    MVC   P+42(6),INVNO                                                    
         CURED INVPAY,(11,P+75),2,MINUS=YES                                     
         CURED INVCD,(9,P+64),2,MINUS=YES,ZERO=BLANK                            
         AP    INVPAY,INVCD                                                     
         CURED INVPAY,(11,P+49),2,MINUS=YES                                     
         BAS   RE,PRTLNE                                                        
*                                                                               
         CLI   PROGPROF+10,C'Y'                                                 
         BNE   INVP10                                                           
*                                                                               
         LA    R0,4                                                             
         MVC   PBLK(132),SPACES                                                 
         LA    R5,PBLK+132                                                      
         CLC   0(132,R5),SPACES                                                 
         BE    XIT                                                              
INVP5    MVC   P,0(R5)             PRINT THE BLOCK                              
         BAS   RE,PRTLNE                                                        
         MVC   0(132,R5),SPACES                                                 
         LA    R5,132(R5)                                                       
         CLC   0(132,R5),SPACES                                                 
         BE    XIT                                                              
         BCT   R0,INVP5                                                         
INVP10   EQU   *                                                                
         BAS   RE,PRTLNE                                                        
         B     XIT                                                              
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* COKE EXPENDITURE                                                    *         
***********************************************************************         
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
         BRAS  RE,PRN              PRINT NARRATIVE                              
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
PRDTOT   NTR1  ,                                                                
         CP    ITEMS,=P'2'                                                      
         BL    PRDTOT4             NO PRODUCT TOTALS IF ONE ITEM                
         CP    CURLNE,ML2LNE                                                    
         BL    *+8                                                              
         BAS   RE,VDCK                                                          
*                                                                               
PRDTOT2  LA    RE,DICO           NEED ADDRESSABILITY TO THE DDICT ENTRY         
         AHI   RE,AC@PROTO-DICO                                                 
         MVC   P+20(L'AC@PROTO),0(RE)        '* TOTAL FOR PRODUCT *'            
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
*&&                                                                             
***********************************************************************         
* AOR                                                                 *         
***********************************************************************         
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
         CLI   SYLDG,C'P'              IS IT PRINT?                             
         BE    *+12                     YES                                     
         CLI   SYLDG,C'Q'                                                       
         BNE   AOR19A                   YES                                     
         CLC   ALPHAID,=C'H7'     FOR MINDSHARE/CLIENT MFC/OFFICE FM            
         BNE   AOR19A             OVERRIDE THE CLIENT NAME FOR NOW              
         CLC   SRTCLIC,=C'MFC'                                                  
         BNE   AOR19A                                                           
         CLC   SRKOFC,=C'FM'                                                    
         BNE   AOR19A                                                           
         MVC   P+2(24),=C'FORD COMMERCIAL DIVISION'                             
AOR19A   MVC   P+24(11),SRXPRD                                                  
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
         CHI   RE,9                                                             
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
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   NARRWRK(0),SRTNRR                                                
*                                                                               
AOR25    EQU   *                                                                
         BAS   RE,AORDP                                                         
         B     XIT                                                              
*                                                                               
AOR30    CLI   MODE,SBACLAST       LAST FOR THE SUBACCOUNT                      
         BNE   XIT                                                              
         CP    CURLNE,ML2LNE       TWO LINES NEEDED FOR CLIENT TOTAL            
         BL    *+8                                                              
         BAS   RE,VDCK                                                          
*                                                                               
AOR33    LA    RE,DICO           NEED ADDRESSABILITY TO THE DDICT ENTRY         
         AHI   RE,AC@CLITO-DICO                                                 
         MVC   P+53(L'AC@CLITO),0(RE)         'CLIENT TOTAL'                    
         CURED SUBTOT,(11,P+74),2,MINUS=YES                                     
         BAS   RE,PRTLNE           PRINT CLIENT TOTAL                           
         BAS   RE,PRTLNE                                                        
         ZAP   SUBTOT,=P'0'                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* AOR DETAIL PRINTING                                                           
***********************************************************************         
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
         BRAS  RE,PRN              PRINT NARRATIVE                              
         BAS   RE,PRTLNE           BLANK LINE                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* UNWIRED REP                                                         *         
***********************************************************************         
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
         BNH   UWR11               IF NO END DO MMMDD/YY                        
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
                                                                                
         CLI  PROGPROF+10,C'Y'                                                  
         BE   PXN9                                                              
                                                                                
         SR    R3,R3                                                            
         IC    R3,SRTNRL                                                        
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
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
NEWPGE   MVI   FORCEHED,C'Y'                                                    
         ZAP   CURLNE,=P'1'                                                     
         BR    RE                                                               
***********************************************************************         
* PRINT A REMITTANCE LINE                                             *         
***********************************************************************         
         DC    F'0'                                                             
PRTLNE   ST    RE,PRTLNE-4                                                      
         TM    DTRSW,DTRANY        DATA TRANSFER (EDI820 OR DATA FILE)          
         BO    PRTLNX                                                           
         AP    CURLNE,=P'1'                                                     
         CLI   SPACING,2                                                        
         BNE   *+10                                                             
         AP    CURLNE,=P'1'                                                     
         GOTO1 ACREPORT                                                         
PRTLNX   L     RE,PRTLNE-4                                                      
         BR    RE                                                               
***********************************************************************         
* OTHER ROUTINES                                                      *         
* FULL CONTAINS THE ADDRESS                                                     
***********************************************************************         
FNDNXT   L     R1,FULL                                                          
FNDNXT5  CLI   0(R1),C' '          FIND LAST CHARACTER                          
         JH    FNDNXT10                                                         
         BCTR  R1,0                                                             
         J     FNDNXT5                                                          
FNDNXT10 LA    R1,1(R1)            R5 TO NEXT SPACE                             
         ST    R1,FULL             STORE ADDR OF NEXT SPACE                     
         BR    RE                                                               
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA CONSTANTS                                                      *         
***********************************************************************         
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
MIXERRM  DS    XL1                                                              
MIXMISS  EQU   X'80'               MISSING EFT BANK SETUP FOR EFT VNDR          
MIXMIX   EQU   X'40'               MIX OF EFT AND EDI820                        
* DSFTK-150                                                                     
MIXMIXP  EQU   X'20'               MIX OF PCARD AND EDI820                      
MIXMIXE  EQU   X'10'               MIX OF PCARD AND EFT                         
MIXMISP  EQU   X'08'               MISSING PCARD BANK SETUP                     
MIXMIXC  EQU   X'04'               MIX OF CCARD AND EDI820                      
MIXMISC  EQU   X'02'               MISSING CCARD BANK SETUP                     
* DSFTK-150                                                                     
*                                                                               
MIXERRT  DC    100XL12'00'         ROOM FOR 100 ERROR ACCOUNTS                  
MXERRENT EQU   (*-MIXERRT)/12                                                   
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* GET AND PRINT CLIENT NAME                                           *         
***********************************************************************         
CLIOUT   NTR1  BASE=*,LABEL=*                                                   
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
         XIT1                                                                   
***********************************************************************         
* GET OFFICE-CHECK NUMBER AND LEDGER LOCK STATUS                      *         
***********************************************************************         
OFNM     NTR1  BASE=*,LABEL=*                                                   
         XC    OFFLIST,OFFLIST     INIT OFFICE                                  
         XC    CLILIST,CLILIST     AND CLIENT LISTS                             
         LA    R3,OFFLIST                                                       
         LA    R5,CLILIST                                                       
         L     R4,ADLEDGER                                                      
*                                                                               
         USING CHARECD,R2          CHECK AUTHORIZATION RECORD                   
         L     R2,AIO                                                           
         XC    CHAKEY,CHAKEY                                                    
         MVI   CHAKTYP,CHAKTYPQ    X'10'                                        
         MVC   CHAKCULA,0(R4)      C/U/L                                        
         MVC   KEYSAVE,CHAKEY      SAVE KEY FOR LATER COMPARE                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCFIL,AIO,AIO                               
         B     OFNM03AB                                                         
*                                                                               
OFNM03AA GOTO1 DATAMGR,DMCB,DMRSEQ,ACCFIL,AIO,AIO    READ THE NEXT              
OFNM03AB CLI   8(R1),0                                                          
         BNE   OFNM15                                                           
         CLC   KEYSAVE(CHAKLDG-CHAKEY+1),CHAKEY                                 
         BNE   OFNM15                                                           
         DROP   R2                                                              
*                                                                               
         LR    RF,R2                                                            
         AH    RF,DATADISP                                                      
*                                                                               
OFNM03   CLI   0(RF),X'54'                                                      
         BE    OFNM05                                                           
         CLI   0(RF),0                                                          
         BE    OFNM03AA                                                         
OFNM04   SR    R0,R0                                                            
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     OFNM03                                                           
*                                                                               
         USING OCNELD,RF                                                        
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
         MVC   SVBANK,OCNBANK      SAVE BANK ACCOUNT FOR LATER                  
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
                                                                                
         CLI   OCNLASR,OCNWSP                                                   
         BNE   *+8                                                              
         OI    PRNTSW,WEBWSP                                                    
                                                                                
         CLI   OCNLASR,OCNWSL                                                   
         BNE   *+8                                                              
         OI    PRNTSW,WEBWSP                                                    
                                                                                
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
*        TM    OCNSTAT2,OCNSFTP    FILE TRANSFER                                
*        BZ    *+12                                                             
*        OI    FTPSW,FTPTRN        SET FOR FILE TRANSFER                        
*        OI    RNSW,MICR           SET MICRO FOR AUTO REGISTER                  
*                                                                               
         TM    RNSW2,RUNUAT                                                     
         BO    OFNM06G                                                          
*                                                                               
         TM    OCNSTAT2,OCNS820    820 DATA TRANSFER                            
         BZ    OFNM06G                                                          
         OI    DTRSW,DTR820        SET 820 DATA TRANSFER                        
         OI    DTRSW,DTRANY        AND SET GENERAL DATA XFER BIT                
         OI    RNSW,MICR           SET MICRO FOR AUTO REGISTER                  
* DSFTK-150                                                                     
         OI    ACCSW1,EDIINCL      SET EDI820 VENDOR IN THIS RUN                
* DSFTK-150                                                                     
*                                                                               
OFNM06G  TM    OCNSTAT2,OCNSDFIL   FLATFILE?                                    
         BZ    *+16                                                             
         OI    DTRSW,DTRFLAT                                                    
         OI    DTRSW,DTRANY        AND SET GENERAL DATA XFER BIT                
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
OFNM15   NI    DTXSW,X'FF'-EDIFAIL                                              
         BRAS  RE,RDSC             GET BANK INFO FROM SC ACCOUNT                
         BE    *+8                                                              
         OI    DTXSW,EDIFAIL                                                    
         XIT1                                                                   
         DROP  R1,RF                                                            
         EJECT                                                                  
**********************************************************************          
* LOAD OUTSIDE PHASES                                                *          
**********************************************************************          
         USING AC55D,RC                                                         
         USING ACWORKD,RA                                                       
         USING MASTD,R5                                                         
LOAD     NTR1  BASE=*,LABEL=*                                                   
         L     R5,ADMASTC                                                       
         MVC   DUB,=CL8'ADFORM'                                                 
         MVC   DUB+6(1),MCTEST3    LOAD EITHER LIVE OR TEST                     
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   LOAD10                                                           
         MVC   DUB,=CL8'ADFORM'                                                 
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
LOAD10   MVC   ADFORM,4(R1)        A(ADFORM)                                    
         MVC   MCAPHAS3,4(R1)      SET UP PATCHABILITY                          
*                                                                               
         ICM   R0,15,0(R1)         LENGTH OF PHASE                              
         ICM   RF,15,4(R1)         A(ADFORM)                                    
         STCM  RF,15,MCUSRDMP                                                   
         STCM  RF,15,WORD          ADDRESS OF 1ST PHASE                         
         STCM  R0,15,FULL          LENGTH OF 1ST PHASE                          
         AR    RF,R0                                                            
         LA    RF,4095(RF)                                                      
         STCM  RF,15,MCUSRDMP+4                                                 
*                                                                               
         MVC   DUB,=CL8'GETFRM'    GETFORM                                      
         MVC   DUB+6(1),MCTEST4    LOAD EITHER LIVE OR TEST                     
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   LOAD20                                                           
         MVC   DUB,=CL8'GETFRM'                                                 
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
LOAD20   MVC   VGETFORM,4(R1)      A(GETFORM)                                   
         MVC   MCAPHAS4,4(R1)      SET UP PATCHABILITY                          
*                                                                               
         ICM   R0,15,0(R1)         LENGTH OF PHASE                              
         L     R1,FULL                                                          
         AR    R0,R1               ADD LENGTH OF PREVIOUS PHASE                 
         ST    R0,FULL                                                          
         ICM   RF,15,WORD          A(GETFORM)                                   
         STCM  RF,15,MCUSRDMP                                                   
         AR    RF,R0                                                            
         LA    RF,4095(RF)                                                      
         STCM  RF,15,MCUSRDMP+4                                                 
*                                                                               
*                                                                               
         XC    TEST5,TEST5                                                      
         MVC   BYTE,MCUPSI         MCUPSI 1=A, 2=B, 3=C                         
         NI    BYTE,TESTLVL                                                     
         BZ    *+14                                                             
         MVI   TEST5,X'C0'                                                      
         OC    TEST5,BYTE          SET TEST LEVEL                               
*                                                                               
         MVC   DUB,=CL8'T00A64A'   GETBANK                                      
         MVC   DUB+6(1),TEST5                                                   
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   LOAD30                                                           
         MVC   DUB,=CL8'T00A64'                                                 
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
LOAD30   MVC   VGETBANK,4(R1)      A(GETBANK)                                   
         MVC   MCAPHAS1,4(R1)      SET UP PATCHABILITY                          
*                                                                               
         ICM   R0,15,0(R1)         LENGTH OF PHASE                              
         L     R1,FULL                                                          
         AR    R0,R1               ADD LENGTH OF PREVIOUS PHASE                 
         ST    R0,FULL                                                          
         ICM   RF,15,WORD          A(GETBANK)                                   
         STCM  RF,15,MCUSRDMP                                                   
         AR    RF,R0                                                            
         LA    RF,4095(RF)                                                      
         STCM  RF,15,MCUSRDMP+4                                                 
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* SEND EMAIL THAT CHECK INFO IS MISSING FROM CHECK RECORD             *         
***********************************************************************         
         USING AC55D,RC                                                         
         USING ACWORKD,RA                                                       
EMLMISS  NTR1  BASE=*,LABEL=*                                                   
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
                                                                                
         GOTO1 VSMTP,DMCB,('SMTPAINI',0)        INITIALIZE                      
                                                                                
*        WORK=EMAIL ADDRESSES : TEXT=SUBJECT LINE                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'MISSLIST),MISSLIST                                        
         MVC   MISSTXT,SPACES                                                   
         MVC   MISSTXT(L'EMLMSUB),EMLMSUB                                       
         GOTO1 VSMTP,DMCB,('SMTPAPRS',WORK),(L'MISSTXT,MISSTXT)                 
                                                                                
*        BODY OF EMAIL                                                          
*        REQUEST ID                                                             
*        MVC   MISSTXT,SPACES                                                   
*        MVC   MISSTXT(L'IDILINE),IDILINE                                       
*        MVC   MISSTXT+13(L'IDABBR),IDABBR                                      
*        GOTO1 VSMTP,DMCB,('SMTPAPTL',MISSTXT)                                  
*        JOBNAME                                                                
         MVC   MISSTXT,SPACES                                                   
         MVC   MISSTXT(L'JOBLINE),JOBLINE                                       
         MVC   MISSTXT+6(L'RCJOB),RCJOB                                         
         GOTO1 VSMTP,DMCB,('SMTPAPTL',MISSTXT)                                  
*        REQUESTOR FROM REQUEST CARD                                            
         MVC   MISSTXT,SPACES                                                   
         MVC   MISSTXT(L'RQSLINE),RQSLINE                                       
         MVC   MISSTXT+12(L'QUESTOR),QUESTOR                                    
         GOTO1 VSMTP,DMCB,('SMTPAPTL',MISSTXT)                                  
*        RUN DATE                                                               
         MVC   MISSTXT,SPACES                                                   
         MVC   MISSTXT(L'DATELINE),DATELINE                                     
         MVC   MISSTXT+7(L'RCDATE),RCDATE                                       
         GOTO1 VSMTP,DMCB,('SMTPAPTL',MISSTXT)                                  
*        UNIT/LEDGER                                                            
         MVC   MISSTXT,SPACES                                                   
         MVC   MISSTXT(L'LDGLINE),LDGLINE                                       
         MVC   MISSTXT+9(2),QUNIT                                               
         GOTO1 VSMTP,DMCB,('SMTPAPTL',MISSTXT)                                  
                                                                                
         GOTO1 VSMTP,DMCB,('SMTPASND',0)        SEND EMAIL                      
         GOTO1 VSMTP,DMCB,('SMTPAEND',0)        DETACH FROM JESMAIL             
         XIT1                                                                   
                                                                                
         LTORG                                                                  
                                                                                
MISSLIST DC    C'US-ACC_TEAM_NY,INT_PROD:'                                      
*MISSLIST DC    C'MNASCA@MEDIAOCEAN.COM:'                                       
EMLMSUB  DC    C'** URGENT ** : CHECK ID MISSING FROM CHECK RECORD'             
                                                                                
IDILINE  DC    C'REQUEST ID = XXXXXXX'                                          
JOBLINE  DC    C'JOB = XXXXXXXX'                                                
RQSLINE  DC    C'REQUESTOR = XXXXXXXXXXX'                                       
DATELINE DC    C'DATE = MM/DD/YY'                                               
LDGLINE  DC    C'LEDGER = XX'                                                   
MISSTXT  DC    CL80' '                                                          
         EJECT                                                                  
***********************************************************************         
* SEND EMAIL WITH LIST OF EFT ACCOUNTS NOT PROCESSED DUE TO MIXING    *         
***********************************************************************         
         USING AC55D,RC                                                         
         USING ACWORKD,RA                                                       
EMLMIX   NTR1  BASE=*,LABEL=*                                                   
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
                                                                                
         GOTO1 VSMTP,DMCB,('SMTPAINI',0)        INITIALIZE                      
                                                                                
*        WORK=EMAIL ADDRESSES : TEXT=SUBJECT LINE                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'MIXLIST),MIXLIST                                          
*                                                                               
         TM    RNSW2,RUNTST                                                     
         BNO   *+10                                                             
         MVC   WORK(L'MIXLISTT),MIXLISTT                                        
*                                                                               
         MVC   MIXTXT,SPACES                                                    
         MVC   MIXTXT(L'EMLMXSUB),EMLMXSUB                                      
* DSFTK-150                                                                     
         TM    MIXERRM,MIXMIXP                                                  
         BZ    *+10                                                             
         MVC   MIXTXT(L'EMLMXSUB),EMLMXSP1                                      
*                                                                               
         TM    MIXERRM,MIXMIXC                                                  
         BZ    *+10                                                             
         MVC   MIXTXT(L'EMLMXSUB),EMLMXSP3                                      
*                                                                               
         TM    MIXERRM,MIXMIXE                                                  
         BZ    *+10                                                             
         MVC   MIXTXT(L'EMLMXSUB),EMLMXSP2                                      
* DSFTK-150                                                                     
         MVC   MIXTXT+54(8),RCJOB                                               
         GOTO1 VSMTP,DMCB,('SMTPAPRS',WORK),(L'MIXTXT,MIXTXT)                   
                                                                                
*        BODY OF EMAIL                                                          
*        REQUEST ID                                                             
         MVC   MIXTXT,SPACES                                                    
         MVC   MIXTXT(L'IDIMLINE),IDIMLINE                                      
         MVC   MIXTXT+13(L'IDABBR),IDABBR                                       
         GOTO1 VSMTP,DMCB,('SMTPAPTL',MIXTXT)                                   
*        JOBNAME                                                                
         MVC   MIXTXT,SPACES                                                    
         MVC   MIXTXT(L'JOBMLNE),JOBMLNE                                        
         MVC   MIXTXT+6(L'RCJOB),RCJOB                                          
         GOTO1 VSMTP,DMCB,('SMTPAPTL',MIXTXT)                                   
*        REQUESTOR FROM REQUEST CARD                                            
         MVC   MIXTXT,SPACES                                                    
         MVC   MIXTXT(L'RQSMLINE),RQSMLINE                                      
         MVC   MIXTXT+12(L'QUESTOR),QUESTOR                                     
         GOTO1 VSMTP,DMCB,('SMTPAPTL',MIXTXT)                                   
*        RUN DATE                                                               
         MVC   MIXTXT,SPACES                                                    
         MVC   MIXTXT(L'DATEMLNE),DATEMLNE                                      
         MVC   MIXTXT+7(L'RCDATE),RCDATE                                        
         GOTO1 VSMTP,DMCB,('SMTPAPTL',MIXTXT)                                   
*        UNIT/LEDGER                                                            
         MVC   MIXTXT,SPACES                                                    
         MVC   MIXTXT(L'LDGMLNE),LDGMLNE                                        
         MVC   MIXTXT+9(2),QUNIT                                                
         GOTO1 VSMTP,DMCB,('SMTPAPTL',MIXTXT)                                   
*        BLANK LINE                                                             
         MVC   MIXTXT,SPACES                                                    
         GOTO1 VSMTP,DMCB,('SMTPAPTL',MIXTXT)                                   
*        ERROR MESSAGE                                                          
         MVC   MIXTXT(L'MIXMSG),MIXMSG                                          
         GOTO1 VSMTP,DMCB,('SMTPAPTL',MIXTXT)                                   
*        BLANK LINE                                                             
         MVC   MIXTXT,SPACES                                                    
         GOTO1 VSMTP,DMCB,('SMTPAPTL',MIXTXT)                                   
                                                                                
*        PRINT TABLE OF ERROR ACCOUNTS                                          
         L     R3,=A(MIXERRT)                                                   
         LA    R2,MXERRENT                                                      
EMIX020  OC    0(12,R3),0(R3)                                                   
         BZ    EMIX050                                                          
         MVC   MIXTXT,SPACES                                                    
         MVC   MIXTXT+2(2),QUNIT                                                
         MVC   MIXTXT+4(12),0(R3)                                               
         GOTO1 VSMTP,DMCB,('SMTPAPTL',MIXTXT)                                   
         LA    R3,12(R3)                                                        
         BCT   R2,EMIX020                                                       
                                                                                
EMIX050  GOTO1 VSMTP,DMCB,('SMTPASND',0)        SEND EMAIL                      
         GOTO1 VSMTP,DMCB,('SMTPAEND',0)        DETACH FROM JESMAIL             
         XIT1                                                                   
                                                                                
         LTORG                                                                  
                                                                                
*                                                                               
MIXLIST  DC    C'US-ACC_TEAM_NY,INT_PROD:'                                      
MIXLISTT DC    C'MNASCA@MEDIAOCEAN.COM:'                                        
*                                                                               
EMLMXSUB DC    C'** URGENT/PLEASE READ ** MIX OF EFT AND EDI820 WITHIN X        
               AC55XXXX CHECK RUN'                                              
                                                                                
* DSFTK-150                                                                     
EMLMXSP1 DC    C'** URGENT/PLEASE READ ** MIX OF PCARD / EDI820 WITHIN X        
               AC55XXXX CHECK RUN'                                              
*                                                                               
EMLMXSP2 DC    C'** URGENT/PLEASE READ ** MIX OF EFT AND CARD  WITHIN  X        
               AC55XXXX CHECK RUN'                                              
* DSFTK-150                                                                     
EMLMXSP3 DC    C'** URGENT/PLEASE READ ** MIX OF CCARD / EDI820 WITHIN X        
               AC55XXXX CHECK RUN'                                              
*                                                                               
                                                                                
IDIMLINE DC    C'REQUEST ID = XXXXXXX'                                          
JOBMLNE  DC    C'JOB = XXXXXXXX'                                                
RQSMLINE DC    C'REQUESTOR = XXXXXXXXXXXX'                                      
DATEMLNE DC    C'DATE = MM/DD/YY'                                               
LDGMLNE  DC    C'LEDGER = XX'                                                   
MIXMSG   DC    C'THE FOLLOWING EFT ACCOUNTS WERE NOT PROCESSED :'               
MIXTXT   DC    CL80' '                                                          
         EJECT                                                                  
***********************************************************************         
* SEND EMAIL WITH LIST OF EFT ACCTS NOT PROCESSED DUE TO MISSING SETUP*         
***********************************************************************         
         USING AC55D,RC                                                         
         USING ACWORKD,RA                                                       
EMLMEFT  NTR1  BASE=*,LABEL=*                                                   
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
                                                                                
         GOTO1 VSMTP,DMCB,('SMTPAINI',0)        INITIALIZE                      
                                                                                
*        WORK=EMAIL ADDRESSES : TEXT=SUBJECT LINE                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'EFTLIST),EFTLIST                                          
*                                                                               
         TM    RNSW2,RUNTST                                                     
         BNO   *+10                                                             
         MVC   WORK(L'EFTLISTT),EFTLISTT                                        
*                                                                               
         MVC   EFTTXT,SPACES                                                    
         MVC   EFTTXT(L'EMLEFSUB),EMLEFSUB                                      
* DSFTK-150                                                                     
         TM    MIXERRM,MIXMISP                                                  
         BZ    *+10                                                             
         MVC   EFTTXT(L'EMLPCSUB),EMLPCSUB                                      
* DSFTK-150                                                                     
* SPEC-28554                                                                    
         TM    MIXERRM,MIXMISC                                                  
         BZ    *+10                                                             
         MVC   EFTTXT(L'EMLCCSUB),EMLCCSUB                                      
* SPEC-28554                                                                    
         MVC   EFTTXT+54(8),RCJOB                                               
         GOTO1 VSMTP,DMCB,('SMTPAPRS',WORK),(L'EFTTXT,EFTTXT)                   
                                                                                
*        BODY OF EMAIL                                                          
*        REQUEST ID                                                             
         MVC   EFTTXT,SPACES                                                    
         MVC   EFTTXT(L'IDIMLIN),IDIMLIN                                        
         MVC   EFTTXT+13(L'IDABBR),IDABBR                                       
         GOTO1 VSMTP,DMCB,('SMTPAPTL',EFTTXT)                                   
*        JOBNAME                                                                
         MVC   EFTTXT,SPACES                                                    
         MVC   EFTTXT(L'JOBMLN),JOBMLN                                          
         MVC   EFTTXT+6(L'RCJOB),RCJOB                                          
         GOTO1 VSMTP,DMCB,('SMTPAPTL',EFTTXT)                                   
*        REQUESTOR FROM REQUEST CARD                                            
         MVC   EFTTXT,SPACES                                                    
         MVC   EFTTXT(L'RQSMLIN),RQSMLIN                                        
         MVC   EFTTXT+12(L'QUESTOR),QUESTOR                                     
         GOTO1 VSMTP,DMCB,('SMTPAPTL',EFTTXT)                                   
*        RUN DATE                                                               
         MVC   EFTTXT,SPACES                                                    
         MVC   EFTTXT(L'DATEMLN),DATEMLN                                        
         MVC   EFTTXT+7(L'RCDATE),RCDATE                                        
         GOTO1 VSMTP,DMCB,('SMTPAPTL',EFTTXT)                                   
*        UNIT/LEDGER                                                            
         MVC   EFTTXT,SPACES                                                    
         MVC   EFTTXT(L'LDGMLN),LDGMLN                                          
         MVC   EFTTXT+9(2),QUNIT                                                
         GOTO1 VSMTP,DMCB,('SMTPAPTL',EFTTXT)                                   
*        BLANK LINE                                                             
         MVC   EFTTXT,SPACES                                                    
         GOTO1 VSMTP,DMCB,('SMTPAPTL',EFTTXT)                                   
*        ERROR MESSAGE                                                          
         MVC   EFTTXT(L'EFTMSG),EFTMSG                                          
* DSFTK-150                                                                     
         TM    MIXERRM,MIXMISP                                                  
         BZ    *+10                                                             
         MVC   EFTTXT(L'PCDMSG),PCDMSG                                          
* DSFTK-150                                                                     
         GOTO1 VSMTP,DMCB,('SMTPAPTL',EFTTXT)                                   
*        BLANK LINE                                                             
         MVC   EFTTXT,SPACES                                                    
         GOTO1 VSMTP,DMCB,('SMTPAPTL',EFTTXT)                                   
                                                                                
*        PRINT TABLE OF ERROR ACCOUNTS                                          
         L     R3,=A(MIXERRT)                                                   
         LA    R2,MXERRENT                                                      
EEFT020  OC    0(12,R3),0(R3)                                                   
         BZ    EEFT050                                                          
         MVC   EFTTXT,SPACES                                                    
         MVC   EFTTXT+2(2),QUNIT                                                
         MVC   EFTTXT+4(12),0(R3)                                               
         GOTO1 VSMTP,DMCB,('SMTPAPTL',EFTTXT)                                   
         LA    R3,12(R3)                                                        
         BCT   R2,EEFT020                                                       
                                                                                
EEFT050  GOTO1 VSMTP,DMCB,('SMTPASND',0)        SEND EMAIL                      
         GOTO1 VSMTP,DMCB,('SMTPAEND',0)        DETACH FROM JESMAIL             
         XIT1                                                                   
                                                                                
         LTORG                                                                  
                                                                                
EFTLIST  DC    C'US-ACC_TEAM_NY,INT_PROD:'                                      
EFTLISTT DC    C'MNASCA@MEDIAOCEAN.COM:'                                        
                                                                                
EMLEFSUB DC    C'** URGENT/PLEASE READ ** MISSING EFT BANK RECORD ON   X        
               AC55XXXX CHECK RUN'                                              
                                                                                
EMLPCSUB DC    C'** URGENT/PLEASE READ ** MISSING PCARD BANK RECD ON   X        
               AC55XXXX CHECK RUN'                                              
                                                                                
EMLCCSUB DC    C'** URGENT/PLEASE READ ** MISSING CCARD BANK RECD ON   X        
               AC55XXXX CHECK RUN'                                              
                                                                                
IDIMLIN  DC    C'REQUEST ID = XXXXXXX'                                          
JOBMLN   DC    C'JOB = XXXXXXXX'                                                
RQSMLIN  DC    C'REQUESTOR = XXXXXXXXXXXX'                                      
DATEMLN  DC    C'DATE = MM/DD/YY'                                               
LDGMLN   DC    C'LEDGER = XX'                                                   
EFTMSG   DC    C'THE FOLLOWING EFT ACCOUNTS WERE NOT PROCESSED :'               
* DSFTK-150                                                                     
PCDMSG   DC    C'THE FOLLOWING PCARD ACCNTS WERE NOT PROCESSED :'               
* DSFTK-150                                                                     
EFTTXT   DC    CL80' '                                                          
         EJECT                                                                  
***********************************************************************         
* PRINT RUN TOTALS OF EFT AND PRINTED CHECKS                          *         
***********************************************************************         
         USING AC55D,RC                                                         
         USING ACWORKD,RA                                                       
RUNTOTS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,DICO           NEED ADDRESSABILITY TO THE DDICT ENTRY         
         AHI   RE,AC@ASYS-DICO                                                  
         MVC   P+TOTCHAT(L'AC@ASYS),0(RE)     'ACCOUNT SYSTEM'                  
         MVC   P+TOTCHAT+18(2),HALF SET SYSTEM NUMBER                           
         BAS   RE,PRTLNE                                                        
*                                                                               
         MVI   SPACING,2                                                        
         LA    RE,DICO           NEED ADDRESSABILITY TO THE DDICT ENTRY         
         AHI   RE,AC@TCSH-DICO                                                  
         MVC   P+TOTCHAT(L'AC@TCSH),0(RE)     'TOTAL CASH'                      
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
         AP    CHOKS,EFTCK#           ADD EFT TO PRINTED TOTAL                  
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
         CP    EFTTOT,=P'0'        ANY EFT CHECKS IN RUN?                       
         BZ    RUNTX                                                            
* PRINTED CHECK SUMMARY PORTION                                                 
         BAS   RE,PRTLNE           SKIP A LINE                                  
         BAS   RE,PRTLNE           SKIP A LINE                                  
         MVC   P+TOTCHAT(L'TXT11),TXT11     'TOTALS FOR PRINTED'                
         BAS   RE,PRTLNE                                                        
         MVI   SPACING,2                                                        
         LA    RE,DICO           NEED ADDRESSABILITY TO THE DDICT ENTRY         
         AHI   RE,AC@TCSH-DICO                                                  
         MVC   P+TOTCHAT(L'AC@TCSH),0(RE)     'TOTAL CASH' (FOR PRNTED)         
         LA    R2,P+TOTFIGS                                                     
         LA    R2,2(R2)                                                         
         ZAP   DUB,RUNTOT                                                       
         SP    DUB,EFTTOT                                                       
         CURED DUB,(14,(R2)),2,COMMAS=YES                                       
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
         MVC   P+TOTCHAT(L'TXT12),TXT12 TOTAL PRINTED CHECKS                    
         LA    R2,P+TOTFIGS2                                                    
         LA    R2,2(R2)                                                         
         SP    CHUSED,EFTCK#      SUBTRACT EFT CHECKS FROM TOTAL CHKS           
         EDIT  CHUSED,(5,(R2))                                                  
         BAS   RE,PRTLNE                                                        
*                                                                               
* EFT SUMMARY PORTION                                                           
         BAS   RE,PRTLNE           SKIP A LINE                                  
         BAS   RE,PRTLNE           SKIP A LINE                                  
         MVC   P+TOTCHAT(L'TXT9),TXT9      'TOTALS FOR EFT'                     
         BAS   RE,PRTLNE                                                        
*                                                                               
         MVI   SPACING,2                                                        
         LA    RE,DICO           NEED ADDRESSABILITY TO THE DDICT ENTRY         
         AHI   RE,AC@TCSH-DICO                                                  
         MVC   P+TOTCHAT(L'AC@TCSH),0(RE)     'TOTAL CASH' (FOR EFT)            
         LA    R2,P+TOTFIGS                                                     
         LA    R2,2(R2)                                                         
         CURED EFTTOT,(14,(R2)),2,COMMAS=YES                                    
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
         MVC   P+TOTCHAT(L'TXT10),TXT10 TOTAL EFT CHECKS                        
         LA    R2,P+TOTFIGS2                                                    
         LA    R2,2(R2)                                                         
         EDIT  EFTCK#,(5,(R2))                                                  
         BAS   RE,PRTLNE                                                        
*                                                                               
RUNTX    XIT1                                                                   
         LTORG                                                                  
***********************************************************************         
* PRINT COMMENTS                                                      *         
***********************************************************************         
         USING AC55D,RC                                                         
         USING ACWORKD,RA                                                       
PRN      NTR1  BASE=*,LABEL=*                                                   
         LTR   R4,R4                                                            
         BNP   PRNXIT                                                           
         LA    R2,PBLK                                                          
PRN3     MVC   P,0(R2)                                                          
         BAS   RE,PRTLNE                                                        
         LA    R2,132(R2)                                                       
         BCT   R4,PRN3                                                          
PRNXIT   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SET DATA FROM OTHER ELEMENTS                                        *         
***********************************************************************         
         USING AC55D,RC                                                         
         USING ACWORKD,RA                                                       
         USING TRNRECD,R3                                                       
SETL     NTR1  BASE=*,LABEL=*                                                   
         XC    SROTH(SROTHL),SROTH    CLEAR OTHER ELEMENT DATA                  
         XC    SRXPY(SRXPYL),SRXPY    EXTRA DATA                                
         XC    SRMTD(SRMTDL),SRMTD    AND MEDIA TRANSFER                        
         XC    SRXMOS,SRXMOS          CLEAR MONTH OF SERVICE                    
*SPEC-35871                                                                     
         XC    SRTCURR,SRTCURR        CLEAR CURRENCY FROM TRASNACTION           
*SPEC-35871                                                                     
         MVC   SRXAC,SPACES           EXPENSE ACCOUNT                           
         MVC   SRPLINV,SPACES         SUPPLIER LONG INVOICE NUMBER              
         MVC   SRTRURF,SPACES         TRUE REFERENCE                            
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
                                                                                
         CLC   OTHNUM+6(3),SPACES  IF SPACES THERE IS NO JOB CODE               
         BE    *+10                (CAUSING TRANSMISSION FAILURE)               
                                                                                
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
         MVC   SVMEDIA,XPYSMED     MEDIA CODE FROM CLEARANCES                   
         B     SETL3                                                            
         DROP  R4                                                               
*                                                                               
SETL7    CLI   0(R4),X'1A'         MEDIA TRANSFER                               
         BNE   SETL8                                                            
         USING MDTELD,R4                                                        
         MVC   SVMEDIA,MDTMED       SAVE MEDIA                                  
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
         SHI   R1,5                SKIP CODE/LEN/UL                             
         BM    SETL3                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRXAC(0),SPDACCS+2                                               
         B     SETL3                                                            
         DROP  R3,R4                                                            
                                                                                
SETL11   CLI   0(R4),X'60'         TRANSACTION STATUS ELEMENT                   
         BNE   SETL12                                                           
         USING TRSELD,R4                                                        
         MVC   SRXMOS,TRSPMOS                                                   
         B     SETL3                                                            
         DROP  R4                                                               
                                                                                
         USING FFTELD,R4                                                        
SETL12   CLI   0(R4),FFTELQ        X'DB' FREE FORM TEXT ELEMENT                 
         BNE   SETL3                                                            
         CLI   FFTTYPE,FFTTINVN    44 - SUPPLIER INVOICE NUMBER                 
         BE    SETL20                                                           
         CLI   FFTTYPE,FFTTKREF    38 - KEY REFERENCE NUMBER                    
         BE    SETL30                                                           
*SPEC-35871                                                                     
         CLI   FFTTYPE,FFTTACUR    243 - ASSOCIATED CURRENCY                    
         BE    SETL40                                                           
*SPEC-35871                                                                     
         B     SETL3                                                            
SETL20   SR    R1,R1                                                            
         ICM   R1,1,FFTDLEN        DATA LENGTH                                  
         BZ    SETL3                                                            
         CHI   R1,L'SRPLINV        BIGGER THAN FIELD CAN HANDLE?                
         BH    SETL3               HANDLE                                       
         BCTR  R1,0                DECREMENT FOR EX MVC                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRPLINV(0),FFTDATA                                               
         B     SETL3                                                            
*                                                                               
SETL30   ICM   R1,1,FFTDLEN        DATA LENGTH                                  
         BZ    SETL3                                                            
         BCTR  R1,0                                                             
         EXMVC R1,SRTRURF,FFTDATA                                               
         B     SETL3                                                            
*                                                                               
*SPEC-35871                                                                     
SETL40   ICM   R1,1,FFTDLEN        DATA LENGTH                                  
         BZ    SETL3                                                            
         BCTR  R1,0                                                             
         EXMVC R1,SRTCURR,FFTDATA                                               
         B     SETL3                                                            
         DROP  R4                                                               
*                                                                               
*SPEC-35871                                                                     
SETLX    MVC   SRADV,SRMADV        SET MOS FROM 1A ELEMENT                      
         OC    SRADV,SRADV         ADVERTISING MONTH FROM 1A ELEMENT            
         BNZ   *+10                                                             
         MVC   SRADV,SROADV        IF NONE, USE OTHER MOS                       
SETLXIT  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* GET PROFILE FOR CLIENT/LEDGER/COMPANY                               *         
***********************************************************************         
         USING AC55D,RC                                                         
         USING ACWORKD,RA                                                       
CLPF     NTR1  BASE=*,LABEL=*                                                   
         USING PRFD,R5                                                          
         L     R5,APROF             A(PROFILE TABLE)                            
*                                                                               
CLPF03   ST    R5,ACLIPRO           A(CLIENT PROFILE)                           
CLPF05   LA    R5,L'PRFLEN(R5)                                                  
         CLI   PRFCOMP,X'FF'                                                    
         BE    CLPFXIT              END OF TABLE                                
         CLC   PRFCOMP(3),QCOMPANY                                              
         BNE   CLPFXIT                                                          
         OC    PRFCLI,PRFCLI                                                    
         BZ    CLPF03               UNIT/LEDGER PROFILE                         
         CLC   PRFCLI,CLNT                                                      
         BNE   CLPF05                                                           
         ST    R5,ACLIPRO                                                       
CLPFXIT  XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET TRANSACTION CASH DISCOUNT                                       *         
***********************************************************************         
         USING AC55D,RC                                                         
         USING ACWORKD,RA                                                       
         USING TRNRECD,R3                                                       
GETCD    NTR1  BASE=*,LABEL=*                                                   
         ZAP   CD,=P'0'            C.D.                                         
         SR    R0,R0                                                            
         LA    R4,TRNKEY+ACCORFST                                               
*                                                                               
GETCD3   IC    R0,1(R4)            BUMP THRU RECORD                             
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    GTCDXIT                                                          
*                                                                               
GETCD5   CLI   0(R4),X'46'         EXTRA PAY ELEMENT                            
         BNE   GETCD7                                                           
         USING XPYELD,R4                                                        
         ZAP   CD,XPYCD                                                         
         B     GTCDXIT                                                          
         DROP  R4                                                               
*                                                                               
GETCD7   CLI   0(R4),X'50'         SUBSIDIARY CASH                              
         BNE   GETCD3                                                           
         USING SCIELD,R4                                                        
         CLI   SCITYPE,C'D'        IS IT CASH DISCOUNT                          
         BNE   GETCD3                                                           
         ZAP   CD,SCIAMNT                                                       
GTCDXIT  XIT1                                                                   
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD THE ADDRESS TO THE SORT RECORD                                  *         
*     R1=ADDRESS OF ADDRESS ELEMENT                                   *         
***********************************************************************         
         USING AC55D,RC                                                         
         USING ACWORKD,RA                                                       
ADDR     NTR1  BASE=*,LABEL=*                                                   
         XC    SRADDR(4*L'SRADDR),SRADDR                                        
         ICM   R2,15,0(R1)         A(ADDRESS ELEMENT)                           
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
         B     ADDRXIT                                                          
*                                                                               
ADDR09   AP    NOADD,=P'1'         NO ADDRESS                                   
ADDRXIT  XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* OPEN THE WORKER FILE                                                *         
***********************************************************************         
         USING AC55D,RC                                                         
         USING ACWORKD,RA                                                       
OPNWK    NTR1  BASE=*,LABEL=*                                                   
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
         CLI   QOPT6,C'D'          ALLOW DUPLICATES FOR TESTING?                
         BNE   *+8                                                              
         MVI   UKFLAG,X'01'                                                     
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
         BNO   OPNXIT                                                           
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
OPNXIT   XIT1                                                                   
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* WRITE FACWK RECORDS FOR SOON UPDATES                                *         
***********************************************************************         
FACOUT   NTR1  BASE=*,LABEL=*                                                   
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
         BE    FACOUTX                                                          
         TM    RNSW,MICR+SOON      TEST MICR - SOON                             
         BNO   FACOUTX                                                          
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
FACOUTX  XIT1                                                                   
         DROP  R3,R4,RF                                                         
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
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
         AHI   R1,1                GET THE NEXT SUBREFERNCE                     
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
         XC    MPYELD(MPYLN4Q),MPYELD                                           
         MVI   MPYEL,MPYELQ                                                     
         MVI   MPYLN,MPYLN4Q                                                    
         MVC   MPYNO,SPACES                                                     
         ZAP   MPYAMNT,=P'0'                                                    
         MVC   MPYBNK,SPACES                                                    
         MVC   MPYSUB,SRTSBR                                                    
         ZAP   MPYPART,=P'0'                                                    
         ZAP   MPYPARC,=P'0'                                                    
         TM    RNSW,SOON           TEST SOON CHECKS                             
         BZ    *+8                                                              
         OI    MPYBYTE,MPYSOON     SET BIT IN 64 ELEM                           
         CLI   SRVTYP,VEFT         IS THIS VENDOR EFT?                          
         BNE   *+8                                                              
         OI    MPYBYTE,MPYEFT      SET BIT IN 64 ELEM                           
* DSFTK-150                                                                     
         CLI   SRVTYP,VPCARD        IS THIS VENDOR PCARD?                       
         BNE   *+8                                                              
         OI    MPYBYTE,MPYPCRD     SET BIT IN 64 ELEM                           
* DSFTK-150                                                                     
         CLI   SRVTYP,VCCARD        IS THIS VENDOR CCARD?                       
         BNE   *+8                                                              
         OI    MPYBYTE,MPYCCRD     SET BIT IN 64 ELEM                           
         MVI   MPYLN4Q(RE),0       SET END OF RECORD                            
         SR    R0,R0                                                            
         IC    R0,MPYLN                                                         
         AR    RE,R0                                                            
         DROP  RE                                                               
                                                                                
         USING FFTELD,RE           ADD REFERENCE NUMBER ELEMENT                 
         XC    FFTELD(FFTDATA-FFTELD+L'TRNREF+1),FFTELD                         
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTDATA-FFTELD+L'TRNREF                                    
         MVI   FFTTYPE,FFTTKREF                                                 
         MVI   FFTDLEN,L'TRNREF                                                 
         MVC   FFTDATA(L'TRNREF),TRNREF                                         
*                                                                               
         CLC   SRTRURF,SPACES     NEED TO CARRY THE ORIGINAL REFERENCE          
         BNH   *+10               IN THE CHECK FILE DSSUP-3770                  
         MVC   FFTDATA(L'SRTRURF),SRTRURF                                       
*                                                                               
         SR    R0,R0                                                            
         IC    R0,FFTLN                                                         
         AR    RE,R0                                                            
         USING FFTELD,RE           ADD CR KEY REFRENCE NUMBER                   
         XC    FFTELD(FFTDATA-FFTELD+L'SRTREF+1),FFTELD                         
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTDATA-FFTELD+L'SRTREF                                    
         MVI   FFTTYPE,FFTTOREF      FFTTOREF   ORIGINAL REFRENCE#              
         MVI   FFTDLEN,L'SRTREF                                                 
         MVC   FFTDATA(L'SRTREF),SRTREF                                         
*                                                                               
         CLC   SRXINV,SPACES                                                    
         BNH   POST6                                                            
         SR    R0,R0                                                            
         IC    R0,FFTLN                                                         
         AR    RE,R0                                                            
                                                                                
         XC    FFTELD(FFTDATA-FFTELD+L'TRNREF+1),FFTELD                         
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTDATA-FFTELD+L'SRXINV                                    
         MVI   FFTTYPE,FFTTINVN                                                 
         MVI   FFTDLEN,L'SRXINV                                                 
         MVC   FFTDATA(L'SRXINV),SRXINV                                         
*                                                                               
POST6    DS    0H                                                               
         CLC   SRXEST,=XL2'00'       ESTIMATE NUMBER                            
         BE    POST7                                                            
         SR    R0,R0                                                            
         IC    R0,FFTLN                                                         
         AR    RE,R0                                                            
                                                                                
         XC    FFTEL(FFTESLNQ),FFTEL                                            
         MVI   FFTEL,FFTELQ          X'DB'                                      
         LA    R1,FFTESLNQ           ESTIMATE ELEMENT LENGTH                    
         STC   R1,FFTLN              STORE LENGTH OF ELEMENT                    
         MVI   FFTDLEN,12            LENGTH OF TEXT                             
         MVI   FFTTYPE,FFTTESTN      ESTIMATE NUMBER (122)                      
         MVI   FFTSEQ,0              ELEMENT SEQUENCE NUMBER                    
         MVC   FFTCESTN,SPACES       INIT AS SPACES                             
         MVC   FFTOESTN,SPACES                                                  
*                                                                               
         EDIT  SRXEST,(3,WORK),ALIGN=LEFT                                       
         MVC   FFTOESTN(3),WORK              MOVE IN EST # IN CHAR              
*                                                                               
*SPEC-35871                                                                     
POST7    CLC   SRTCURR,SPACES      CURRENCY                                     
         BNH   POST8                                                            
         SR    R0,R0                                                            
         IC    R0,FFTLN                                                         
         AR    RE,R0                                                            
                                                                                
         XC    FFTELD(FFTDATA-FFTELD+L'SRTCURR+1),FFTELD                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTDATA-FFTELD+L'SRTCURR                                   
         MVI   FFTTYPE,FFTTACUR      CURRENCY PASSED FROM CLEARANCES            
         MVI   FFTDLEN,L'SRTCURR                                                
         MVC   FFTDATA(L'SRTCURR),SRTCURR                                       
*                                                                               
*SPEC-35871                                                                     
POST8    OC    SRTMMOS,SRTMMOS                                                  
         BZ    POST9                                                            
         SR    R0,R0                                                            
         IC    R0,FFTLN                                                         
         AR    RE,R0                                                            
                                                                                
         DROP  RE                                                               
         USING GDAELD,RE           BUILD MANUAL CHECK ELEMENT                   
         XC    GDAELD(GDALNQ),GDAELD                                            
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDAMMOS                                                  
         MVC   GDADATE,SRTMMOS                                                  
         MVI   GDALNQ(RE),0        SET END OF RECORD                            
         SR    R0,R0                                                            
         IC    R0,GDALN                                                         
         AR    RE,R0                                                            
*                                                                               
POST9    BAS   RE,PWK              ADD RECORD TO WORKER FILE                    
         AP    WRKAMT,TRNAMNT                                                   
         B     POSTX                                                            
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD THE BANK POSTING - HANDLE THE VOIDS                           *         
***********************************************************************         
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
         MVI   CEDLN,CEDLN2Q                                                    
         MVC   CED#INV,SR#INV                                                   
         MVI   CEDBYTE,0                                                        
         TM    RNSW,SOON           TEST SOON CHECKS                             
         BZ    *+8                                                              
         OI    CEDBYTE,CEDSOON     SET BIT                                      
* DSFTK-150                                                                     
         CLI   SRVTYP,VPCARD       IS THIS VENDOR PCARD?                        
         BNE   *+8                                                              
         OI    CEDBYTE,CEDPCRD     SET BIT                                      
* DSFTK-150                                                                     
         CLI   SRVTYP,VCCARD       IS THIS VENDOR CCARD?                        
         BNE   *+8                                                              
         OI    CEDBYTE,CEDCCRD     SET BIT                                      
         CLI   SRVTYP,VEFT         IS THIS VENDOR EFT?                          
         BNE   *+8                                                              
         OI    CEDBYTE,CEDEFT      SET BIT                                      
         SR    R1,R1                                                            
         IC    R1,CEDLN                                                         
         AR    R4,R1                                                            
         MVI   0(R4),0                                                          
*SPEC-35871                                                                     
         DROP  R4                                                               
*                                                                               
         CLC   SRTCURR,SPACES      CURRENCY                                     
         BNH   POST23                                                           
         USING FFTELD,R4                                                        
         XC    FFTELD(FFTDATA-FFTELD+L'SRTCURR+1),FFTELD                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTDATA-FFTELD+L'SRTCURR                                   
         MVI   FFTTYPE,FFTTACUR      CURRENCY PASSED FROM CLEARANCES            
         MVI   FFTDLEN,L'SRTCURR                                                
         MVC   FFTDATA(L'SRTCURR),SRTCURR                                       
         SR    R1,R1                                                            
         IC    R1,FFTLN                                                         
         AR    R4,R1                                                            
         MVI   0(R4),0                                                          
*                                                                               
*SPEC-35871                                                                     
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
*                                                                               
         TM    RNSW,SOON           PUT STARTING CHECK NUMBER IN                 
         BZ    POST27              WORKER FILE TO AVOID BUFFER                  
         TM    RNSW,MICR           PROBLEM WHEN DOING SOON/MICR                 
         BZ    POST27              OR SOON/LASER CHECKS                         
         UNPK  CRDDATA(6),CHNUM(4)                                              
*                                                                               
POST27   BAS   RE,PWK              ADD TO WORKER FILE                           
         MVC   CRDDATA,SPACES      RESET DATA TO SPACES                         
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
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
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
         XC    WORK,WORK                                                        
         MVC   WORK(3),=C'A56'                                                  
         MVC   WORK+3(2),ORIGINUM                                               
         GOTO1 PQPROF,DMCB,(X'80',WORK),(0,REMOTEC),ADCOMFAC                    
*        CLI   DMCB+8,0                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
         MVC   REMOTPRG,=C'56'                                                  
         MVC   REMOTFRM,=C'1S  '                                                
         MVI   REMOTCLS,C'Q'                                                    
*        MVC   REMOTJID(3),=C'A56'                                              
                                                                                
*DSFTK-195                                                                      
         TM    DTRSW,DTR820                                                     
         BO    *+12                                                             
*DSFTK-195                                                                      
         TM    PRNTSW,WEBWSP                                                    
         BZ    RREG050                                                          
         TM    RNSW,SOON           ONLY FOR SOON CHECKS                         
         BZ    RREG050                                                          
         MVI   REMOTSYS,C'A'                                                    
         MVC   REMOTDST,ORIGINUM                                                
         MVC   REMOTJID(3),=C'A56'                                              
                                                                                
RREG050  MVC   SVREMPQK,MCREMPQK                                                
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
*        MVI   DUB+6,C'A'                ** USE 'A' VERSION OF A56 **           
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)                  STORE ADDRESS OF OVERLAY               
*                                                                               
         MVC   PAGE,=H'1'          SET TO PAGE 1 FOR REGISTER                   
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
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PRINT THE LINE BETWEEN THE REMITTANCE ADVICE AND CHECK              *         
***********************************************************************         
DLN      DS    0D                                                               
         NMOD1 0,**DLN**                                                        
         L     RC,=A(LWS)                                                       
         LR    R3,R1                                                            
         MVI   BYTE,C'P'           PRINT DEADLINE                               
         LTR   R3,R3                                                            
         BZ    *+12                                                             
         MVI   BYTE,C'S'           SAVE DEADLINE                                
         B     DLN01                                                            
*                                                                               
         ZAP   DUB,DLNLNE          NO.OF LINES IN REMITTANCE ADVICE             
         SP    DUB,CURLNE          MAX - NO PRINTED                             
         UNPK  SKIP+1(3),DUB+6(2)  SKIP DOWN TO CORRECT PLACE                   
         OI    SKIP+3,X'F0'        IN BODY OF REPORT                            
         MVC   SKIP+0(2),=C'BL'                                                 
         GOTO1 PRINT,DMCB,P,SKIP                                                
*                                                                               
DLN01    MVC   P,SPACES                                                         
         MVC   P+01(7),IDABBR                                                   
         ZAP   FULL,CHNUM          ADD 1 IF BYTE IS SET TO S                    
         CLI   BYTE,C'S'           ARE WE SAVING DEADLINE                       
         BNE   *+10                                                             
         AP    FULL,=P'1'                                                       
         EDIT  (P4,FULL),(6,P+21),FILL=0                                        
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
DLN11    CLI   BYTE,C'S'           ARE WE SAVING DEADLINE                       
         BE    *+14                IF YES-THEN THERE ARENT LINEUPS              
         CP    RUNTOT,=P'0'        LINEUP CHEQUE                                
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
         GOTO1 PUBEDIT,DMCB,(8,DUB),(C'S',P+70)                                 
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
         ST    R5,FULL                                                          
         BRAS  RE,FNDNXT               R5 TO NEXT SPACE                         
         L     R5,FULL                                                          
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
         ST    R5,FULL                                                          
         BRAS  RE,FNDNXT              R5= NEXT SPACE                            
         L     R5,FULL                                                          
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
DLN05    CLC   ALPHAID,MCCANN         SPECIAL FOR MCCANN                        
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
         ST    R5,FULL                                                          
         BRAS  RE,FNDNXT            R5=NEXT SPACE                               
         L     R5,FULL                                                          
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
         ST    R5,FULL                                                          
         BRAS  RE,FNDNXT           R5 TO NEXT SPACE                             
         L     R5,FULL                                                          
         MVI   0(R5),C'='                                                       
         MVC   1(2,R5),ALPHAID                                                  
         LA    R5,4(R5)                                                         
         MVC   0(L'AC@LGR,R5),AC@LGR                                            
         LA    R5,L'AC@LGR-1(R5)   LEDGER=XX                                    
         ST    R5,FULL                                                          
         BRAS  RE,FNDNXT           R5 TO NEXT SPACE                             
         L     R5,FULL                                                          
         MVI   0(R5),C'='                                                       
         L     R2,ADLEDGER                                                      
         MVC   1(2,R5),1(R2)                                                    
         LA    R5,4(R5)                                                         
*&&                                                                             
DLN15    GOTO1 ADSQUASH,DMCB,P+1,110                                            
         CLI   BYTE,C'P'           DO WE WANT TO PRINT DEADLINE                 
         BNE   DLNX                                                             
         GOTO1 PRINT,DMCB,P,=C'BL01'                                            
         MVC   P,SPACES                                                         
DLNX     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CREATE AN DATA FILE:                                                *         
* MINDSHARE EDI820 FILE                                               *         
* O&M-WACHOVIA EDI820 FILE                                            *         
* EFT FILE                                                            *         
* FLATFILE                                                            *         
***********************************************************************         
         USING EDTTABD,R1                                                       
EDTRN    DS    0D                                                               
         NMOD1 0,*EDTR*,R8                                                      
**                                                                              
** NOTE:  R9 IS BEING USED AS EDIWRK AREA REGISTER SO DON'T USE IT              
**        IN THIS NMOD                                                          
**                                                                              
         L     RC,=A(LWS)                                                       
         CLI   MODE,RUNLAST        NEED TO PROCESS RUNLAST FOR ALL              
         BNE   EDTRN40             EDI TYPES & MAY HAVE MORE THAN ONE           
         TM    DTRSW,DTRFLAT       ** FLATFILE **                               
         BZ    EDTRN20                                                          
         CP    TOTRECFF,=P'0'                                                   
         BE    EDTRN20                                                          
         SR    RF,RF                                                            
         ICM   RF,7,=AL3(FLATFILE-EDTRN)                                        
         AR    RF,RB                                                            
         BASR  RE,RF                                                            
EDTRN20  TM    DTRSW,DTREFT        EDI TYPE PER RUN SO NEED TO CHECK            
         BZ    EDTRN30             FOR EACH.                                    
         SR    RF,RF               ** EFT **                                    
         ICM   RF,7,=AL3(FTRRTN-EDTRN)                                          
         AR    RF,RB                                                            
         BASR  RE,RF                                                            
EDTRN30  TM    DTRSW,DTR820        ** EDI820 **                                 
         BZ    EDTRNX                                                           
         SR    RF,RF                                                            
         ICM   RF,7,=AL3(FTRRTN-EDTRN)                                          
         AR    RF,RB                                                            
         BASR  RE,RF                                                            
         B     EDTRNX                                                           
*                                                                               
EDTRN40  CLI   SRVTYP,VEFT         IS THIS AN EFT VENDOR?                       
         BNE   *+14                                                             
         SR    RF,RF                                                            
         ICM   RF,7,=AL3(FTRRTN-EDTRN)   DISPLACEMENT TO OVERLAY                
         B     EDTRN50                                                          
*                                                                               
* DSFTK-150                                                                     
         CLI   SRVTYP,VPCARD       IS THIS AN PCARD VENDOR?                     
         BNE   *+14                                                             
         SR    RF,RF                                                            
         ICM   RF,7,=AL3(FTRRTN-EDTRN)   DISPLACEMENT TO OVERLAY                
         B     EDTRN50                                                          
* DSFTK-150                                                                     
*                                                                               
         CLI   SRVTYP,VCCARD       IS THIS AN CCARD VENDOR?                     
         BNE   *+14                                                             
         SR    RF,RF                                                            
         ICM   RF,7,=AL3(FTRRTN-EDTRN)   DISPLACEMENT TO OVERLAY                
         B     EDTRN50                                                          
*                                                                               
         TM    DTRSW,DTRFLAT       FLATFILE?                                    
         BZ    *+14                                                             
         SR    RF,RF                                                            
         ICM   RF,7,=AL3(FLATFILE-EDTRN)   DISPLACEMENT TO OVERLAY              
         B     EDTRN50                                                          
*                                                                               
         TM    DTRSW,DTR820                                                     
         BZ    EDTRNX                                                           
         SR    RF,RF                                                            
         ICM   RF,7,=AL3(FTRRTN-EDTRN)   DISPLACEMENT TO OVERLAY                
*                                                                               
EDTRN50  AR    RF,RB                                                            
         BASR  RE,RF                                                            
EDTRNX   XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* FILE TRANSFER ROUTINE (USED FOR EFT AND EDI820 FOR O&M-WACHOVIA     *         
***********************************************************************         
         USING EDIDD,R9                                                         
         USING FRMTABD,R4                                                       
FTRRTN   L     RC,=A(LWS)                                                       
         L     R9,AEDIWRK                                                       
         ST    R9,SVWRKREG                                                      
         ST    RE,WORD                                                          
*                                  ** ONCE CFILE FORMAT REC IS READY            
*                                  ** CALL RDFORMC TO GET THE FORMAT            
*                                  ** AND SAVE IT OFF                           
         USING FORMD,RE                                                         
         L     RE,AFRMBLK                                                       
         LA    R4,FFRMFRM                                                       
         DROP  RE                                                               
                                                                                
* 1. READ THE AFM ACCOUNT RECORD(SC) TO GET THE BANK, BRANCH & BANK ACC         
* 2. CALL THE GETBANK MODULE TO GET THE BANKING INFO FROM THE CFILE             
*    BANK REC AND THE AFM BANK REC                                              
* 3. CALL THE GETFORM MODULE TO GET THE FORMAT LAYOUT                           
*                                                                               
         CLI   MODE,ACCFRST                                                     
         BE    FTRACCF                                                          
         CLI   MODE,PROCTRNS                                                    
         BE    FTRPRCT                                                          
         CLI   MODE,ACCLAST                                                     
         BE    FTRACCL                                                          
         CLI   MODE,RUNLAST                                                     
         BE    FTRRUNL                                                          
         B     FTRXIT                                                           
*                                                                               
* ACCOUNT FIRST                                                                 
*                                                                               
FTRACCF  ZAP   PRDPAY,=P'0'        KEEP TRACK OF NET TOTAL BY CHECK             
         ZAP   PRDCD,=P'0'         KEEP TRACK OF CASH DISCOUNT BY CHECK         
*        NI    DTXSW,X'FF'-EDIFAIL                                              
         BRAS  RE,EDIINF                                                        
*        ZAP   EDIISEQ,=P'0'                                                    
         TM    DTXSW,FTRREAD       BANK RECS READ ALREADY?(1ST TIME IN)         
         BO    FTRAF10             NO GO DO IT                                  
         ZAP   PKCNT4,=P'0'                                                     
         ZAP   PKCNT5,=P'0'                                                     
         ZAP   TOTBAMT,=P'0'                                                    
         ZAP   TOTBCNT,=P'0'                                                    
         ZAP   EDIENTH,=P'0'                                                    
         NI    EDIFLAG,X'FF'-EDIACCF                                            
*        BRAS  RE,RDSC             GET BANK INFO FROM SC ACCOUNT                
*        BE    *+12                IF BANK INFO NOT SETUP - EXIT                
*        OI    DTXSW,EDIFAIL                                                    
*        B     FTRXIT                                                           
         BRAS  RE,RDBNKC           GET BANK INFO FROM CFILE BANK REC            
         BRAS  RE,RDFORMC          GET FORMAT INFO FROM CFILE(R4 POINTS         
         OI    DTXSW,FTRREAD       TO FORMAT BLOCK ON EXIT)                     
         BRAS  RE,FTROPEN                                                       
         OI    DTXSW,FILOPN                                                     
         XC    TRNSEQ#,TRNSEQ#          INITIALIZE SEQUENCE #                   
         TIME  DEC,DUB,LINKAGE=SYSTEM   SAVE TIME 1ST TIME IN IN PACKED         
         MVC   EDITIMEP,DUB             FORMAT                                  
         MVI   BYTE,THDR                TRANSMISSION HEADER                     
         TM    FRMSTAT,FRMHTCNT         INCLUDE THDR/TRAILER IN ACCT?           
         BZ    *+10                                                             
         AP    PKCNT5,=P'1'                                                     
         MVC   THREE(L'FRMSTATS),FRMSTATS                                       
* DSFTK-150                                                                     
         TM    SRTSWIPE,VBPSING                                                 
         BZ    *+10                                                             
         MVC   EDIPTYPE,=C'AP001'                                               
         TM    SRTSWIPE,VBPMULT                                                 
         BZ    *+10                                                             
         MVC   EDIPTYPE,=C'AP002'                                               
* DSFTK-150                                                                     
         BRAS  RE,GORUNIT                                                       
FTRAF10  OI    EDIFLAG,EDIACCF          FIRST TIME FOR THIS ACCOUNT             
         GOTOR FTRADDR,EDIRADD1         FORMAT VENDOR (RECIPIENT) ADDR          
         MVC   EDIMAIL,BYTE             SAVE OFF MAIL INDICATOR                 
         MVC   EDIVADDL,WORK         SAVE OFF # OF STREET ADDR LINES            
         BRAS  RE,RDVENDOR              READ VENDOR FOR SOME INFO               
         BRAS  RE,EDIINF                                                        
FTRAF20  DS    0H                                                               
         CLC   EDIFORMT,=CL10'BOM80'  FOR BANK OF MONTREAL NO REMITT            
         BNE   *+14                   INFO SENT TO BANK SO ONLY PROCESS         
         CLC   PRCKDTE,SRCDTE                                                   
         BE    FTRAF40                                                          
         ZAP   BATAMT,=P'0'        CLEAR BATCH AMT ACCUM                        
         ZAP   BATCNT,=P'0'        CLEAR BATCH REC ACCUM                        
         CLC   EDIFORMT,=CL10'BOAVAR' NO HEADER IN THIS FORMAT                  
         BE    FTRAF40                                                          
*                                                                               
FTRAF22  CLI   0(R4),EOF                                                        
         BNE   *+6                                                              
         DC    H'0'                MUST HAVE A HEADER RECORD                    
         CLI   FRMRECN,HDR         IS THIS THE HEADER ENTRY                     
         BE    FTRAF30                                                          
         AHI   R4,FRMLNQ                                                        
         B     FTRAF20                                                          
*                                                                               
FTRAF30  DS    0H                                                               
         MVC   BYTE,FRMRECN        SAVE RUNIT MODE (HDR,DTL,TRL)                
         MVC   THREE(L'FRMSTATS),FRMSTATS                                       
* DSFTK-150                                                                     
         TM    SRTSWIPE,VBPSING                                                 
         BZ    *+10                                                             
         MVC   EDIPTYPE,=C'AP001'                                               
         TM    SRTSWIPE,VBPMULT                                                 
         BZ    *+10                                                             
         MVC   EDIPTYPE,=C'AP002'                                               
* DSFTK-150                                                                     
         BRAS  RE,GORUNIT                                                       
         AP    TOTTRNS,=P'1'                                                    
*                                                                               
FTRAF40  MVC   PRCKDTE,SRCDTE                                                   
         B     FTRXIT                                                           
*                                                                               
* PROCTRNS                                                                      
*                                                                               
FTRPRCT  DS    0H                                                               
         CLC   EDIFORMT,=CL10'BOM80'  FOR BANK OF MONTREAL NO REMITT            
         BNE   *+12                   INFO SENT TO BANK SO ONLY PROCESS         
         TM    EDIFLAG,EDIACCF        FIRST TIME FOR ACCOUNT (C RECORD)         
         BZ    FTRXIT                                                           
*                                                                               
         NI    EDIFLAG,X'FF'-EDIACCF FIRST TIME IS OVER                         
         AP    BATAMT,SRCAMT          ADD CHK AMT TO TTL CASH FOR BATCH         
         AP    TOTBAMT,SRCAMT         TOTAL CASH FOR ALL BATCHES                
         AP    BATCNT,=P'1'           ADD 1 TO RECORD COUNT FOR BATCH           
         AP    TOTBCNT,=P'1'          RECORD COUNT FOR ALL BATCHES              
         AP    PRDPAY,SRTAMT        KEEP TRACK OF NET AMT BY CHECK              
         AP    PRDCD,SRTCD          KEEP TRACK OF C/D BY CHECK                  
         BRAS  RE,EDIINF                                                        
FTRPR10  CLI   0(R4),EOF                                                        
         BE    FTRXIT                                                           
         BNE   *+6                                                              
         DC    H'0'                MUST HAVE A DETAIL RECORD                    
         CLI   FRMRECN,DTL         IS THIS THE DETAIL ENTRY?                    
         BE    FTRPR20                                                          
         AHI   R4,FRMLNQ                                                        
         B     FTRPR10                                                          
*                                                                               
FTRPR20  MVC   BYTE,FRMRECN        SAVE RUNIT MODE (HDR,DTL,TRL)                
         MVC   THREE(L'FRMSTATS),FRMSTATS                                       
* DSFTK-150                                                                     
         TM    SRTSWIPE,VBPSING                                                 
         BZ    *+10                                                             
         MVC   EDIPTYPE,=C'AP001'                                               
         TM    SRTSWIPE,VBPMULT                                                 
         BZ    *+10                                                             
         MVC   EDIPTYPE,=C'AP002'                                               
* DSFTK-150                                                                     
         BRAS  RE,GORUNIT                                                       
         B     FTRXIT                                                           
*                                                                               
* ACCLAST                                                                       
FTRACCL  DS    0H                                                               
FTRACL10 CLI   0(R4),EOF                                                        
         BE    FTRXIT                                                           
         CLC   EDIFORMT,=CL10'BOM80'                                            
         BNE   FTRACL15                                                         
         LA    RE,SREC             R3 = RECORD JUST PROCESSED                   
         AHI   RE,SRCDTE-SREC      POINT TO CHECK DATE                          
         L     RF,ANXTSR           R4 = NEXT RECORD                             
         AHI   RF,SRCDTE-SREC      POINT TO CHECK DATE OF NEXT RECORD           
         TM    SCS,SCSEOF          LAST RECORD                                  
         BO    *+14                THEN ALWAYS PROCESS                          
         CLC   0(L'SRCDTE,RE),0(RF) IF CHECK DATES ARE SAME DON'T PUT           
         BE    FTRXIT               OUT CTRL REC.                               
FTRACL15 CLI   FRMRECN,CTRL        IS THIS THE CHECK TRAILER REC?               
         BE    FTRACL20                                                         
         AHI   R4,FRMLNQ                                                        
         B     FTRACL10                                                         
*                                                                               
FTRACL20 MVC   BYTE,FRMRECN        SAVE RUNIT MODE (HDR,DTL,TRL,ETC.)           
         MVC   THREE(L'FRMSTATS),FRMSTATS                                       
* DSFTK-150                                                                     
         TM    SRTSWIPE,VBPSING                                                 
         BZ    *+10                                                             
         MVC   EDIPTYPE,=C'AP001'                                               
         TM    SRTSWIPE,VBPMULT                                                 
         BZ    *+10                                                             
         MVC   EDIPTYPE,=C'AP002'                                               
* DSFTK-150                                                                     
         BRAS  RE,GORUNIT                                                       
         B     FTRXIT                                                           
*                                                                               
*                                                                               
* RUNLAST                                                                       
FTRRUNL  DS    0H                                                               
         NI    TEST5,ALL-(TRLPRCD+TRL2PRCD)                                     
FTRRL10  TM    DTXSW,FTRREAD       MAKE SURE THIS IS NOT A ZERO EFT RUN         
         BZ    FTRXIT              SINCE IT ALWAYS GOES TO RUNLAST              
         CLI   0(R4),EOF                                                        
         BE    FTRRL80                                                          
         TM    TEST5,TRLPRCD                                                    
         BO    *+12                                                             
         CLI   FRMRECN,TRL         IS THIS THE TRAILER ENTRY?                   
         BE    FTRRL20                                                          
         TM    TEST5,TRL2PRCD                                                   
         BO    *+12                                                             
         CLI   FRMRECN,TRL2                                                     
         BE    FTRRL30                                                          
         CLI   FRMRECN,TRLB        BLOCK TRAILER?                               
         BE    FTRRL40                                                          
FTRRLNX  AHI   R4,FRMLNQ                                                        
         B     FTRRL10                                                          
*                                                                               
FTRRL20  MVC   BYTE,FRMRECN        SAVE RUNIT MODE (HDR,DTL,TRL)                
         MVC   THREE(L'FRMSTATS),FRMSTATS                                       
         BRAS  RE,GORUNIT                                                       
         OI    TEST5,TRLPRCD                                                    
         B     FTRRLNX             NOW LOOK FOR OTHER TRAILERS                  
*                                                                               
FTRRL30  DS    0H                                                               
         ZAP   DUB,TOTRECS         TOTAL # OF RECORDS SO FAR                    
         AP    DUB,=P'1'           ADD ONE FOR THIS ENTRY                       
         DP    DUB,=PL2'10'        DIVIDE BY 10 TO FIND THE # OF BLOCKS         
         CP    DUB(6),=P'0'        CHECK IF ZERO.  IF SO THEN MUST BE           
         BNE   *+14                1 BLOCK                                      
         AP    DUB(6),=P'1'                                                     
         B     FTRRL32                                                          
         CP    DUB+6(2),=P'0'      CHECK FOR A REMAINDER                        
         BE    *+10                IF SO THEN ADD 1 TO THE BLOCK COUNT          
         AP    DUB(6),=P'1'                                                     
FTRRL32  ZAP   EDIBLCNT,DUB(6)                                                  
         MVC   BYTE,FRMRECN        SAVE RUNIT MODE (HDR,DTL,TRL)                
         MVC   THREE(L'FRMSTATS),FRMSTATS                                       
         BRAS  RE,GORUNIT                                                       
         OI    TEST5,TRL2PRCD                                                   
         B     FTRRLNX             NOW LOOK FOR BLOCK TRAILER                   
*                                                                               
FTRRL40  ZAP   DUB,EDIBLCNT        # OF BLOCKS                                  
         ZAP   DOUBLE,TOTRECS      # OF RECORDS                                 
         MP    DUB,=P'10'          MULTIPLY # OF BLOCKS BY 10                   
         SP    DUB,DOUBLE          SUBTRACT THAT BY # OF RECORDS                
         CVB   R1,DUB                                                           
         STH   R1,HALF             HOW MANY FILLER LINES TO PUT                 
         LTR   R1,R1                                                            
         BNP   FTRRL80                                                          
FTRRL45  MVC   BYTE,FRMRECN                                                     
         MVC   THREE(L'FRMSTATS),FRMSTATS                                       
         BRAS  RE,GORUNIT                                                       
         LH    RF,HALF                                                          
         SHI   RF,1                                                             
         STH   RF,HALF                                                          
         BP    FTRRL45                                                          
*                                                                               
FTRRL80  TM    DTXSW,FILOPN        TEST ACTIVITY                                
         BNO   FTRXIT                                                           
* DSFTK-150                                                                     
* NEED TO PUT A TABLE OF FORAMT CODES AND ROUTINE ADDRESSES IN THIS             
* PROGRAM - FORMAT CODE PASSED BACK FROM GETFORM TO POINT TO ROUTINE            
* ADDRESS IN THIS PROGRAM                                                       
*                                                                               
         TM    EDIRSTAT,EDISPCL    SPECIAL ROUTINE?                             
         BZ    FTRRL85                                                          
         CLOSE (CHASEIN)           CLOSE TAPE                                   
         BRAS  RE,CHSCSV           PROCESS CHASE PCARD FORMAT                   
         B     FTRRL100                                                         
* DSFTK-150                                                                     
FTRRL85  TM    EDIRSTAT,EDIVRLN    VARIABLE RECORD FORMAT?                      
         BZ    FTRRL90                                                          
         CLOSE (EDIOUT)            CLOSE EDI TAPE                               
         B     FTRRL100                                                         
*                                                                               
FTRRL90  CLOSE (FTROUT)                                                         
*                                                                               
FTRRL100 DS    0H                                                               
*DSFTK-195                                                                      
         USING MASTD,RF                                                         
         L     RF,ADMASTC          IF RUNNING ON CSC DO NOT WANT TO             
         USING SSOOFF,RE           CREATE AN EDICT HEADER OR MQ                 
         ICM   RE,15,MCSSB         HEADER                                       
         BZ    FTR110                                                           
*                                                                               
         CLI   SSODSPAC,C'C'                                                    
         BE    FTRXIT                                                           
*                                                                               
         CLI   SSODSPAC,C'Q'                                                    
         BE    FTRXIT                                                           
*DSFTK-195                                                                      
*MN FTR110   LA    RF,HDRPQ            PUT OUT EDICT HEADER TO PQ               
FTR110   L     RF,=A(HDRPQ)        PUT OUT EDICT HEADER TO PQ                   
         TM    DTRSW,DTRMQ         ARE WE MQING THIS?                           
         BNO   *+8                                                              
         L     RF,=A(MQRPQ)        PUT OUT MQ HEADER TO PQ                      
         GOTOR (RF)                                                             
*                                                                               
*TRXIT   XIT1                                                                   
FTRXIT   L     RE,WORD             RESTORE ADDRESS OF RE                        
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* EDI CHECKS DATA FILE (DDS VERSION)                                 *          
**********************************************************************          
         USING EDIDD,R9                                                         
FLATFILE L     R9,AEDIWRK                                                       
         ST    R9,SVWRKREG                                                      
         ST    RE,WORD                                                          
*                                                                               
         CP    SRCAMT,=P'0'         NEVER PROCESS NEGATIVE CHECK                
         BNH   FFILEX                                                           
*                                                                               
         CLI   MODE,ACCFRST                                                     
         BE    FFACCF                                                           
         CLI   MODE,PROCTRNS                                                    
         BE    FFPRCT                                                           
         CLI   MODE,RUNLAST                                                     
         BE    FFRUNL                                                           
         B     FFILEX                                                           
*                                                                               
* ACCOUNT FIRST                                                                 
FFACCF   TM    DTXSW,FFLOPN        TEST DLCB INITIALIZED (1ST TIME IN)          
         BO    FFAF10                                                           
         BRAS  RE,INITDLCB                                                      
         OI    DTXSW,FFLOPN                                                     
         GOTO1 DATCON,DMCB,(5,0),(20,EDIFDTE)  FILE CREATION DATE               
         MVC   EDIALPH,ALPHAID                                                  
         MVC   EDIUSR,IDABBR       REQUEST USER ID                              
         MVC   EDILDG,SRACC+2      LEDGER                                       
         MVC   EDILDGN,LDGNME      LEDGER NAME                                  
         MVI   EDICURR,C'1'        CURRENCY CODE 1=UDS 2=CAD                    
         TM    LGSW,CANAD                                                       
         BZ    *+8                                                              
         MVI   EDICURR,C'2'                                                     
* COMPANY NAME AND REQUEST USER ID NAME WERE FILLED IN AT REQFRST               
         L     R4,=A(EDIHED)         PUT OUT HEADER RECORD                      
         BRAS  RE,EDIOUTP                                                       
         AP    TOTRECFF,=P'1'                                                   
         EDIT  TOTRECFF,EDIRECS,ZERO=NOBLANK,FILL=0                             
*                                                                               
FFAF10   MVC   EDIPACC(L'TRNKULA),SRACC+1      PAYEE (VENDOR) CODE              
         ZAP   DUB2,CHNUM                      BUMP TO PROPER CHECK             
         AP    DUB2,=P'1'                      NUMBER FOR TRANSMISSION          
         EDIT  (P8,DUB2),EDICHKN,FILL=0        CHECK NUMBER                     
         EDIT  SRCAMT,EDICAMT,2,ZERO=NOBLANK,ALIGN=LEFT  CHK AMT                
         AP    TCASHFF,SRCAMT                                                   
*                                                                               
         MVI   EDICAMTW,C' '                                                    
         MVC   EDICAMTW+1(L'EDICAMTW-1),EDICAMTW                                
         ZAP   DUB,SRCAMT                                                       
         L     R2,AIO                                                           
         MVC   0(255,R2),SPACES                                                 
         GOTO1 NUMTOLET,DMCB,(RCLANG,DUB),(C'P',(R2))                           
         A     R2,DMCB                                                          
         BCTR  R2,0                R2 TO LAST LETTER OR NUMBER                  
         CLI   0(R2),C'S'                                                       
         BNE   FFAF12                                                           
         MVC   1(3,R2),=C' 00'                                                  
         L     R4,DMCB                                                          
         AHI   R4,3                                                             
         ST    R4,DMCB             ADJUST LENGTH FOR 00 PENNIES                 
*                                                                               
FFAF12   L     R2,AIO                                                           
         LR    R4,R2                                                            
         A     R4,DMCB                                                          
         MVC   0(4,R4),=C'/100'                                                 
         L     R4,DMCB                                                          
         AHI   R4,4                                                             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   EDICAMTW(0),0(R2)   MOVE AMOUNT IN WORDS                         
*                                                                               
         GOTO1 DATCON,DMCB,(1,SRCDTE),(20,EDICDTE)  CHECK DATE                  
         MVC   EDIPNME,SRANME                       PAYEE NAME                  
         EDIT  SRREQ,(3,EDIREQ#),ALIGN=LEFT,FILL=0  REQUEST NUMBER              
         TM    SYEQU,MEDQ          IS THIS A MEDIA LEDGER?                      
         BZ    FFAF20              NO CAN SKIP THIS                             
         MVC   EDIMEDC,SRACC+3     MOVE IN MEDIA CODE FROM 1ST BYTE OF          
         CLC   SRACC+1(3),=C'SSN'  ACCOUNT UNLESS IT'S SSN THAN GET             
         BNE   *+10                FROM MDTELD ELEMENT                          
         MVC   EDIMEDC,SVMEDIA                                                  
FFAF20   XC    EDIRADD1(4*L'EDIRADD1),EDIRADD1                                  
         LA    R1,EDIRADD1          MOVE IN UP TO FOUR ADDRESS LINES            
         LA    RE,SRADDR                                                        
         LA    R0,4                                                             
FFAF22   OC    0(L'SRADDR,RE),0(RE)                                             
         BZ    FFAF30                                                           
         MVC   0(L'SRADDR,R1),0(RE)                                             
         LA    RE,L'SRADDR(RE)                                                  
         LA    R1,L'SRADDR(R1)                                                  
         BCT   R0,FFAF22                                                        
*                                                                               
FFAF30   MVI   EDISPACE,C' '       THIS FIELD REPRESENTS THE ADDRESS            
*                                  FIELDS THAT ARE NOT USED YET                 
         L     R4,=A(EDICTR)        PUT OUT CHECK DETAIL RECORD                 
         BRAS  RE,EDIOUTP                                                       
         AP    TOTRECFF,=P'1'                                                   
         EDIT  TOTRECFF,EDIRECS,ZERO=NOBLANK,FILL=0                             
         B     FFILEX                                                           
*                                                                               
* PROCTRNS                                                                      
FFPRCT   DS    0H                                                               
         GOTO1 DATCON,DMCB,(1,SRTDTE),(20,EDIINVD) INVOICE DATE                 
         TM    SYEQU,MEDQ          MEDIA LEDGER?                                
         BZ    FFPR50              NO                                           
         CLC   SRXPER,SPACES       YES-GET INVOICE DATE FROM XPYPER             
         BNH   FFPR50              IF IT EXISTS.                                
         CLI   SRXPER+6,C' '                                                    
         BH    FFPR10              IF NO END DO MMMDD/YY                        
         GOTO1 DATCON,DMCB,(0,SRXPER),(8,EDIPERD)                               
         B     FFPR50                                                           
*                                                                               
FFPR10   CLI   SRACC+3,C'N'                                                     
         BE    FFPR30              NETWORK IS SPECIAL                           
*                                                                               
FFPR20   GOTO1 DATCON,DMCB,(0,SRXPER+6),(9,EDIPERD) MMM/YY BROADCAST            
         B     FFPR50                                                           
*                                                                               
FFPR30   CLC   SRXPER(6),SRXPER+6     FOR NETWORK IF START=END                  
         BNE   FFPR40                                                           
         GOTO1 DATCON,DMCB,(0,SRXPER),(8,EDIPERD) MMMDD/YY                      
         B     FFPR50                                                           
FFPR40   DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,SRXPER),(0,SRXPER)                                
         GOTO1 DATCON,DMCB,(0,SRXPER+6),(0,SRXPER+6)                            
         GOTO1 GETBROAD,DMCB,(1,SRXPER),WORK,GETDAY,ADDAY                       
         GOTO1 GETBROAD,DMCB,(1,SRXPER+6),WORK+12,GETDAY,ADDAY                  
         CLC   WORK(12),WORK+12         IS IT ONE BROADCAST MONTH               
         BE    FFPR20                   IF IT IS DO IT LIKE SPOT                
         GOTO1 DATCON,DMCB,(0,SRXPER),(8,EDIPERD) IF START NOT = END            
         GOTO1 DATCON,DMCB,(0,SRXPER+6),(8,WORK)  MMMDD-DD/YY                   
         MVI   EDIPERD+5,C'-'                                                   
         MVC   EDIPERD+6(5),WORK+3                                              
*                                                                               
FFPR50   CLC   SRTMMOS,SPACES                                                   
         BNH   FFPR60                                                           
         XC    WORK,WORK                                                        
         MVC   WORK(L'SRTMMOS),SRTMMOS                                          
         MVI   WORK+2,X'01'        MAKE SURE THERE'S A DAY                      
         GOTO1 DATCON,DMCB,(1,WORK),(20,WORK+10)  MEDIA MOS                     
         MVC   EDIMOS,SPACES                                                    
         TM    SYEQU,MEDQ          MEDIA LEDGER?                                
         BZ    FFPR60                                                           
         MVC   EDIMOS(6),WORK+10                                                
FFPR60   MVC   EDICAC,SPACES                                                    
         MVC   EDICACN,SPACES                                                   
         CLI   SRCNTR+1,C' '       IS UL PART OF CONTRA ACCOUNT?                
         BE    *+14                                                             
         MVC   EDICAC,SRCNTR+1     THAN MOVE IN WHOLE CONTRA ACCOUNT            
         B     FFPR70                                                           
         CLI   SRCNTR+3,C' '       NO U/L - IS THERE AN ACCOUNT?                
         BE    *+14                                                             
         MVC   EDICAC(12),SRCNTR+3  THAN MOVE IN THIS PART OF CONTRA            
         B     FFPR70                                                           
         MVC   EDICAC(3),SRCNTR+12  ELSE JUST MOVE IN THE CLIENT CODE           
FFPR70   ZIC   R1,SRCNML           LENGTH OF CONTRA ACCOUNT NAME                
         LTR   R1,R1                                                            
         BNP   FFPR80                                                           
         BCTR  R1,0                                                             
         EXMVC R1,EDICACN,SRCNME                                                
FFPR80   CLC   =C'SY',SRACC+1      FOR THE SY LEGDER-DON'T STORE CLIENT         
         BE    FFPR110             PRODUCT AND THEIR NAMES.                     
         MVC   EDICLT,SPACES                                                    
         TM    SYEQU,MEDQ          MEDIA LEDGER?                                
         BZ    *+20                NO GET CLIENT NAME FROM SJ ACCT              
         MVC   EDICLT,SRTCLIC                                                   
         MVC   EDICLTN(L'SRXCLI),SRXCLI  ELSE GET CLT NME FROM SORT REC         
         B     FFPR90                                                           
*                                                                               
         CLC   SRCNTR+1(2),=C'SJ'    IF PROD/EXP AND C/A IS NOT SJ              
         BNE   FFPR90                THEN DON'T STORE CLIENT                    
         CLC   SRTCLIC,SPACES                                                   
         BNH   FFPR90                                                           
         MVC   EDICLT,SRTCLIC                                                   
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'SRTCLIC),SRTCLIC                                          
         BRAS  RE,SJNAM                                                         
         MVC   EDICLTN,WORK                                                     
*                                                                               
FFPR90   MVC   EDIPRD,SPACES                                                    
         TM    SYEQU,MEDQ          MEDIA LEDGER?                                
         BZ    FFPR92              NO GET PRODUCT NAME FROM SJ ACCT             
         MVC   EDIPRD,SROPRD       PRODUCT CODE                                 
         CLC   SROPRD,SPACES                                                    
         BH    *+10                                                             
         MVC   EDIPRD,SRTREF       1ST 3 CHARACTERS ARE REFERENCE               
         MVC   EDIPRDN(L'SRXPRD),SRXPRD  ELSE GET PROD NME FROM SORT            
         B     FFPR100                                                          
*                                                                               
FFPR92   CLC   SROPRD,SPACES                                                    
         BNH   FFPR100                                                          
         MVC   EDIPRD,SROPRD                                                    
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'SRTCLIC),SRTCLIC                                          
         MVC   WORK+L'SRCLI(L'SROPRD),SROPRD                                    
         BRAS  RE,SJNAM                                                         
         MVC   EDIPRDN,WORK                                                     
*                                                                               
FFPR100  MVC   EDIJOB,SPACES                                                    
         TM    SYEQU,MEDQ          MEDIA LEDGER?                                
         BZ    *+14                YES THAN DONE WITH THIS                      
         MVC   EDIJOB(L'SROEST),SROEST  ESTIMATE CODE                           
         B     FFPR110                                                          
*                                                                               
         CLC   SROJOB,SPACES                                                    
         BNH   FFPR110                                                          
         MVC   EDIJOB(L'SROJOB),SROJOB  NO MOVE IN JOB CODE INSTEAD AND         
         MVC   WORK,SPACES              GET JOB NAME FROM SJ ACCOUNT            
         MVC   WORK(L'SRTCLIC),SRTCLIC                                          
         MVC   WORK+L'SRTCLIC(L'SROPRD),SROPRD                                  
         MVC   WORK+L'SRTCLIC+L'SROPRD(L'SROJOB),SROJOB                         
         BRAS  RE,SJNAM                                                         
         MVC   EDIJOBN,WORK                                                     
*                                                                               
FFPR110  CLC   SRXINV,SPACES       FOR MEDIA CHKS INV COMES FROM                
         BNH   *+14                46 ELEM.  FOR PROD CHKS INV COMES            
         MVC   EDIINVN(L'SRXINV),SRXINV  FROM REFERENCE                         
         B     FFPR120                                                          
         MVC   EDIINVN(L'SRTREF),SRTREF                                         
FFPR120  CLC   SRPLINV,SPACES      LONGER INVOICE EXISTS?                       
         BNH   *+10                                                             
         MVC   EDIINVN(L'SRPLINV),SRPLINV     USE IT                            
*                                                                               
         MVC   EDIVCDE,SPACES                                                   
         MVC   EDIVNME,SPACES                                                   
         TM    SYEQU,MEDQ          TRUE VENDOR CODE/NAME IS ONLY FOR            
         BZ    FFPR150             MEDIA CHECKS                                 
         CLC   SRCNTR+4(8),SPACES  IS THIS A PUB?                               
         BE    FFPR140                                                          
*REP                                                                            
         MVC   EDIVNME(L'SRCNME),SRCNME REP-IT COMES FROM C/A NAME              
         CLI   SRCNTR+1,C' '            IS U/L PART OF C/A CODE?                
         BNH   *+14                                                             
         MVC   EDIVCDE(11),SRCNTR+1     IF SO THEN ADD IT TO TRUE               
         B     *+10                     VENDOR CODE ELSE SKIP IT                
         MVC   EDIVCDE(9),SRCNTR+3                                              
         CLI   SRACC+2,C'Q'        P OR Q MEANS USE PUBED                       
         BH    FFPR150                                                          
         SR    R1,R1                                                            
         IC    R1,SRCNML                                                        
         LA    R2,EDIVNME+4(R1)      POINT 2 BEYOND NAME                        
         MVC   WORK(15),SRACC                                                   
         PACK  DUB(6),WORK+4(11)                                                
         MVC   DUB+5(1),WORK+14    EDITION                                      
         GOTO1 PUBEDIT,DMCB,DUB,(C'S',1(R2))                                    
         CLC   SRCNTR+9(3),=C'ZZZ'                                              
         BNE   FFPR130                                                          
         LA    RF,17(R2)                                                        
         CLI   0(RF),C','                                                       
         BE    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   1(3,RF),=C'ALL'                                                  
*                                                                               
FFPR130  MVI   0(R2),C'('                                                       
         LA    R2,17(R2)                                                        
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),C')'                                                       
         B     FFPR150                                                          
*                                                                               
* PUB                                                                           
FFPR140  MVC   EDIVCDE(L'SRACC-3),SRACC+3  TRUE VENDOR CODE                     
         MVC   EDIVNME(L'SRANME),SRANME PUB-IT COMES FROM ACCT NAME             
*                                                                               
FFPR150  MVC   EDIOFFC,SRTANL           OFFICE                                  
         BRAS  RE,OFFNAM                GET THE OFFICE NAME                     
         MVC   EDIOFFN,WORK                                                     
         XC    WORK,WORK                                                        
         MVC   WORK(L'SRXMOS),SRXMOS                                            
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(6,EDIMOA)                                  
         MVI   EDINARR,C' '                                                     
         MVC   EDINARR+1(L'EDINARR-1),EDINARR                                   
         CLI   SRTNRL,0            ANY NARRATIVE?                               
         BE    FFPR160                                                          
         ZIC   RF,SRTNRL                                                        
         BCTR  RF,0                                                             
         EXMVC RF,EDINARR,SRTNRR                                                
*                                                                               
FFPR160  ZAP   INVPAY,SRTAMT       TRANSACTION AMT (NET)                        
         ZAP   INVCD,SRTCD         CASH DISCOUNT AMOUNT                         
         EDIT  INVPAY,EDINAMT,2,ZERO=NOBLANK,ALIGN=LEFT,FLOAT=- NET AMT         
         EDIT  INVCD,EDIDAMT,2,ZERO=NOBLANK,ALIGN=LEFT,FLOAT=- C/D              
* SRTGRS IS THE TRUE GROSS.  THIS WILL CONTAIN THE GROSS FOR MEDIA OR           
* ZERO (FROM THE 50 ELEM) AND WILL ALWAYS BE ZERO FOR PRODUCTION CHKS.          
         EDIT  SRTGRS,EDITGAMT,2,ZERO=NOBLANK,ALIGN=LEFT,FLOAT=-                
         AP    INVPAY,INVCD                                                     
         EDIT  INVPAY,EDIGAMT,2,ZERO=NOBLANK,ALIGN=LEFT,FLOAT=- NET+C/D         
         EDIT  SRTGST,EDIGST,2,ZERO=NOBLANK,ALIGN=LEFT,FLOAT=- GST AMT          
         EDIT  SRTPST,EDIPST,2,ZERO=NOBLANK,ALIGN=LEFT,FLOAT=- PST AMT          
         L     R4,=A(EDISDR)        PUT OUT STUB DETAIL RECORD                  
         BRAS  RE,EDIOUTP                                                       
         AP    TOTRECFF,=P'1'                                                   
         EDIT  TOTRECFF,EDIRECS,ZERO=NOBLANK,FILL=0                             
         B     FFILEX                                                           
*                                                                               
FFRUNL   AP    TOTRECFF,=P'1'                                                   
         EDIT  TOTRECFF,EDIRECS,ZERO=NOBLANK,FILL=0                             
         EDIT  TCASHFF,EDITOTD,2,ZERO=NOBLANK,ALIGN=LEFT,FLOAT=-                
         EDIT  CHOKS,EDITCHK,ZERO=NOBLANK,FILL=0                                
         MVC   P,SPACES                                                         
         L     R4,=A(EDITRR)       PUT OUT TRAILER RECORD                       
         BRAS  RE,EDIOUTP                                                       
         BRAS  RE,CLSDLCB                                                       
         MVI   FORCEHED,C'Y'       SO REGISTER PRINTS HEADLINES                 
*                                                                               
*FILEX   XIT1                                                                   
FFILEX   L     RE,WORD             RESTORE ADDRESS OF RE                        
         BR    RE                                                               
         DROP  R9                                                               
*        DROP R8                                                                
         EJECT                                                                  
*&&DO                                                                           
**********************************************************************          
* MINDSHARE 820 3040 BANK TRANSMISSION-PUT RECORDS TO TAPE           *          
**********************************************************************          
         USING E820D,R7                                                         
MIND820  L     R7,=A(E820WRK)                                                   
         ST    R7,SVWRKREG                                                      
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
*-----------------------------------                                            
* ACCFRST                                                                       
*-----------------------------------                                            
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
*-----------------------------------                                            
* ADDRESS                                                                       
*-----------------------------------                                            
         LA    R2,SRADDR          POINT TO ADDRESS                              
         LA    R3,L'SRADDR        LENGTH OF ONE ADDRESS LINE                    
         LA    R4,SRADDR+(3*L'SRADDR) FOURTH ADDRESS LINE                       
         OC    SRADDR+(1*L'SRADDR),SPACES  MAKE SURE STATE IS UPPERCASE         
         OC    SRADDR+(2*L'SRADDR),SPACES  ON LINES 2,3 OR 4                    
         OC    SRADDR+(3*L'SRADDR),SPACES                                       
*                                                                               
MINDA10  CR    R2,R4              POINTING TO FIRST LINE?                       
         BE    MINDA25            MUST BE AN ERROR                              
         GOTO1 ADFORM,DMCB,((R3),(R4)),(30,E8CITY),                    +        
               E8STATE,(9,E8ZIP),(2,E8CTRY),0                                   
         TM    0(R1),X'80'        80=SERIOUS ERROR                              
         BO    MINDA15                                                          
         TM    0(R1),X'01'        FOREIGN?                                      
         BO    MINDA22                                                          
         CLC   E8CITY,SPACES                                                    
         BNE   MINDA20                                                          
MINDA15  OC    0(L'SRADDR,R4),0(R4)  IF ZERO, THEN NO ADDRESS LINE              
         BZ    *+8                                                              
         AHI   R3,L'SRADDR        SEND ANOTHER LINE                             
         AHI   R4,-L'SRADDR       BACK UP                                       
         B     MINDA10                                                          
*                                                                               
MINDA20  TM    0(R1),X'02'        02=INCORRECT STATE/ZIP COMBO                  
         BO    MINDA25                                                          
         CLC   E8CTRY(2),=C'US'                                                 
         BNE   MINDA22                                                          
         MVC   E8ADR1,0(R2)                                                     
         LA    R2,L'SRADDR(R2)                                                  
         CR    R2,R4                                                            
         BE    MINDA30                                                          
         MVC   E8ADR2,0(R2)                                                     
         B     MINDA30                                                          
*                                                                               
MINDA22  MVI   E8FORF,C'F'           HANDLE FOREIGN ADDRESSES                   
         LA    R3,E8ADR1                                                        
         LHI   R0,4                  COPY ALL FOUR ADRESS LINES                 
MINDA23  OC    0(L'SRADDR,R2),0(R2)                                             
         BZ    MINDA30                                                          
         MVC   0(L'E8ADR1,R3),0(R2)                                             
         LA    R2,L'SRADDR(R2)                                                  
         LA    R3,L'E8ADR1(R3)                                                  
         BCT   R0,MINDA23                                                       
         B     MINDA30                                                          
*                                                                               
MINDA25  MVC   E8ADR1(17),=CL17'*INVALID ADDRESS*'                              
         MVC   E8CITY(17),=CL17'*INVALID ADDRESS*'                              
         MVC   E8STATE,=CL17'*INVALID ADDRESS*'                                 
         MVC   E8ZIP,=CL17'*INVALID ADDRESS*'                                   
         MVC   E8CTRY,=CL17'*INVALID ADDRESS*'                                  
*                                                                               
MINDA30  OC    E8ADR1,SPACES       MUST BE ALL CAPS FOR BOA                     
         OC    E8ADR2,SPACES                                                    
         OC    E8ADR3,SPACES                                                    
         OC    E8ADR4,SPACES                                                    
         OC    E8CITY(E8CSZQ),SPACES                                            
*---------------------------------------------                                  
         L     R4,=A(E8ATAB)        820 AGENCY TABLE                            
         USING E8ATABD,R4                                                       
MINDA31  CLI   0(R4),X'FF'          VALID AGENCY TO OUTPUT                      
         BE    MINDA34                                                          
         CLC   ORIGINUM,E8AONUM     MATCH USER ID                               
         BE    MINDA34                                                          
         LA    R4,E8ATBLNQ(R4)                                                  
         B     MINDA31                                                          
MINDA34  MVC   E8BNKNO,E8ABNKA       SAVE BANK ACCOUNT                          
         MVC   E8LOGO,E8ALOGO        SAVE LOGO CODE                             
         MVC   E8RACO,E8ARACO        SAVE RETURN ADDRESS CODE                   
         MVC   E8SIGCO,E8ASIGCO      SAVE SIGNATURE CODE                        
         DROP  R4                                                               
*                                                                               
         L     R4,=A(E8H7)          820 AGENCY TABLE                            
         TM    DTXSW,FILOPN         TEST MINDSHARE FILE OPENED                  
         BO    MINDA35                                                          
         BRAS  RE,MOPN              OPEN IT                                     
         OI    DTXSW,FILOPN                                                     
MINDA35  BRAS  RE,MINDWOF                                                       
*                                                                               
         L     R4,=A(E8H7N4)                                                    
         CLI   E8FORF,C' '          IF FOREIGN                                  
         BE    MINDA40               NO                                         
         CLC   E8ADR3,SPACES        AND MORE THAN TWO LINES                     
         BE    MINDA40               NO                                         
         L     R4,=A(E8H7N3)        NEED TO PUT OUT 2ND N3 RECORD               
MINDA40  BRAS  RE,MINDWOF                                                       
         B     NOWXIT                                                           
*-----------------------------------                                            
* PROCTRNS                                                                      
*-----------------------------------                                            
MPROCTRN AP    INVPAY,SRTAMT           ACCUMULATE INVOICE AMOUNT                
         AP    INVCD,SRTCD             AND CASH DISCOUNT                        
         AP    PRDPAY,SRTAMT           ACCUMULATE TOTAL AMOUNT                  
         AP    PRDCD,SRTCD             AND TOTAL CASH DISCOUNT                  
*                                                                               
         CLI   SYEQU,MEDQ              IS IT A MEDIA CHECK?                     
         BE    MINDP02                 YES, SHOW EVERY INVOICE                  
*                                                                               
         L     RF,ANXTSR               RF = NEXT SORT RECORD                    
         TM    SCS,SCSEOF              LAST RECORD                              
         BO    *+14                                                             
         CLC   SREC(SRKLQ-1),0(RF)     TEST SAME INV (-1 FOR SUBREF)            
         BE    NOWXIT                  EQUAL, EXIT MERGE INVOICES               
*                                                                               
MINDP02  BRAS  RE,MINDGET              GET INVOICE INFORMATION                  
         BAS   RE,BUMPRMR                INCREMENT RMR SEQUENCE #               
*                                                                               
         L     R4,=A(RMRHED)           REMMITTANCE INFORMATION                  
         CLC   E8INFOS,E8INFO          MAKE SURE INFO HAS CHANGED               
         BE    MINDP10                                                          
         CLC   E8INFO2,SPACES          IS INFO2 SPACES?                         
         BNE   MINDP05                 NO, CONTINUE                             
         MVC   E8INFO2,E8INFO          YES, COPY INFO TO INFO2                  
         B     MINDP10                                                          
*                                                                               
MINDP05  BRAS  RE,MINDWOF                                                       
         BAS   RE,BUMPRMR                INCREMENT RMR SEQUENCE #               
*                                                                               
MINDP10  MVC   E8INFOS,E8INFO                                                   
*-----------------------------------                                            
* REMITTANCE                                                                    
*-----------------------------------                                            
         L     R4,=A(RMRDET)           REMMITTANCE INFORMATION                  
         BRAS  RE,MINDWOF                                                       
*                                                                               
         CLC   E8NARR1,SPACES            REMMITTANCE NARRATIVE                  
         BNH   NOWXIT                                                           
         BAS   RE,BUMPRMR                INCREMENT RMR SEQUENCE #               
         L     R4,=A(RMRTEXT)            REMMITTANCE NARRATIVE                  
         BRAS  RE,MINDWOF                                                       
*                                                                               
**AW - BANK OF AMERICA CANNOT HANDLE MORE THAN 80 CHARACTERS                    
*        CLC   E8NARR2,SPACES                                                   
*        BNH   NOWXIT                                                           
*        MVC   E8NARR(L'E8NARR2),E8NARR2 TEXT IN E8NARR1 FOR PROCESSING         
*        L     R4,=A(RMRTEXR)            REMMITTANCE NARRATIVE                  
*        BRAS  RE,MINDWOF                                                       
*                                                                               
*        CLC   E8NARR3,SPACES                                                   
*        BNH   NOWXIT                                                           
*        MVC   E8NARR1,SPACES                                                   
*        MVC   E8NARR(L'E8NARR3),E8NARR3 TEXT IN E8NARR1 FOR PROCESSING         
*        L     R4,=A(RMRTEXR)            REMMITTANCE NARRATIVE                  
*        BRAS  RE,MINDWOF                                                       
*                                                                               
*                                                                               
         B     NOWXIT                                                           
*-----------------------------------                                            
* ACCLAST                                                                       
*-----------------------------------                                            
MACCLAST BAS   RE,BUMPRMR                                                       
         EDIT  PRDPAY,E8CHAMT,2,ZERO=NOBLANK,ALIGN=LEFT,COMMAS=Y                
         AP    PRDPAY,PRDCD                                                     
         EDIT  PRDPAY,E8CHGRSS,2,ZERO=NOBLANK,ALIGN=LEFT,COMMAS=Y               
*                                                                               
         L     R4,=A(RMRTOT)        TOTAL RECS FOR 820                          
         BRAS  RE,MINDWOF                                                       
         B     NOWXIT                                                           
*-----------------------------------                                            
* RUNLAST                                                                       
*-----------------------------------                                            
MRUNLAST TM    DTXSW,FILOPN        TEST MINDSHARE ACTIVITY                      
         BNO   NOWXIT                                                           
         CLOSE (EDIOUT)            CLOSE MINDSHARE FILE                         
*                                                                               
NOWXIT   XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
* INCREMENT RMR SEQUENCE NUMBER                                                 
*********************************************************************           
BUMPRMR  AP    INVCNT,=P'1'              BUMP SEQUENCE                          
         EDIT  INVCNT,E8RMRSEQ,ZERO=NOBLANK,FILL=0                              
         BR    RE                                                               
*********************************************************************           
* WRITE EDI OUTPUT FILE                                             *           
*********************************************************************           
MINDWOF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
MWOF10   L     R2,AIO                                                           
         USING E820HDRD,R2                                                      
         MVC   E820SET,=C'820'      SET                                         
         MVC   E820SPA,SPACES                                                   
         MVC   E820SEG(6),0(R4)     SEQMENT AND SEQUENCE                        
         MVC   E820ZRO,=C'00000'                                                
         LA    R3,E820LNQ                                                       
         STCM  R3,3,RECLEN         INITIALIZE RECORD LENGTH                     
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
         ICM   R1,3,RECLEN                                                      
         AR    R1,R3                                                            
         STCM  R1,3,RECLEN                                                      
         LR    R0,R2               R0=DESTINATION                               
         LR    R1,R3               R1 & RF = LENGTH                             
         LR    RF,R3                                                            
         MVCL  R0,RE               DATA TO IO AREA                              
         AR    R2,R3               R2 TO NEXT AREA                              
*                                                                               
MWOF50   CLI   0(R4),EOR           TEST END OF RECORD                           
         BE    MWOF60                                                           
         L     R5,=A(XDATA)                                                     
         ST    R5,FULL             STORE ADDRESS OF RBC TABLE                   
         BRAS  RE,XTRA             EXTRACT SPECIAL DATA                         
         L     R2,RECNXT           R2=NEXT DATA AREA                            
         LA    R4,4(R4)            BUMP R4 PASSED ESCAPE SEQUENCE               
         B     MWOF20                                                           
*                                                                               
MWOF60   L     R2,AIO                                                           
         XC    E820LN(4),E820LN                                                 
         MVC   E820LN,RECLEN       SET LENGTH                                   
*                                                                               
         L     RF,=A(EDIOUT)                                                    
         PUT   (RF),E820HDRD                                                    
*                                                                               
         LA    R4,1(R4)                                                         
         CLI   0(R4),EOT           TEST END OF TABLE                            
         BNE   MWOF10                                                           
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*******************************************************************             
* GET INVOICE INFORMATION,NUMBER,CD,AND INVOICE DATE/PERIOD       *             
*******************************************************************             
MINDGET  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   E8INFO,SPACES                                                    
         MVC   E8INFO2,SPACES                                                   
         MVC   E8INVDTE,SPACES                                                  
         MVC   E8INVNO,SPACES                                                   
         MVC   E8NARR1,SPACES                                                   
         MVC   E8NARR2,SPACES                                                   
         MVC   E8NARR3,SPACES                                                   
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
         ST    R5,FULL                                                          
         BRAS  RE,FNDNXT               R5 TO NEXT SPACE                         
         L     R5,FULL                                                          
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
         ST    R5,FULL                                                          
         BRAS  RE,FNDNXT                                                        
         L     R5,FULL                                                          
         MVC   1(L'SRXPRD,R5),SRXPRD   PRODUCT NAME                             
         LA    R5,E8INFO2+L'E8INFO2-1                                           
         ST    R5,FULL                                                          
         BRAS  RE,FNDNXT                                                        
         L     R5,FULL                                                          
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
MINDG55  MVC   E8INFO(L'SRANME),SRANME PRINT PUB NAME                           
         CLI   TYCDE,50                PRINT REP                                
         BNE   MINDG75                 NO                                       
*                                                                               
         MVC   E8INFO(L'SRCNME),SRCNME  CONTRA ACCOUNT NAME                     
         LA    R5,E8INFO+L'E8INFO-1                                             
         ST    R5,FULL                                                          
         BRAS  RE,FNDNXT                                                        
         L     R5,FULL                                                          
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
         ST    R5,FULL                                                          
         BRAS  RE,FNDNXT                                                        
         L     R5,FULL                                                          
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
         CLI   SROPRD,0                  OTHERS ELEMENT                         
         BE    MINDG100                                                         
         LA    R5,E8INFO2                                                       
         MVC   0(L'AC@PRO,R5),AC@PRO     C'PRODUCT='                            
         LA    R5,E8INFO2+L'E8INFO2-1                                           
         ST    R5,FULL                                                          
         BRAS  RE,FNDNXT                                                        
         L     R5,FULL                                                          
         MVI   0(R5),C'='                                                       
         MVC   1(L'SROPRD,R5),SROPRD     PRODUCT CODE                           
         LA    R5,E8INFO2+L'E8INFO2-1                                           
         ST    R5,FULL                                                          
         BRAS  RE,FNDNXT                                                        
         L     R5,FULL                                                          
         LA    RE,DICO           NEED ADDRESSABILITY TO THE                     
         AHI   RE,AC@ASYS-DICO   DATA DICTIONARY                                
         MVC   1(L'AC@JOB,R5),0(RE)      C'JOB='                                
         LA    R5,E8INFO2+L'E8INFO2-1                                           
         ST    R5,FULL                                                          
         BRAS  RE,FNDNXT                                                        
         L     R5,FULL                                                          
         MVI   0(R5),C'='                                                       
         MVC   1(L'SROJOB,R5),SROJOB     JOB CODE                               
*                                                                               
MINDG100 MVC   E8INVNO(L'SRTREF),SRTREF  USE REFERENCE                          
         CLC   SRPLINV,SPACES            SUPPLIER INVOICE?                      
         BNH   *+10                                                             
         MVC   E8INVNO,SRPLINV           USE SUPPLIER INVOICE                   
         GOTO1 DATCON,DMCB,(1,SRTDTE),(10,E8INVDTE)  INVOICE DATE               
*       -------------------------------------------                             
MINDG110 EDIT  INVCD,E8CD,2,ALIGN=LEFT,FLOAT=-,ZERO=BLANK                       
         EDIT  INVPAY,E8INVNET,2,ALIGN=LEFT,FLOAT=-,COMMAS=YES                  
         AP    INVPAY,INVCD                                                     
         EDIT  INVPAY,E8INVGRS,2,ALIGN=LEFT,FLOAT=-,COMMAS=YES                  
*                                                                               
         MVC   E8NARR,SRTNRR             NARRATIVE                              
*                                                                               
MIND112  ZAP   INVPAY,=P'0'                                                     
         ZAP   INVCD,=P'0'                                                      
*                                                                               
MINDGX   XIT1                                                                   
**********************************************************************          
* OPEN MINDSHARE 820 EDI FILE                                        *          
**********************************************************************          
MOPN     NTR1  BASE=*,LABEL=*                                                   
         L     R2,=A(DDPARM)                                                    
         L     R3,=A(DSPARM)                                                    
         GOTO1 DYNALLOC,DMCB,(0,(R2)),(R3)                                      
         OPEN  (EDIOUT,OUTPUT)                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XIT1                                                                   
         DROP  R7                                                               
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* READ VENDOR ACCOUNT FOR SOME INFO                                 *           
*********************************************************************           
***                                                                             
*** CAN USE AIO TO READ BUT CAN'T KEEP IT AROUND!                               
***                                                                             
         USING EDIDD,R9                                                         
         USING ACTRECD,RF                                                       
RDVENDOR NTR1  BASE=*,LABEL=*                                                   
         L     R9,AEDIWRK                                                       
         ST    R9,SVWRKREG                                                      
*                                                                               
         XC    WORK,WORK                                                        
         L     RF,AIO                                                           
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY(ACTKEND),SRACC   C/U/L/VENDOR ACCT FROM SOR REC          
         MVC   TEMPKEY(ACTKEND),0(RF)                                           
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCFIL,AIO,AIO                               
         L     RF,AIO                                                           
         CLC   0(ACTKEND,RF),TEMPKEY                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO                                                           
         AH    R2,DATADISP                                                      
*                                                                               
         LA    RE,EDIVEN                  CLEAR VENDOR RECORD EDI INFO          
         LHI   RF,EDIVENLN                                                      
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0                                                            
*                                                                               
         USING FFTELD,R2                                                        
RDVEN10  CLI   0(R2),0                                                          
         BE    RDVENDX                                                          
         CLI   0(R2),FFTELQ        DB ELEMENT                                   
         BNE   RDVEN15                                                          
         CLI   FFTTYPE,FFTTPFAX    FAX NUMBER                                   
         BE    RDVEN30                                                          
         CLI   FFTTYPE,FFTTEML     EMAIL                                        
         BE    RDVEN40                                                          
         CLI   FFTTYPE,FFTTCONT    CONTACT NAME                                 
         BE    RDVEN50                                                          
         CLI   FFTTYPE,FFTTPTEL    TELEPHONE NUMBER                             
         BE    RDVEN55                                                          
         B     RDVENXT                                                          
RDVEN15  CLI   0(R2),X'80'         VENDOR BANK PROFILE ELEM                     
         BE    RDVEN60                                                          
RDVENXT  IC    R0,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,R0                                                            
         B     RDVEN10                                                          
*                                                                               
RDVEN30  ZIC   RF,FFTDLEN                                                       
         BCTR  RF,0                                                             
         EXMVC RF,EDIFAX,FFTDATA   SAVE FAX                                     
         B     RDVENXT                                                          
*                                                                               
RDVEN40  ZIC   RF,FFTDLEN                                                       
         BCTR  RF,0                                                             
         EXMVC RF,EDIEMAIL,FFTDATA  SAVE EMAIL                                  
         B     RDVENXT                                                          
*                                                                               
RDVEN50  ZIC   RF,FFTDLEN                                                       
         BCTR  RF,0                                                             
         EXMVC RF,EDICNTN,FFTDATA   SAVE CONTACT NAME                           
         B     RDVENXT                                                          
*                                                                               
RDVEN55  ZIC   RF,FFTDLEN                                                       
         BCTR  RF,0                                                             
         EXMVC RF,EDICPHN,FFTDATA   SAVE TELPHONE NUMBER                        
         B     RDVENXT                                                          
*                                                                               
         USING VBPFELD,R2                                                       
RDVEN60  MVC   EDIDBNKA,VBPBACC    VENDOR BANK ACCOUNT                          
         MVC   EDIDROUT,VBPROUT    VENDOR ROUTING NUMBER                        
*                                                                               
         USING RMTTABD,RF                                                       
         LA    RF,RMTTAB                                                        
RDVEN65  CLI   0(RF),X'FF'                                                      
         BE    RDVENXT                                                          
         CLC   EDIFORMT,RMTFRM     MATCH ON FORMAT                              
         BNE   RDVENNX                                                          
         TM    RMTBYTE,RMTCTRY     ENTRY BY COUNTRY?                            
         BZ    RDVEN70             NO SO FOUND ENTRY                            
         TM    LGSW,CANAD          CANADIAN LEDGER?                             
         BZ    RDVEN67             YES                                          
         TM    RMTBYTE,RMTCAN      IS THIS FOR CANADA?                          
         BO    RDVEN70                                                          
         B     RDVENNX                                                          
*                                                                               
RDVEN67  TM    RMTBYTE,RMTCAN                                                   
         BZ    RDVEN70                                                          
*                                                                               
RDVENNX  LA    RF,RMTLNQ(RF)                                                    
         B     RDVEN65                                                          
*                                                                               
RDVEN70  CLI   RMTVSET,0           IS THIS THE DEFAULT                          
         BE    *+14                USE IT                                       
         CLC   VBPRDEL,RMTVSET     MATCH ON REMITT DEL                          
         BNE   RDVENNX                                                          
         MVC   EDIREMDL,RMTESET                                                 
         TM    VBPRDEL,VBPFXRM     IS THE REMITT DEL FAX?                       
         BZ    *+12                                                             
         MVI   EDIREMDC,C'F'       THEN SAVE THIS INFO                          
         B     RDVENXT                                                          
         TM    VBPRDEL,VBPEMRM     IS THE REMITT DEL EMAIL?                     
         BZ    RDVENXT                                                          
         MVI   EDIREMDC,C'E'       THEN SAVE THIS INFO                          
         B     RDVENXT                                                          
RDVENDX  XIT1                                                                   
         DROP  R9,RF                                                            
*                                                                               
RMTTAB   DC    CL10'ROY435',X'80',AL1(VBPFXRM),CL2'F '  US/FAX                  
         DC    CL10'ROY435',X'80',AL1(VBPEMRM),CL2'G '  US/EMAIL                
         DC    CL10'ROY435',X'80',AL1(0),CL2'E '  US/DEFAULT                    
         DC    CL10'ROY435',X'C0',AL1(VBPFXRM),CL2'C '  CA/FAX                  
         DC    CL10'ROY435',X'C0',AL1(VBPEMRM),CL2'D '  CA/EMAIL                
         DC    CL10'ROY435',X'C0',AL1(0),CL2'A '  CA/DEFAULT                    
         DC    CL10'HS600',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                  
         DC    CL10'HS600',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL                
         DC    CL10'HS600',X'00',AL1(0),CL2'  '   ALL/DEFAULT                   
         DC    CL10'HS600A',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                 
         DC    CL10'HS600A',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL               
         DC    CL10'HS600A',X'00',AL1(0),CL2'  '   ALL/DEFAULT                  
         DC    CL10'HS600B',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                 
         DC    CL10'HS600B',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL               
         DC    CL10'HS600B',X'00',AL1(0),CL2'  '   ALL/DEFAULT                  
         DC    CL10'HS600C',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                 
         DC    CL10'HS600C',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL               
         DC    CL10'HS600C',X'00',AL1(0),CL2'  '   ALL/DEFAULT                  
         DC    CL10'HS600D',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                 
         DC    CL10'HS600D',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL               
         DC    CL10'HS600D',X'00',AL1(0),CL2'  '   ALL/DEFAULT                  
         DC    CL10'HS600E',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                 
         DC    CL10'HS600E',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL               
         DC    CL10'HS600E',X'00',AL1(0),CL2'  '   ALL/DEFAULT                  
         DC    CL10'HS600F',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                 
         DC    CL10'HS600F',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL               
         DC    CL10'HS600F',X'00',AL1(0),CL2'  '   ALL/DEFAULT                  
         DC    CL10'HS600G',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                 
         DC    CL10'HS600G',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL               
         DC    CL10'HS600G',X'00',AL1(0),CL2'  '   ALL/DEFAULT                  
         DC    CL10'HS600H',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                 
         DC    CL10'HS600H',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL               
         DC    CL10'HS600H',X'00',AL1(0),CL2'  '   ALL/DEFAULT                  
         DC    CL10'HS600I',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                 
         DC    CL10'HS600I',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL               
         DC    CL10'HS600I',X'00',AL1(0),CL2'  '   ALL/DEFAULT                  
         DC    CL10'HS600J',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                 
         DC    CL10'HS600J',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL               
         DC    CL10'HS600J',X'00',AL1(0),CL2'  '   ALL/DEFAULT                  
         DC    CL10'HS600K',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                 
         DC    CL10'HS600K',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL               
         DC    CL10'HS600K',X'00',AL1(0),CL2'  '   ALL/DEFAULT                  
         DC    CL10'HS600L',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                 
         DC    CL10'HS600L',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL               
         DC    CL10'HS600L',X'00',AL1(0),CL2'  '   ALL/DEFAULT                  
         DC    CL10'HS600M',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                 
         DC    CL10'HS600M',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL               
         DC    CL10'HS600M',X'00',AL1(0),CL2'  '   ALL/DEFAULT                  
         DC    CL10'HS600N',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                 
         DC    CL10'HS600N',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL               
         DC    CL10'HS600N',X'00',AL1(0),CL2'  '   ALL/DEFAULT                  
         DC    CL10'HS600N1',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                
         DC    CL10'HS600N1',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL              
         DC    CL10'HS600N1',X'00',AL1(0),CL2'  '   ALL/DEFAULT                 
         DC    CL10'HS600N2',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                
         DC    CL10'HS600N2',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL              
         DC    CL10'HS600N2',X'00',AL1(0),CL2'  '   ALL/DEFAULT                 
         DC    CL10'HS600N3',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                
         DC    CL10'HS600N3',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL              
         DC    CL10'HS600N3',X'00',AL1(0),CL2'  '   ALL/DEFAULT                 
         DC    CL10'HS600N4',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                
         DC    CL10'HS600N4',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL              
         DC    CL10'HS600N4',X'00',AL1(0),CL2'  '   ALL/DEFAULT                 
         DC    CL10'HS600N5',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                
         DC    CL10'HS600N5',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL              
         DC    CL10'HS600N5',X'00',AL1(0),CL2'  '   ALL/DEFAULT                 
         DC    CL10'HS600N6',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                
         DC    CL10'HS600N6',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL              
         DC    CL10'HS600N6',X'00',AL1(0),CL2'  '   ALL/DEFAULT                 
         DC    CL10'HS600N7',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                
         DC    CL10'HS600N7',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL              
         DC    CL10'HS600N7',X'00',AL1(0),CL2'  '   ALL/DEFAULT                 
         DC    CL10'HS600N8',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                
         DC    CL10'HS600N8',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL              
         DC    CL10'HS600N8',X'00',AL1(0),CL2'  '   ALL/DEFAULT                 
         DC    CL10'HS600N9',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                
         DC    CL10'HS600N9',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL              
         DC    CL10'HS600N9',X'00',AL1(0),CL2'  '   ALL/DEFAULT                 
         DC    CL10'HS600NA',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                
         DC    CL10'HS600NA',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL              
         DC    CL10'HS600NA',X'00',AL1(0),CL2'  '   ALL/DEFAULT                 
         DC    CL10'HS600NB',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                
         DC    CL10'HS600NB',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL              
         DC    CL10'HS600NB',X'00',AL1(0),CL2'  '   ALL/DEFAULT                 
         DC    CL10'HS600NC',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                
         DC    CL10'HS600NC',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL              
         DC    CL10'HS600NC',X'00',AL1(0),CL2'  '   ALL/DEFAULT                 
*MN SPEC-45972,SPEC-45976,SPEC-46426                                            
         DC    CL10'HS600ND',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                
         DC    CL10'HS600ND',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL              
         DC    CL10'HS600ND',X'00',AL1(0),CL2'  '   ALL/DEFAULT                 
*MN SPEC-45972,SPEC-45976,SPEC-46426                                            
*MN SPEC-46428                                                                  
         DC    CL10'HS600A1',X'00',AL1(VBPFXRM),CL2'FX'  ALL/FAX                
         DC    CL10'HS600A1',X'00',AL1(VBPEMRM),CL2'EM'  ALL/EMAIL              
         DC    CL10'HS600A1',X'00',AL1(0),CL2'  '   ALL/DEFAULT                 
*MN SPEC-46428                                                                  
         DC    X'FF'                                                            
*&&DO                                                                           
*                                  CANADIAN TABLE OF PAYMENT TYPES              
CARMTAB  DC    AL1(VBPFXRM),C'C'   FAX REMITTANCE GETS PAYMENT TYPE C           
         DC    AL1(VBPEMRM),C'D'   EMAIL REMITTANCE GETS PAYMENT TYPE D         
         DC    X'FF'                                                            
*                                  US TABLE OF PAYMENT TYPES                    
USRMTAB  DC    AL1(VBPFXRM),C'F'   FAX REMITTANCE GETS PAYMENT TYPE F           
         DC    AL1(VBPEMRM),C'G'   EMAIL REMITTANCE GETS PAYMENT TYPE G         
         DC    X'FF'                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* FORMAT ADDRESS BLOCKS                                             *           
*        P1=A(ADDRESS BLOCK)                                        *           
*********************************************************************           
         USING ADDBLKD,R5                                                       
         USING EDIDD,R9                                                         
FTRADDR  NTR1  BASE=*,LABEL=*                                                   
         LR    R5,R1                                                            
         MVI   WORK,0              CLEAR FIRST BYTE OF WORK                     
         OC    SRADDR+(1*L'SRADDR),SPACES  MAKE SURE STATE IS UPPERCASE         
         OC    SRADDR+(2*L'SRADDR),SPACES  ON LINES 2,3 OR 4                    
         OC    SRADDR+(3*L'SRADDR),SPACES                                       
         LA    R2,SRADDR          POINT TO ADDRESS                              
         LA    R3,L'SRADDR        LENGTH OF ONE ADDRESS LINE                    
         LA    R4,SRADDR+(3*L'SRADDR) FOURTH ADDRESS LINE                       
*                                                                               
FTRAD10  CR    R2,R4              POINTING TO FIRST LINE?                       
         JE    FTRADERR           MUST BE AN ERROR                              
         GOTO1 ADFORM,DMCB,((R3),(R4)),(25,ADDBCTY),                   +        
               ADDBST,(10,ADDBZIP),(2,ADDBCTRY),(30,ADDBCNME)                   
         ZIC   RF,WORK                                                          
         AHI   RF,1                                                             
         STC   RF,WORK                                                          
         TM    0(R1),X'80'        80=SERIOUS ERROR                              
         JO    FTRAD20                                                          
         CLC   =C'CA',ADDBCTRY                                                  
         JNE   FTRAD12                                                          
         TM    EDIRSTAT,EDICANF    DOES THIS BANK TREAT CANADIAN ADDRS          
         JO    FTRAD40             AS FOREIGN ADDRESSES?                        
         J     *+14                                                             
FTRAD12  CLC   =C'US',ADDBCTRY                                                  
         JNE   FTRAD40             FOREIGN                                      
*        CLI   SRVTYP,VEFT         IS THIS AN EFT VENDOR?                       
*        BE    *+12                                                             
*        TM    DTRSW,DTRANY        DATA TRANSFER (EDI820 OR DATA FILE)          
*        BNO   *+10                                                             
*        MVC   0(L'SRADDR,R4),SPACES  CLEAR OUT LAST ADD LINE (C/S/ZIP)         
         CLC   ADDBCTY,SPACES                                                   
         JNE   FTRAD30                                                          
FTRAD20  CLC   0(L'SRADDR,R4),SPACES IF ZERO, THEN NO ADDRESS LINE              
         JNH   *+8                                                              
         AHI   R3,L'SRADDR        SEND ANOTHER LINE                             
         AHI   R4,-L'SRADDR       BACK UP                                       
         J     FTRAD10                                                          
*                                                                               
FTRAD30  TM    0(R1),X'02'        02=INCORRECT STATE/ZIP COMBO                  
         JO    FTRADERR                                                         
         MVI   BYTE,C'1'           DEFAULT TO CANADIAN ADDR.                    
         CLC   =C'CA',ADDBCTRY                                                  
         JE    *+8                                                              
         MVI   BYTE,C'2'           MUST BE U.S. ADDRESS                         
         MVC   ADDBADD1,0(R2)                                                   
         LA    R2,L'SRADDR(R2)                                                  
         CR    R2,R4                                                            
         JE    FTRAD100                                                         
         MVC   ADDBADD2,0(R2)                                                   
         J     FTRAD100                                                         
*                                                                               
FTRAD40  MVI   BYTE,C'3'           INDICATES FOREIGN ADDRESS                    
         LA    R3,ADDBADD1                                                      
         LHI   R0,4                  COPY ALL FOUR ADRESS LINES                 
FTRAD50  OC    0(L'SRADDR,R2),0(R2)                                             
         JZ    FTRAD100                                                         
         MVC   0(L'ADDBADD1,R3),0(R2)                                           
         LA    R2,L'SRADDR(R2)                                                  
         LA    R3,L'ADDBADD1(R3)                                                
         JCT   R0,FTRAD50                                                       
         J     FTRAD100                                                         
*                                                                               
FTRADERR MVI   BYTE,C'4'           ERROR WITH ADDR-RETURN TO CLT                
*                                                                               
FTRAD100 LHI   R1,4                4 ADDR LINES MAX                             
         ZIC   RF,WORK             WORK HAS # OF ADDR LINES PROCESSED           
         SR    R1,RF                                                            
         STC   R1,WORK                                                          
*                                                                               
FTRADX   XIT1                                                                   
         DROP  R5,R9                                                            
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* READ SC BANK ACCOUNT FOR BANKING INFORMATION                      *           
*********************************************************************           
***                                                                             
*** CAN USE AIO TO READ BUT CAN'T KEEP IT AROUND!                               
***                                                                             
         USING EDIDD,R9                                                         
         USING ACTRECD,RF                                                       
RDSC     NTR1  BASE=*,LABEL=*                                                   
         L     R9,AEDIWRK                                                       
         ST    R9,SVWRKREG                                                      
*                                                                               
         XC    WORK,WORK                                                        
         L     RF,AIO                                                           
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY(ACTKEND),SVBANK  C/U/L/SC ACCT FROM CHECK RECORD         
         MVC   TEMPKEY(ACTKEND),0(RF)                                           
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCFIL,AIO,AIO                               
         L     RF,AIO                                                           
         CLC   0(ACTKEND,RF),TEMPKEY                                            
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO                                                           
         AH    R2,DATADISP                                                      
                                                                                
         MVI   BYTE,0                                                           
RDSC10   CLI   0(R2),0                                                          
         JE    RDSC90                                                           
         CLI   0(R2),CBPELQ        2F-CASH ACCT BANK PROFILE ELEMENT            
         JE    RDSC30                                                           
         CLI   0(R2),FFTELQ        DB-FREEFORM TEXT ELEMENT                     
*MN      JNE   *+12                                                             
         JNE   RDSC20                                                           
         CLI   2(R2),FFTTEDIP                                                   
         JE    RDSC40                                                           
*MN SPEC-46328                                                                  
         CLI   2(R2),FFTTSCTX                                                   
         JE    RDSC50                                                           
*MN SPEC-46328                                                                  
RDSC20   SR    R1,R1                                                            
         IC    R1,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,R1                                                            
         J     RDSC10                                                           
*                                                                               
         USING CBPFELD,R2                                                       
RDSC30   MVC   EDIBNKC,CBPBCDE     BANK/BRANCH CODE (TO READ CFILE)             
         MVC   EDISBNKA,CBPBACC    SOURCE BANK ACCT FROM SC                     
         MVI   BYTE,1              MUST HAVE THIS ELEMENT                       
         J     RDSC20                                                           
         DROP  R2                                                               
*                                                                               
         USING FFTELD,R2                                                        
RDSC40   LA    RE,FFTEFT           IS IT EFT?                                   
         TM    DTRSW,DTR820        ARE WE DOING AN 820?                         
         JNO   *+8                                                              
         LA    RE,FFT820           OR 820?                                      
         SR    R1,R1                                                            
         IC    R1,FFTETYP          MOVE TYPE INTO R1                            
         CR    R1,RE                                                            
         JNE   RDSC20                                                           
         MVC   EDIMSGCL,FFTMSGC    SAVE OFF MESSAGE CLASS                       
         MVC   EDIDSN,FFTDNAM      SAVE OFF DATASET NAME                        
         MVC   EDIFORMT,FFTFKEY    FORMAT KEY                                   
         J     RDSC20                                                           
*        DROP  R2                                                               
*                                                                               
*MN SPEC-46328                                                                  
RDSC50   DS    0H                                                               
         MVC   EDIFFTX1,FFTXSCF1   SAVE SC FREEFORM TEXT FIELD 1                
         MVC   EDIFFTX2,FFTXSCF2   SAVE SC FREEFORM TEXT FIELD 2                
         MVC   EDIFFTX3,FFTXSCF3   SAVE SC FREEFORM TEXT FIELD 3                
         MVC   EDIFFTX4,FFTXSCF4   SAVE SC FREEFORM TEXT FIELD 4                
         MVC   EDIFFTX5,FFTXSCF5   SAVE SC FREEFORM TEXT FIELD 5                
         MVC   EDIFFTX6,FFTXSCF6   SAVE SC FREEFORM TEXT FIELD 6                
         MVC   EDIFFTX7,FFTXSCF7   SAVE SC FREEFORM TEXT FIELD 7                
         MVC   EDIFFTX8,FFTXSCF8   SAVE SC FREEFORM TEXT FIELD 8                
         J     RDSC20                                                           
         DROP  R2                                                               
*                                                                               
*MN SPEC-46328                                                                  
RDSC90   CLI   BYTE,1              DID WE FIND THE CASH ELEMENT?                
         JNE   RDSCNO                                                           
RDSCYES  CR    RC,RC               SET BRANCH TO EQUAL                          
         J     RDSCX                                                            
RDSCNO   SR    R1,R1                                                            
         CR    RC,R1               SET BRANCH TO NOT EQUAL                      
RDSCX    XIT1                                                                   
         DROP  R9,RF                                                            
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* CALL GETBANK ROUTINE TO GET LOWEST LEVEL OF BANKING INFORMATION   *           
* ROUTINE READS THE CFILE BANK RECORD AS WELL AS THE ACC/AFM BANK   *           
* REC                                                                           
*********************************************************************           
         USING BANKD,R3                                                         
         USING EDIDD,R9                                                         
         DS    F                                                                
RDBNKC   NTR1  BASE=*,LABEL=*                                                   
         L     R9,AEDIWRK                                                       
         ST    R9,SVWRKREG                                                      
*                                                                               
         L     RE,AIO              CLEAR IO TO SPACES                           
         LHI   RF,2000                                                          
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0                                                            
*                                                                               
         L     R3,AIO                                                           
         MVC   BANCPY,RCCOMPFL     COMPANY CODE                                 
         MVC   BANORIG,ORIGINUM    ORIGIN NUMBER                                
         MVC   BANCDE,EDIBNKC      BANK CODE                                    
         MVC   BANBRN,EDIBNKC+3    BRANCH CODE                                  
         MVI   ACCSW,0                                                          
         GOTO1 VGETBANK,DMCB,(1,AIO),ADCOMFAC,0                                 
         CLC   BANEFFKY,SPACES                                                  
*SPEC-13701                                                                     
         BNH   *+14                                                             
         OI    ACCSW,EFTON                                                      
* DSFTK-150                                                                     
         MVC   EDIEFADV,BANEFUSR   SET ON BANK RECORD IN ADVN FIELD             
* DSFTK-150                                                                     
         CLC   BAN82FKY,SPACES                                                  
*SPEC-13701                                                                     
         BNH   *+14                                                             
         OI    ACCSW,EDION                                                      
*SPEC-13701                                                                     
         MVC   EDIEFADV,BAN82USR   SET ON BANK RECORD IN ADVN FIELD             
*SPEC-13701                                                                     
*                                                                               
         MVC   EDISROUT,BANRNO     ROUTING NUMBER                               
         MVC   EDIBKNME,BNKNME     BANK NAME                                    
*                                                                               
         NI    EDIFLAG,X'FF'-(EDIFNID+EDIFNNM)                                  
         LA    R5,BANEFFLN         SET UP DISP FOR EFT OVERRIDES                
         TM    DTRSW,DTR820        ARE WE DOING AN 820?                         
         BNO   *+8                                                              
         LA    R5,BAN82FLN         SET UP DISP FOR 820 OVERRIDES                
         OC    0(L'BANDFORG,R5),0(R5) USING ORIGIN ID FOR FILENAME?             
         BZ    *+12                                                             
         OI    EDIFLAG,EDIFNID                                                  
         B     RDBNKC02                                                         
         OC    L'BANDFORG(L'BANDFNME,R5),L'BANDFORG(R5) FILENAME?               
         BZ    RDBNKC02                                                         
         OI    EDIFLAG,EDIFNNM                                                  
*                                                                               
         USING EDITABD,RF                                                       
RDBNKC02 LA    RF,EDITAB           DEFAULT VS OVERRIDE TABLE                    
         LA    R5,BANEFTKY         SET UP DISP FOR EFT OVERRIDES                
         TM    DTRSW,DTR820        ARE WE DOING AN 820?                         
         BNO   *+8                                                              
         LA    R5,BAN82TKY         SET UP DISP FOR 820 OVERRIDES                
         AHI   R3,BANTRNS-BANKD    SET UP DISPLACEMENTS FOR DEFAULTS            
         SR    R1,R1                                                            
RDBNKC10 CLI   0(RF),EOF           END OF TABLE?                                
         BE    RDBNKCX                                                          
         SR    RE,RE                                                            
         ICM   RE,3,EDIFDSP        POINT TO CURRENT EDI RECEIVING FIELD         
         L     R2,SVWRKREG         R2=ADDR OF CURRENT WORKING STORAGE           
         AR    RE,R2                                                            
         ICM   R1,3,EDIFLN         GET RECEIVING LENGTH                         
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),SPACES      IS THERE AN SC OVERRIDE                      
         BH    RDBNKC50                ALREADY IN THE FIELD?                    
         SR    R4,R4                                                            
         ICM   R4,3,EDIDDSP        OVERRIDER FIELD                              
         AR    R4,R5                                                            
*                                                                               
         TM    EDISTAT,EDISZERO    ARE WE COMPARING ON ZERO?                    
         BNO   RDBNKC20                                                         
         CLI   0(R4),0                                                          
         BE    RDBNKC30                                                         
         B     RDBNKC40                                                         
*                                                                               
RDBNKC20 EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),SPACES      ANYTHING IN THE OVERRIDE FIELD?              
         BH    *+12                                                             
RDBNKC30 SR    R4,R4                                                            
         ICM   R4,3,EDIDDSP        DEFAULT FIELD                                
         AR    R4,R3                                                            
RDBNKC40 EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R4)       MOVE IN DATA                                 
*                                                                               
RDBNKC50 AHI   RF,EDITABLN                                                      
         B     RDBNKC10                                                         
*                                                                               
RDBNKCX  CLI   EDITRNTY,EDITMQ     IS THIS AN MQ REQUEST?                       
         BNE   *+8                                                              
         OI    DTRSW,DTRMQ         SET MQ DATA TRANSFER                         
*MQ                                                                             
         TM    RNSW2,RUNTST        FOR REGRESSION TESTING MQ RUNS               
         BNO   *+8                                                              
         NI    DTRSW,X'FF'-DTRMQ                                                
*MQ                                                                             
         XIT1                                                                   
         DROP  R3,RF                                                            
         EJECT                                                                  
**********************************************************************          
* DEFAULT/OVERRIDE TABLE                                             *          
**********************************************************************          
EDITAB   DS    0C                                                               
         DC    AL2(EDIFORMT-EDIDD,L'EDIFORMT,BANDFFKY-BANTRNS),X'00'            
         DC    AL2(EDIDSN-EDIDD,L'EDIDSN,BANDFDSN-BANTRNS),X'00'                
         DC    AL2(EDITRNKY-EDIDD,L'EDITRNKY,BANDFTKY-BANTRNS),X'00'            
         DC    AL2(EDIADVID-EDIDD,L'EDIADVID,BANDFUSR-BANTRNS),X'00'            
         DC    AL2(EDIADVAC-EDIDD,L'EDIADVAC,BANDFACN-BANTRNS),X'00'            
         DC    AL2(EDIMSGCL-EDIDD,L'EDIMSGCL,BANDFCLS-BANTRNS),X'00'            
         DC    AL2(EDICHRG-EDIDD,L'EDICHRG,BANDFCHR-BANTRNS),X'00'              
         DC    AL2(EDITRNTY-EDIDD,L'EDITRNTY,BANDFTYP-BANTRNS),X'80'            
         DC    AL1(EOF)                                                         
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* CALL GETFORM ROUTINE TO GET BANK FORMAT FOR AGENCY                *           
* ON EXIT R4 WILL CONTAIN THE FORMAT BLOCK                                      
*********************************************************************           
         USING FORMD,R3                                                         
         USING EDIDD,R9                                                         
RDFORMC  NTR1  BASE=*,LABEL=*                                                   
         L     R9,AEDIWRK                                                       
         ST    R9,SVWRKREG                                                      
*                                                                               
         L     RE,AFRMBLK          CLEAR BNKBLK TO SPACES                       
         LHI   RF,L'FRMBLK                                                      
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0                                                            
*                                                                               
         L     R3,AFRMBLK                                                       
         MVC   FORMCPY,RCCOMPFL    COMPANY CODE                                 
         MVC   FORMBCDE,EDIBNKC    BANK CODE                                    
         MVC   FORMFRM,EDIFORMT    FORMAT CODE                                  
         OC    FORMFRM,SPACES      DON'T LEAVE ANY BINARY ZEROES                
         GOTO1 VGETFORM,DMCB,(X'01',AFRMBLK),ADCOMFAC,0                         
         MVC   EDIRECLN,FFRMRLN    RECORD LENGTH                                
         MVC   EDIBLKSZ,FFRMBSZ    BLOCK SIZE                                   
         MVC   EDIRSTAT,FFRMRST    RECORD STATUS                                
         XIT1                                                                   
         DROP  R3,R9                                                            
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ALLOCATE DATA SET FOR FILE TRANSFER (EFT AND EDI820)               *          
**********************************************************************          
         USING DSNMD,R3                                                         
         USING EDIDD,R9                                                         
FTROPEN  NTR1  BASE=*,LABEL=*                                                   
         L     R9,AEDIWRK                                                       
*        TM    RQSW,RQPQ           PUT TO PQ INSTEAD OF TAPE?                   
*        BO    RBCOPX              THAN NO NEED TO ALLOCATE                     
* DSFTK-150                                                                     
         TM    EDIRSTAT,EDISPCL    SPECIAL ROUTINE?                             
         BZ    *+14                                                             
         MVC   DYDCB,=CL8'FTROUTV'                                              
         B     FTRO05                                                           
* DSFTK-150                                                                     
         MVC   DYDCB,=CL8'FTROUT'    DEFAULT TO FTROUT                          
         TM    EDIRSTAT,EDIVRLN    VARIABLE RECORD FORMAT?                      
         BZ    FTRO05                                                           
         MVC   DYDCB,=CL8'EDIOUT'                                               
FTRO05   LA    R3,DYTDSN                                                        
*                                                                               
         TM    DTRSW,DTRMQ         MQ ENTRIES MUST HAVE DSN OVERRIDE            
         BNO   FTRO10                                                           
         LA    R3,DYDDSN                                                        
         TM    RNSW2,RUNTST        RUN=TEST                                     
         BNO   *+10                                                             
         MVC   DSNDENV,=C'TEST.'   CHANGE FROM PROD TO TEST                     
         TM    DTRSW,DTRNEFT       IS THIS AN EFT TRANSMISSION                  
         BNO   *+10                                                             
         MVC   DSNDTRNT,=C'EFT.'                                                
         GOTO1 DATCON,DMCB,(1,TODAY1),(X'20',WORK)                              
         MVC   DSNDDTE(6),WORK     820 FILE.YYMMDD.HHMMSS                       
         TIME  DEC,DUB,LINKAGE=SYSTEM                                           
         UNPK  WORK(15),DUB                                                     
         MVC   DSNDTME(7),WORK                                                  
         B     FTRO10                                                           
*                                                                               
FTRO10   CLC   EDIDSN(L'SPACES),SPACES   DSN FOUND AT BANK LEVEL?               
         BNH   FTRO20                                                           
         LA    R1,L'DSNVAR                                                      
         LA    RE,DSNVAR                                                        
         TM    DTRSW,DTRMQ         DONT ERASE ACCDISK.FTP.                      
         BNO   *+12                                                             
         LA    R1,L'DSNDVAR                                                     
         LA    RE,DSNDVAR                                                       
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SPACES                                                   
         MVC   0(L'EDIDSN,RE),EDIDSN   YES SO GET IT FROM THERE                 
         B     FTRO50                                                           
*                                                                               
FTRO20   LA    R5,DSNDUSID         USER ID FOR DISK                             
         LA    RF,DSNDVAR                                                       
         TM    DTRSW,DTRMQ         ARE WE MQING THIS?                           
         BO    FTRO25                                                           
         LA    R5,DSNUSID          USER ID FOR TAPES                            
         LA    RF,DSNVAR2                                                       
         MVC   DSNALPH,ALPHAID     ALPHA ID                                     
FTRO25   MVC   0(L'DSNDVAR,RF),SPACES                                           
         LHI   R1,L'IDABBR         MAX LENGTH OF USER ID                        
         LA    RE,IDABBR+L'IDABBR-1 POINT TO LAST BYTE OF USER ID               
FTRO30   CLI   0(RE),C' '                                                       
         BH    FTRO40                                                           
         BCTR  RE,0                                                             
         BCT   R1,FTRO30                                                        
FTRO40   LR    RF,R5                                                            
         BCTR  R1,0                                                             
         EXMVC R1,0(RF),IDABBR     FILL IN USER ID                              
         AHI   R1,1                BUMP UP FOR REAL LENGTH                      
         AR    RF,R1               BUMP TO NEXT SPOT                            
         MVI   0(RF),C'.'                                                       
         MVC   1(1,RF),SRACC+2       LEDGER                                     
*                                                                               
FTRO50   TM    DTRSW,DTRMQ         ARE WE MQING THIS?                           
         BNO   FTRO70                                                           
         MVC   WORK(DSNLNQ),DSNACC   ADJUST FOR SPACES                          
         MVC   DSNACC(DSNLNQ),SPACES                                            
         LA    R0,DSNLNQ                                                        
         LA    RE,DSNACC                                                        
         LA    RF,WORK                                                          
FTRO60   CLI   0(RF),X'40'         DONT MOVE IN SPACES                          
         BNH   *+14                                                             
         MVC   0(1,RE),0(RF)                                                    
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         BCT   R0,FTRO60                                                        
         DROP  R3                                                               
*                                                                               
FTRO70   SR    R1,R1                                                            
         ICM   R1,3,EDIBLKSZ       VARIABLE BLOCK SIZE                          
         BNZ   *+6                                                              
         DC    H'0'                NOT TAPE OUTPUT                              
*MOFAC-638                                                                      
         USING MASTD,RF                                                         
         L     RF,ADMASTC          IF RUNNING ON CSC DO NOT WANT TO             
         USING SSOOFF,RE           CREATE A PRODUCTION TAPE                     
         ICM   RE,15,MCSSB                                                      
         BZ    FTRO71                                                           
*                                                                               
         CLI   SSODSPAC,C'C'                                                    
         BNE   FTRO70A                                                          
         TM    RNSW,SOON           ALLOWS TESTING THROUGH TSO                   
         BZ    FTRO71                                                           
         MVC   DYTDSN1,=C'CSCTAPE.'                                             
         MVC   DYTDSN2(8),=C'AC055DDS'                                          
         TM    EDIRSTAT,EDIVRLN    VARIABLE RECORD FORMAT?                      
         BZ    *+10                                                             
         MVC   DYTDSN2(8),=C'AC055DDV'                                          
         TM    EDIRSTAT,EDISPCL    SPECIAL ROUTINE?                             
         BZ    *+10                                                             
         MVC   DYTDSN2(8),=C'AC055DDV'                                          
         B     FTRO71                                                           
*                                                                               
FTRO70A  CLI   SSODSPAC,C'Q'                                                    
         BNE   FTRO70B                                                          
         MVC   DYTDSN1,=C'FQATAPE.'                                             
         MVC   DYTDSN2(8),=C'AC055DDS'                                          
         TM    EDIRSTAT,EDIVRLN    VARIABLE RECORD FORMAT?                      
         BZ    *+10                                                             
         MVC   DYTDSN2(8),=C'AC055DDV'                                          
         TM    EDIRSTAT,EDISPCL    SPECIAL ROUTINE?                             
         BZ    *+10                                                             
         MVC   DYTDSN2(8),=C'AC055DDV'                                          
         B     FTRO71                                                           
*                                                                               
FTRO70B  CLI   SSODSPAC,C'T'                                                    
         BNE   FTRO71                                                           
*MQ                                                                             
         TM    RNSW,SOON           ALLOWS TESTING THROUGH TSO                   
         BZ    FTRO71                                                           
         TM    DTRSW,DTRMQ         ARE WE MQING THIS?                           
         BO    FTRO71                                                           
*MQ                                                                             
         MVC   DYTDSN1,=C'TSTTAPE.'                                             
         MVC   DYTDSN2(8),=C'AC055DDS'                                          
         TM    EDIRSTAT,EDIVRLN    VARIABLE RECORD FORMAT?                      
         BZ    *+10                                                             
         MVC   DYTDSN2(8),=C'AC055DDV'                                          
         TM    EDIRSTAT,EDISPCL    SPECIAL ROUTINE?                             
         BZ    *+10                                                             
         MVC   DYTDSN2(8),=C'AC055DDV'                                          
*MOFAC-638                                                                      
* DSFTK-150                                                                     
FTRO71   TM    EDIRSTAT,EDISPCL    SPECIAL ROUTINE?                             
         BZ    FTRO72                                                           
         L     RE,=A(FTROUTV)                                                   
         B     FTRO75                                                           
* DSFTK-150                                                                     
FTRO72   L     RE,=A(FTROUT)       DEFAULT TO FTROUT                            
         TM    EDIRSTAT,EDIVRLN    VARIABLE RECORD FORMAT?                      
         BZ    FTRO75                                                           
         L     RE,=A(EDIOUT)                                                    
FTRO75   STH   R1,62(RE)           DTF BLOCKSIZE                                
         ICM   R1,3,EDIRECLN       VARIABLE RECORD LENGTH                       
         BNZ   *+6                                                              
         DC    H'0'                NOT TAPE OUTPUT                              
         STH   R1,82(RE)           RECORD LENGTH                                
         MVI   BYTE,1              GENERATION +1                                
*                                                                               
FTRO80   DS    0H                                                               
* DSFTK-150                                                                     
* NEED TO PUT A TABLE OF BLURBS AND ROUTINE ADDRESSES IN THIS                   
* PROGRAM - BLURB PASSED BACK FROM GETFORM TO POINT TO ROUTINE                  
* ADDRESS IN THIS PROGRAM                                                       
*                 CHSCSV           OPEN DIFFERENT FILE                          
*                                                                               
         TM    EDIRSTAT,EDISPCL    SPECIAL ROUTINE?                             
         BZ    FTRO85                                                           
         GOTO1 DYNALLOC,DMCB,(X'80',=CL8'CHASEIN'),                    X        
               (X'40',=AL3(10,10)),0                                            
         OPEN  (CHASEIN,OUTPUT)                                                 
* DSFTK-150                                                                     
*                                                                               
FTRO85   TM    DTRSW,DTRMQ         ARE WE MQING THIS?                           
         BNO   FTRO90                                                           
                                                                                
         MVI   BYTE,X'45'              CYLINDER (X'40')                         
*                                      CL44 DSN (X'04')                         
*                                      3RD PARM (X'01')                         
         MVC   DUB,=X'000005000001'    PRI=5,SEC=1                              
         GOTO1 DYNALLOC,DMCB,(X'80',DYDCB),(BYTE,DUB),(X'80',(R3))              
         B     FTRO100                                                          
*                                                                               
FTRO90   GOTO1 DYNALLOC,DMCB,(0,DYDCB),(X'FE',(R3)),(X'80',BYTE)                
*                                                                               
FTRO100  DS    0H                                                               
* DSFTK-150                                                                     
         TM    EDIRSTAT,EDISPCL    SPECIAL ROUTINE?                             
         BO    FTRO130                                                          
* DSFTK-150                                                                     
FTRO105  TM    EDIRSTAT,EDIVRLN    VARIABLE RECORD FORMAT?                      
         BO    FTRO110                                                          
         OPEN  (FTROUT,OUTPUT)                                                  
         B     FTRO120                                                          
FTRO110  OPEN  (EDIOUT,OUTPUT)                                                  
FTRO120  LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FTRO130  TM    QOPT6,C'T'          PUT TO DATA SET INSTEAD OF TAPE?             
         BO    FTROPX              THAN NO GENERATION # NEEDED.                 
         TM    DTRSW,DTRMQ         IF MQING - DATASET IS STATIC                 
         BO    FTROPX                                                           
         GOTO1 DYNALLOC,DMCB,(C'D',DYDCB),RTDSN   FULLY-QUALIFIED DSN           
         CLI   DMCB+4,0                                                         
         JE    *+2                 UNSUCCESSFUL DSN RETRIEVAL !?!               
FTROPX   XIT1                                                                   
         DROP  R9                                                               
*********************************************************************           
* LITERALS                                                          *           
*********************************************************************           
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* WRITE OUT EDICT HEADER TO THE PRINT QUE                           *           
*********************************************************************           
         USING EDIDD,R9                                                         
HDRPQ    NTR1  BASE=*,LABEL=*                                                   
         L     R9,AEDIWRK                                                       
         ST    R9,SVWRKREG                                                      
*                                                                               
         TM    RNSW2,RUNTST        RUN=TEST                                     
         BO    HDRPQX                                                           
*                                                                               
         CLC   EDITRNKY,SPACES         THESE WILL BE MANUALLY SENT TO A         
         BNH   HDRPQX                  DIFFERENT INBOX                          
*                                                                               
         TM    RNSW2,WRTNO                                                      
         BO    HDRPQX                  AN EDICT HEADER                          
*                                                                               
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         L     R5,ADMASTD                                                       
         USING MASTD,R5                                                         
         L     R4,REMOTEC                                                       
         USING REMOTED,R4                                                       
         MVC   SVREMPQK,MCREMPQK                                                
         MVC   FULL(L'REMOTJID),REMOTJID  SAVE PQ INITIALS FOR 56               
         XC    MCREMPQK,MCREMPQK       CLEAR THIS TO GET NEW REPORT             
         MVC   REMOTPRG,=C'65'         **WHAT DO WE CALL IT?                    
         MVC   REMOTFRM,=C'1S  '                                                
         MVI   REMOTCLS,C'G'                                                    
         MVI   REMOTSYS,C'A'                                                    
         MVC   REMOTDST,ORIGINUM                                                
         MVC   REMOTJID(3),=C'A65'                                              
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P+4(5),=C'*HDR*'                                                 
         MVC   P+9(6),=C'EDICT='            EDICT KEY                           
         MVC   P+15(7),EDITRNKY                                                 
         MVI   P+34,C'W'                     WIDE                               
         MVI   P+37,C'D'                     INDICATES DATASET                  
         GOTO1 PRINT,DMCB,P,=C'BL01'                                            
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P(5),=C'++DDS'                                                   
         MVC   P+6(5),=C'ACCXX'                                                 
         MVC   P+9(2),ALPHAID                                                   
         MVC   P+11(3),=C'TRN'                                                  
         GOTO1 PRINT,DMCB,P,=C'BL01'                                            
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P(5),=C'++DDS'                                                   
         MVC   P+11(3),=C'DSN'                                                  
         MVC   P+15(44),RTDSN                                                   
         GOTO1 PRINT,DMCB,P,=C'BL01'                                            
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P(5),=C'++DDS'                                                   
         MVC   P+11(3),=C'FIL'                                                  
         MVC   P+15(8),=C'EFTFILE.'    FILE NAME                                
         GOTO1 DATCON,DMCB,(1,TODAY1),(X'20',WORK)                              
         CLC   ALPHAID,=C'I8'                                                   
         BE    HDRPQ10                                                          
*        CLC   =C'YRYTD',IDABBR                                                 
*        BE    HDRPQ12                                                          
*        CLC   =C'YRYQD',IDABBR                                                 
*        BE    HDRPQ12                                                          
         TM    EDIFLAG,EDIFNID+EDIFNNM                                          
         BNZ   HDRPQ15                                                          
         MVC   P+23(6),WORK            EFTFILE.YYMMDD.HHMMSS.NNNN               
         MVI   P+29,C'.'               WHERE NNNN IS THE GEN #                  
         TIME  DEC,DUB,LINKAGE=SYSTEM                                           
         UNPK  WORK(15),DUB                                                     
         MVC   P+30(6),WORK                                                     
         MVI   P+36,C'.'                                                        
         USING ACBANKD,RF                                                       
         L     RF,AFTROUTB                                                      
         MVC   P+37(4),ACBFSEQ#+5  GET GEN # FROM ACBANKD FIELD                 
         B     HDRPQ20                                                          
*                                                                               
HDRPQ10  MVC   P+15(11),=C'IM_EFTFILE.'    FILE NAME                            
         MVC   P+26(6),WORK            IM_EFTFILE.YYMMDD.HHMMSS.NNNN            
         MVI   P+32,C'.'               WHERE NNNN IS THE GEN #                  
         TIME  DEC,DUB,LINKAGE=SYSTEM                                           
         UNPK  WORK(15),DUB                                                     
         MVC   P+33(6),WORK                                                     
         MVI   P+39,C'.'                                                        
         USING ACBANKD,RF                                                       
         L     RF,AFTROUTB                                                      
         MVC   P+40(4),ACBFSEQ#+5  GET GEN # FROM ACBANKD FIELD                 
         B     HDRPQ20                                                          
*                                                                               
*&&DO                                                                           
HDRPQ12  MVC   P+15(5),IDABBR           YRYTD OR YRYQD                          
         MVC   P+20(9),=C'_EFTFILE.'                                            
         MVC   P+29(6),WORK            YRYTD_EFTFILE.YYMMDD.HHMMSS.NNNN         
         MVI   P+35,C'.'               WHERE NNNN IS THE GEN #                  
         TIME  DEC,DUB,LINKAGE=SYSTEM                                           
         UNPK  WORK(15),DUB                                                     
         MVC   P+36(6),WORK                                                     
         MVI   P+42,C'.'                                                        
         USING ACBANKD,RF                                                       
         L     RF,AFTROUTB                                                      
         MVC   P+43(4),ACBFSEQ#+5  GET GEN # FROM ACBANKD FIELD                 
         B     HDRPQ20                                                          
*&&                                                                             
*                                                                               
* IF USING FILENAME OPTION ON AFM BANK RECORD THEN FILENAME IS EITHER:          
* A55_USERID_YYMMDD.HHMMSS.NNNN                                                 
* A55_USERNAME_YYMMDD.HHMMSS.NNNN                                               
*                                                                               
HDRPQ15  MVC   P+15(4),=C'A55_'                                                 
         MVC   AREA(100),SPACES       BUILD REST OF FILENAME IN AREA            
         TIME  DEC,DUB,LINKAGE=SYSTEM                                           
         UNPK  WORK+10(15),DUB                                                  
         TM    EDIFLAG,EDIFNID        USING ORIGIN ID NAME?                     
         BZ    *+14                                                             
         MVC   AREA(L'IDABBR),IDABBR                                            
         B     *+10                                                             
         MVC   AREA(L'BANDFNME),EDIORGN  MUST BE USING ORIGIN NAME              
         GOTO1 ADSQUASH,DMCB,AREA,100                                           
         L     RF,DMCB+4              RF=LENGTH OF ID OR NAME                   
         LA    RE,P+19                                                          
         SHI   RF,1                                                             
         EXMVC RF,0(RE),AREA          MOVE ID OR NAME TO PRINT LINE             
         AHI   RF,1                   BUMP BACK UP FOR REAL LENGTH              
         AR    RE,RF                  POINT PAST THE NAME                       
         MVI   0(RE),C'_'                                                       
         AHI   RE,1                                                             
         MVC   0(6,RE),WORK              WORK HAS THE DATE                      
         MVI   6(RE),C'.'                                                       
         MVC   7(6,RE),WORK+10           WORK+10 HAS THE TIME                   
         MVI   13(RE),C'.'                                                      
         USING ACBANKD,RF                                                       
         L     RF,AFTROUTB                                                      
         MVC   14(4,RE),ACBFSEQ#+5  GET GEN # FROM ACBANKD FIELD                
*                                                                               
*&&DO                                                                           
         LA    RF,AREA+L'EDIORGN                                                
         MVI   0(RF),C'_'                                                       
         AHI   RF,1                                                             
         MVC   1(6,RF),WORK              WORK HAS THE DATE                      
         MVI   7(RF),C'.'                                                       
         MVC   8(6,RF),WORK+10           WORK+10 HAS THE TIME                   
         MVI   14(RF),C'.'                                                      
         USING ACBANKD,RF                                                       
         L     RF,AFTROUTB                                                      
         MVC   15(4,RF),ACBFSEQ#+5  GET GEN # FROM ACBANKD FIELD                
         GOTO1 ADSQUASH,DMCB,AREA,100                                           
         L     RF,DMCB+4                                                        
         BCTR  RF,0                                                             
         EXMVC RF,P+19,AREA                                                     
*&&                                                                             
         DROP  RF                                                               
*                                                                               
HDRPQ20  GOTO1 PRINT,DMCB,P,=C'BL01'                                            
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P(5),=C'++DDS'                                                   
         MVC   P+11(3),=C'EXT'                                                  
         MVC   P+15(3),=C'TXT'  EXTENSION TYPE .TXT                             
         GOTO1 PRINT,DMCB,P,=C'BL01'                                            
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P(5),=C'++DDS'                                                   
         MVC   P+11(8),=C'SUB CLA('                                             
         MVC   P+19(L'EDIMSGCL),EDIMSGCL MESSAGE CLASS                          
         MVC   P+27(38),=C'),CHA( ),ACC(    ),USE(       ),MOD( )'              
         MVC   P+33(L'EDICHRG),EDICHRG     CHARGE                               
         MVC   P+40(L'EDIADVAC),EDIADVAC ADVANTIS ACCOUNT                       
         MVC   P+50(L'EDIADVID),EDIADVID ADVANTIS USER ID                       
         MVC   BYTE,EDITRNMD                                                    
         OI    BYTE,X'F0'                                                       
         MVC   P+63(1),BYTE     TRANSMISSION MODE                               
         GOTO1 PRINT,DMCB,P,=C'BL01'                                            
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P(40),=C'*TAPE GENERATED, SENDING DATA VIA EDICT*'               
         MVI   P+43,X'5E'        MOVE IN SEMI COLON FOR EDICT SCAN              
         GOTO1 PRINT,DMCB,P,=C'BL01'                                            
         MVC   P,SPACES            CLEAR PRINTLINE FOR A56                      
         MVC   MCREMPQK,SVREMPQK   RESTORE                                      
         MVC   REMOTJID,FULL       RESTORE PQ INITALS FOR A56                   
HDRPQX   XIT1                                                                   
         DROP  R4,R9                                                            
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* WRITE OUT MQ HEADER TO THE MQ FILE                                *           
*********************************************************************           
         USING EDIDD,R9                                                         
MQRPQ    NTR1  BASE=*,LABEL=*                                                   
         L     R9,AEDIWRK                                                       
         ST    R9,SVWRKREG                                                      
*                                                                               
*MQ                                                                             
         TM    RNSW2,RUNTST        RUN=TEST                                     
         BO    MQRPQX                                                           
*MQ                                                                             
*                                                                               
         TM    UPSI,NOMPQ          DO WE WANT TO SEND MESSAGES?                 
         BO    MQRPQX                                                           
*                                                                               
         GOTO1 VMQRPT,DMCB,(0,=C'OPEN'),(0,MQFILID),(X'E0',0),0                 
         CLI   DMCB+8,0            IF MQ OPEN FAILS                             
         BE    *+14                                                             
         MVC   AUTOREAS,OPENFAIL                                                
         B     MQERREND            SEND EMAIL NOTIFICATION AND DUMP             
*                                                                               
         MVC   MQMRECTY,EDITRNKY   MQ MESSAGE (TRANS KEY FROM BANK REC)         
         MVC   MQMDSN(RTLNQ),SPACES                                             
         MVC   MQMDSN(DSNDSLNQ),DYDDSN3     SKIP SFTPDISK.PROD.                 
*                                                                               
         MVC   MQDATE,DYDDTE-2     DSECT DSNMD IS NOT IN SYNC WITH              
         MVC   MQTIME,DYDTIM-2     DYDDSN - TWO OFF                             
*                                                                               
         GOTO1 VMQRPT,DMCB,(0,=C'PUT'),MQMESS,MQMLNQ,0                          
         CLI   DMCB+8,0            IF MQ PUT FAILS                              
         BE    *+14                                                             
         MVC   AUTOREAS,PUTFAIL                                                 
         B     MQERREND            SEND EMAIL NOTIFICATION AND DUMP             
*                                                                               
*SPEC-13701                                                                     
         CLC   EDIEFADV(3),=C'MQ2' SENDING SECOND COPY TO SECOND LOC?           
         BNE   MQRPQ20                                                          
*                                                                               
         GOTO1 VMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
         CLI   DMCB+8,0            IF MQ PUT FAILS                              
         BE    *+14                                                             
         MVC   AUTOREAS,CLOSFAIL                                                
         B     MQERREND            SEND EMAIL NOTIFICATION AND DUMP             
*                                                                               
         GOTO1 VMQRPT,DMCB,(0,=C'OPEN'),(0,MQFILID),(X'E0',0),0                 
         CLI   DMCB+8,0            IF MQ OPEN FAILS                             
         BE    *+14                                                             
         MVC   AUTOREAS,OPENFAIL                                                
         B     MQERREND            SEND EMAIL NOTIFICATION AND DUMP             
*                                                                               
         MVC   MQMRECTY,SPACES      CLEAR                                       
         MVC   MQMRECTY,EDIEFADV+3  SECOND DEST FOR SAME FILE                   
         GOTO1 VMQRPT,DMCB,(0,=C'PUT'),MQMESS,MQMLNQ,0                          
         CLI   DMCB+8,0            IF MQ PUT FAILS                              
         BE    *+14                                                             
         MVC   AUTOREAS,PUTFAIL                                                 
         B     MQERREND            SEND EMAIL NOTIFICATION AND DUMP             
*                                                                               
*SPEC-13701                                                                     
*                                                                               
MQRPQ20  GOTO1 VMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
         CLI   DMCB+8,0            IF MQ PUT FAILS                              
         BE    MQRPQX                                                           
         MVC   AUTOREAS,CLOSFAIL                                                
*                                                                               
MQERREND GOTO1 DATAMGR,DMCB,=C'OPMSG',(=AL1(AUTOLENG),AUTONOTE)                 
         DC    H'00'                                                            
*                                                                               
MQRPQX   XIT1                                                                   
         DROP  R9                                                               
*********************************************************************           
* MQ CONSTANTS                                                      *           
*********************************************************************           
MQFILID  DC    CL16'PAYCHECK********'                                           
*MQFILID  DC    CL16'CHECKPAY********'                                          
AUTONOTE DC    C'AUTONOTE*MNAS,JSHA,AHYD'                                       
AUTOREAS DS    CL15                                                             
AUTOLENG EQU   *-AUTONOTE                                                       
OPENFAIL DC    CL(L'AUTOREAS)'MQ OPEN FAILED'                                   
PUTFAIL  DC    CL(L'AUTOREAS)'MQ PUT ERROR'                                     
CLOSFAIL DC    CL(L'AUTOREAS)'MQ CLOSE FAILED'                                  
SETUFAIL DC    CL(L'AUTOREAS)'NOT SETUP TO MQ'                                  
*********************************************************************           
* MQ TABLES                                                         *           
*********************************************************************           
MQMESS   DS    0C                                                               
         DC    CL6'DANOT1'         RECORD TYPE                                  
         DC    CL3'ACC'            MQ KEY                                       
MQMRECTY DC    CL4'TEST'                                                        
         DC    CL8'BILLING '                                                    
         DC    CL8'        '                                                    
MQDATE   DC    CL6' '                                                           
MQTIME   DC    CL6' '                                                           
         DC    CL64' '             EMPTY                                        
MQMDSN   DC    CL128' '            DATASET NAME                                 
MQMLNQ   EQU   *-MQMESS                                                         
         DC    AL1(EOF)                                                         
**********************************************************************          
* CONSTANTS                                                          *          
* THIS WILL BE THE DEFAULT DATASET NAME IF THERE IS NO OVERRIDE FOUND*          
* ON THE AFM/BANK RECORD.                                            *          
**********************************************************************          
*YDCB    DC    CL8'FTROUT'                                                      
DYDCB    DC    CL8'      '         EITHER FTROUT OR EDIOUT BASED ON             
DYTDSN   DS    0C                  COMPANY.                                     
DYTDSN1  DC    CL8'ACCTAPE.'                                                    
DYTDSN2  DC    0CL18                                                            
         DC    CL4'EFT.'           TRNSMISSION TYPE                             
         DC    C'A'                A - ALPHA ID PREFIX                          
         DC    CL2'AA'             AA=ALPHA                                     
         DC    C'.'                                                             
         DC    CL8'UUUUUUUU'       U'S=USER ID,                                 
         DC    C'.'                                                             
         DC    C'L'                L=LEDGER                                     
DYTLNQ   DC    CL(DSNLNQ-(*-DYTDSN))' '                                         
*                                                                               
DYDDSN   DS    0C                                                               
DYDDSN1  DC    CL9'SFTPDISK.'                                                   
DYDDSN2  DC    CL5'PROD.'          PROD OR TEST BASED ON THE RUN                
DYDDSN3  DC    CL1'E'              E=EDI                                        
DYDDSN4  DC    CL4'820.'           TRNSMISSION TYPE                             
DYDDSN5  DC    CL10'UUUUUUUU.L'    U'S=USER ID,L=LEDGER                         
         DC    C'.'                .                                            
         DC    C'D'                D PREFIX FOR TODAY'S DATE                    
DYDDTE   DC    CL6' '              TODAY'S DATE W/  D PREFIX                    
         DC    C'.'                .                                            
         DC    C'T'                T PREFIX FOR CURRENT TIME                    
DYDTIM   DC    CL7' '              CURRENT TIME                                 
*                                                                               
RTDSN    DC    CL25' '                                                          
RTGEN    DC    CL8' '        G0001V00                                           
RTND     DC    CL19' '       SPARE                                              
RTLNQ    EQU   *-RTDSN                                                          
*                                                                               
RTDSNTST DC    CL25'ACCDISK.SONY.EFT.G0007V00'                                  
RTGENTST DC    CL8' '        G0001V00                                           
RTNDTST  DC    CL19' '       SPARE                                              
RTTSTLNQ EQU   *-RTDSNTST                                                       
*                                                                               
DDPARM   DC    CL8'EDIOUT'             MINDSHARE EDI 820 OUTPUT                 
DSPARM   DC    CL20'ACCTAPE.AC055H71'                                           
*                                                                               
         EJECT                                                                  
*********************************************************************           
* LITERALS                                                          *           
*********************************************************************           
         LTORG                                                                  
         EJECT                                                                  
*******************************************************************             
* EXTRA DATA                                                      *             
*  R4=A(4 BYTE ESCAPE SEQUENCE)                                   *             
*  R2=A(OUTPUT AREA)                                              *             
*  FULL CONTAINS THE ADDRESS OF THE DATA TABLE FOR THE SPECIFIC   *             
*  BANK                                                           *             
*******************************************************************             
XTRA     NTR1  BASE=*,LABEL=*                                                   
         ST    R2,RECNXT                                                        
         SR    R1,R1                                                            
         IC    R1,1(R4)             R1=LENGTH OF OUTPUT DATA                    
         STH   R1,HALF                                                          
         SR    R3,R3                                                            
         ICM   R3,3,2(R4)           R3=EQU OF DATA TO PROCESS                   
         BCTR  R3,0                                                             
         SLL   R3,2                                                             
         L     R5,FULL              START OF DATA TABLE                         
         AR    R5,R3                INDEX INTO DATA TABLE                       
         L     RF,0(R5)             RF=A(SOURCE)                                
* REMOVED THIS CODE B/C THERE ARE NO ENTRIES WITH THE 80 BIT ON AND             
* IT WAS CAUSING A PROBLEM W/ THE NARRATIVE WHICH HAS A LENGTH OF 200           
* WHICH IS X'C8' - WHICH TURNS ON THE 80 BIT.                                   
*        TM    0(R5),X'80'                                                      
*        BNO   XTRA5                                                            
*        BASR  RE,RF                RF=A(ROUTINE)                               
*        BCTR  R1,0                                                             
*        B     XTRA7                                                            
*                                                                               
XTRA5    BCTR  R1,0                                                             
         EX    R1,*+8              DATA TO OUTPUT RECORD                        
         B     *+10                                                             
         MVC   0(0,R2),SPACES                                                   
*                                                                               
         L     RE,SVWRKREG         RE=ADDR OF CURRENT WORKING STORAGE           
         AR    RF,RE               ADD DISPLACEMENT OF DATA INTO E820W          
         CLI   0(RF),0             BINARY ZEROS?                                
         BE    XTRA7               YES, DON'T MOVE IT                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,0(R5)            RF=LENGTH OF XDATA                           
         BCTR  RE,0                                                             
         CR    RE,R1                                                            
         BL    *+6                                                              
         LR    RE,R1               USE SHORTEST LENGTH                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)                                                    
*                                                                               
XTRA7    LA    R2,1(R1,R2)                                                      
         ST    R2,RECNXT           UPDATE A(NEXT BYTE)                          
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,RECLEN         UPDATE LENGTH                                
         LA    R3,1(R1,R3)                                                      
         STCM  R3,3,RECLEN                                                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* READ SJ RECORD TO GET NAME                                         *          
* ON ENTRY WORK CONTAINS EITHER THE CLIENT, PRODUCT OR JOB CODE      *          
* ON EXIT WORK CONTAINS THE CLIENT,PRODUCT OR JOB NAME               *          
**********************************************************************          
         USING ACTRECD,R2                                                       
SJNAM    NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIO                                                           
         MVC   ACTRECD,SPACES                                                   
         MVC   ACTKCPY,QCOMPANY                                                 
         MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),=C'SJ'                              
         MVC   ACTKACT,WORK                                                     
         MVC   TEMPKEY(ACTKEND),0(R2)                                           
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCFIL,AIO,AIO                               
         CLC   0(L'ACTKCULA,R2),TEMPKEY                                         
         BNE   SJNAMX                                                           
*                                                                               
         L     R3,AIO                                                           
         AH    R3,DATADISP                                                      
*                                                                               
SJNAM10  CLI   0(R3),0                                                          
         BE    SJNAMX                                                           
         CLI   0(R3),X'20'                                                      
         BE    SJNAM20                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     SJNAM10                                                          
*                                                                               
         USING NAMELD,R3                                                        
SJNAM20  MVC   WORK,SPACES                                                      
         ZIC   R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)    HEADER+1 FOR EXMVC                           
         EXMVC R1,WORK,NAMEREC                                                  
*                                                                               
SJNAMX   XIT1                                                                   
         DROP  R2,R3                                                            
         EJECT                                                                  
**********************************************************************          
* LITERAL POOL                                                       *          
**********************************************************************          
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* CLOSE DLCB FOR DOWNLOAD                                            *          
**********************************************************************          
         USING DLCBD,R1                                                         
CLSDLCB  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,DLCB                                                          
         MVI   DLCBACT,DLCBEOR                                                  
         GOTO1 DLFLD                LAST FOR REPORT                             
         XIT1                                                                   
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
* LITERAL POOL                                                       *          
**********************************************************************          
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALISE DLFLD FOR DOWNLOADING                                    *         
***********************************************************************         
         USING DLCBD,R1                                                         
INITDLCB NTR1  BASE=*,LABEL=*                                                   
         LA    R1,DLCB             DOWNLOAD CONTROL BLOCK                       
*                                                                               
*        MVI   SKIPSPEC,C'R'       FORCE REQUEST PAGE SO DOWNLOAD               
*        MVI   RCREQREP,C'Y'       REPORT WILL START ON PAGE 2.                 
*        GOTO1 ACREPORT            THIS IS TO MAKE IT 'DATA' TYPE               
*        MVI   FORCEHED,C'Y'       AS OPPOSED TO 'TEXT' TYPE                    
*        MVI   RCSUBPRG,0                                                       
*        GOTO1 ACREPORT                                                         
*                                                                               
         XC    DLCBD(DLCBL),DLCBD  WILL START ON 2ND PAGE                       
         LA    RF,DLLINE                                                        
         ST    RF,DLCBAPR          USER SUPPLIED PRINT ROUTINE                  
         LA    RF,P                                                             
         ST    RF,DLCBAPL          USER SUPPLIED PRINT LINE                     
         MVI   DLCBACT,DLCBSOR     START OF REPORT                              
         OI    DLCBFLG1,DLCBFXFL+DLCBFXTN  EXTENDED TEXT FIELD USED             
         MVC   DLCXMAXL,=Y(L'P)                                                 
         MVI   DLCXDELC,C','       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      ALTERNATE TEXT DELIMITER                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON, END-OF-LINE                      
         MVI   DLCXEORC,C':'       END-OF-REPORT                                
         GOTO1 DLFLD               FIRST FOR REPORT                             
         XIT1                                                                   
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* USER SUPPLIED PRINT LINE ROUTINE                                    *         
***********************************************************************         
DLLINE   MVI   FORCEHED,C'N'       NEVER HEAD UP                                
         MVI   SPACING,1                                                        
         MVI   LINE,1                                                           
         GOTO1 ACREPORT                                                         
         XIT1                                                                   
*********************************************************************           
* WRITE FLAT FILE FOR STATUS:                                       *           
* DOWNLOADED VERSION TO PQ                                                      
*********************************************************************           
EDIOUTP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
EDIO10   L     RE,AIO              CLEAR IO TO SPACES                           
         LHI   RF,2000                                                          
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0                                                            
*                                                                               
         L     R2,AIO                                                           
         SR    R3,R3               R3=LENGTH OF DATA                            
         LR    RE,R4               RE=START OF DATA                             
EDIO20   CLI   0(R4),ESC           TEST ESCAPE SEQUENCE                         
         BE    EDIO30                                                           
         CLI   0(R4),EOR           TEST END OF RECORD                           
         BE    EDIO30                                                           
         CLI   0(R4),EOL           END OF LINE                                  
         BE    EDIO25                                                           
         LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         B     EDIO20                                                           
*                                                                               
EDIO25   LR    R0,R2               R0=DESTINATION                               
         LR    R1,R3               R1 & RF = LENGTH                             
         LR    RF,R3                                                            
         MVCL  R0,RE               DATA TO IO AREA                              
         BRAS  RE,DWLLIN                                                        
         AHI   R4,1                BUMP PAST EOL                                
         B     EDIO10                                                           
*                                                                               
EDIO30   LTR   R3,R3               TEST ANY DATA TO MOVE                        
         BZ    EDIO40                                                           
         LR    R0,R2               R0=DESTINATION                               
         LR    R1,R3               R1 & RF = LENGTH                             
         LR    RF,R3                                                            
         MVCL  R0,RE               DATA TO IO AREA                              
         BRAS  RE,DWLLIN                                                        
*                                                                               
EDIO40   CLI   0(R4),EOR           TEST END OF RECORD                           
         BE    EDIO50                                                           
         L     R5,=A(EDIDATA)                                                   
         ST    R5,FULL             STORE ADDRESS OF RBC TABLE                   
         BRAS  RE,XTRA             EXTRACT SPECIAL DATA                         
         LH    R3,HALF             THE LENGTH OF THE DATA FROM XTRA             
         BRAS  RE,DWLLIN                                                        
         LA    R4,4(R4)            BUMP R4 PASSED ESCAPE SEQUENCE               
         B     EDIO10                                                           
*                                                                               
EDIO50   L     R2,AIO                                                           
         LA    R4,1(R4)                                                         
         CLI   0(R4),EOT           TEST END OF TABLE                            
         BNE   EDIO10                                                           
*                                                                               
         USING DLCBD,RF                                                         
         LA    RF,DLCB                                                          
         MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 DLFLD,DLCBD                                                      
*                                                                               
EDIOX    XIT1                                                                   
         DROP  RF                                                               
         EJECT                                                                  
**********************************************************************          
* LITERAL POOL                                                       *          
**********************************************************************          
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FORMAT DATA INTO DOWNLOAD FORMAT                                    *         
* ENTRY:  R2 POINTS TO AIO WHICH HOLDS THE DATA                       *         
********* HALF CONTAINS THE LENGTH OF THE FIELD                       *         
***********************************************************************         
         USING DLCBD,R5                                                         
DWLLIN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,DLCB                                                          
         LR    RF,R2               OUTPUT FIELD                                 
         LR    R1,R3               LENGTH OF DATA                               
         AR    RF,R3               POINT TO END OF DATA                         
         BCTR  RF,0                                                             
         CLI   0(RF),C'"'          REPLACE ANY " WITH ' IN DATA                 
         BNE   *+8                 ELSE DOWNLOAD NOT HAPPY...                   
         MVI   0(RF),C''''         ALL THAT FOR ONE LOUSY DINK...               
         BCTR  RF,0                                                             
         BCT   R1,*-14                                                          
*                                                                               
DWLIN10  DS    0H                                                               
         MVI   DLCBFLX,C' '        FILL FIELD WITH SPACES                       
         MVC   DLCBFLX+1(L'DLCBFLX-1),DLCBFLX                                   
*                                                                               
         LR    R1,R3               LENGTH OF DATA                               
         BCTR  R1,0                                                             
         EXMVC R1,DLCBFLX,0(R2)                                                 
         MVI   DLCBACT,DLCBPUT     PUTTING TEXT                                 
         MVI   DLCBTYP,DLCBTXT                                                  
         OI    DLCBFLG1,DLCBFXFL+DLCBFXTN                                       
DWLIN20  GOTO1 DLFLD,DLCBD                                                      
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         LTORG                                                                  
*                                                                               
EDIOUT   DCB   DDNAME=EDIOUT,                       MINDSHARE US       X        
               DSORG=PS,                            GOING TO BOA       X        
               MACRF=(PM),                          STATUS 'EDI820'    X        
               RECFM=VB,                                               X        
               LRECL=4004,                                             X        
               BLKSIZE=32760                                                    
*                                                                               
FTROUT   DCB   DDNAME=FTROUT,                       FILE TRANSFERS     X        
               DSORG=PS,                            EDI820 FOR         X        
               RECFM=FB,                            WACHOVIA & EFT'S   X        
               LRECL=00435,                                            X        
               BLKSIZE=00435,                                          X        
               MACRF=PM                                                         
*                                                                               
* DSFTK-150                                                                     
FTROUTV  DCB   DDNAME=FTROUTV,                      CSV FILE FOR JPM   X        
               DSORG=PS,                            CHASE PCARD        X        
               RECFM=VB,                            PAYMENTS           X        
               LRECL=28395,                                            X        
               BLKSIZE=28399,                                          X        
               MACRF=PM                                                         
*                                                                               
CHASEIN  DCB   DDNAME=CHASEIN,                      FILE TRANSFERS     X        
               DSORG=PS,                            EDI820 FOR         X        
               RECFM=FB,                            WACHOVIA & EFT'S   X        
               LRECL=00350,                                            X        
               BLKSIZE=0,                                              X        
               MACRF=PM                                                         
*                                                                               
* DSFTK-150                                                                     
         EJECT                                                                  
RELOTAB  DS    0A                                                               
         DC    V(ACLIST)                                                        
         DC    V(DATVAL)                                                        
         DC    V(GETBROAD)                                                      
         DC    V(NUMTOLET)                                                      
         DC    V(PUBEDIT)                                                       
         DC    V(RIGHT)                                                         
         DC    V(DLFLD)                                                         
         DC    V(PQPROF)                                                        
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
         DC    A(EDTRN)                                                         
         DC    A(FTRRTN)                                                        
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
         DC    A(FRMBLK)           BLOCK WHICH HOLDS OUTPUT FORMAT              
         DC    A(FTROUTB)          GETFORM BLOCK                                
         DC    A(EDIWRK)           EDI WORK BLOCK                               
         DC    A(PDRFT)            DRAFT CHECK MASK                             
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
ETXT9    DCDD  AC#EFTOT,14,L      'TOTALS FOR EFT'                              
ETXT10   DCDD  AC#EFTCK,19,L      'TOTAL EFT CHECKS '                           
ETXT11   DCDD  AC#TPRI,19,L       'TOTAL FOR PRINTED'                           
ETXT12   DCDD  AC#TPRIC,20,L      'TOTAL PRINTED CHECKS'                        
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
CTXT9    DCDD  AC#EFTOT,14,L      'TOTALS FOR EFT'                              
CTXT10   DCDD  AC#EFTCA,19,L      'TOTAL EFT CHEQUES'                           
CTXT11   DCDD  AC#TPRI,19,L       'TOTAL FOR PRINTED'                           
CTXT12   DCDD  AC#TPRIC,20,L      'TOTAL PRINTED CHECKS'                        
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
*        DC    C'X',AL1(CEXQ),C'EXPENDITURE',AL3(CXPN-AC5502),AL1(0)            
         DC    C' ',AL1(EXPQ),C'EXPENSE    ',AL3(EXPN-AC5502),AL1(0)            
         DC    X'FF'                                                            
*                                                           PRINT               
PRNT     DC    AL1(50),AL3(PRR-AC5502),AL1(3),AL1(TYPCD)    REP                 
         DC    AL1(09),AL3(AOR-AC5502),AL1(6),AL1(0)        AOR                 
         DC    AL1(26),AL3(AOR-AC5502),AL1(6),AL1(0)        AOR                 
         DC    AL1(13),AL3(P13-AC5502),AL1(3),AL1(TYPCD)    AOR                 
         DC    AL1(00),AL3(PRP-AC5502),AL1(3),AL1(TYPCD)    PUB                 
*                                                                               
*                                                           SPOT                
SPOT     DC    AL1(34),AL3(SPR-AC5502),AL1(2),AL1(0)        REP                 
         DC    AL1(35),AL3(UWR-AC5502),AL1(7),AL1(0)        UNWIRED REP         
         DC    AL1(09),AL3(AOR-AC5502),AL1(6),AL1(0)        AOR                 
         DC    AL1(26),AL3(AOR-AC5502),AL1(6),AL1(0)        AOR                 
         DC    AL1(13),AL3(S13-AC5502),AL1(2),AL1(0)        AOR                 
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
*CXPN     DC    AL1(00),AL3(CXP-AC5502),AL1(5),AL1(TYCKT)    COKE EXP           
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
TYP      EQU   6                   VENDOR TYPE                                  
*                                                                               
*              SORT OPTION TABLE                                                
*              1 BYTE OPTION CODE, 4 BYTES FIELD EQUATES                        
*                                                                               
SORTS    DS    0CL5                                                             
         DC    C' ',AL1(TYP,REQ,ACO,000)    DEFAULT-TYPE,REQ,ACCOUNT            
         DC    C'N',AL1(TYP,REQ,ACN,000)    TYPE,REQUEST,ACCOUNT NAME           
         DC    C'C',AL1(TYP,REQ,ACO,000)    TYPE,REQUEST,ACCOUNT                
         DC    C'A',AL1(TYP,REQ,AMT,000)    TYPE,REQUEST,AMOUNT                 
         DC    C'D',AL1(TYP,REQ,CSD,000)    TYPE,REQUEST,CASH DISCOUNT          
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
         DC    AL1(TYP),AL1(L'VTYPE),S(VTYPE)     TYPE OF VENDOR                
         DC    X'FF'                                                            
*                                                                               
SORTCRD  DC    CL80'SORT FIELDS=(01,000,A),FORMAT=BI,WORK=1'                    
RECCRD   DC    CL80'RECORD TYPE=F,LENGTH=(000)'                                 
                                                                                
***********************************************************************         
* IO AND BUFFER AREAS                                                 *         
***********************************************************************         
         DS    0D                                                               
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
         EJECT                                                                  
***********************************************************************         
*  820 3040 AGENCY BANK ACCOUNT TABLE (COVERED BY E8ATABD)                      
*  ORIGIN #,0,LOGO CODE,RET ADDR CODE,SIG CODE                                  
***********************************************************************         
         DS    0D                                                               
E8ATAB   DS    0CL24                                                            
*                                                                               
         DC    AL2(13142),AL1(0)                      MSCK                      
         DC    CL4'0108',CL4'425',CL4'0107'                                     
*                                                                               
         DC    AL2(11604),AL1(0)                      MSNYMAX                   
         DC    CL4'0128',CL4'440',CL4'0141'                                     
*                                                                               
         DC    AL2(11698),AL1(0)                      MSCONMX                   
         DC    CL4'0128',CL4'440',CL4'0141'                                     
*                                                                               
         DC    AL2(11875),AL1(0)                      MSME                      
         DC    CL4'0131',CL4'472',CL4'0107'                                     
*                                                                               
         DC    AL2(11876),AL1(0)                      MSCONME                   
         DC    CL4'0131',CL4'472',CL4'0107'                                     
*                                                                               
         DC    AL2(11869),AL1(0)                      MSGM                      
         DC    CL4'0132',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(13784),AL1(0)                      MSGMCK                    
         DC    CL4'0132',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(13787),AL1(0)                      MSMXCK                    
         DC    CL4'0132',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(13013),AL1(0)                      MECK (MEDIACOM)           
         DC    CL4'0135',CL4'834',CL4'0134'                                     
*                                                                               
         DC    AL2(13785),AL1(0)                      MECKGM                    
         DC    CL4'0132',CL4'827',CL4'0142'                                     
*                                                                               
         DC    AL2(6028),AL1(0)                       YNCKM                     
         DC    CL4'0140',CL4'858',CL4'0141'                                     
*                                                                               
         DC    AL2(13786),AL1(0)                      YNCKGM                    
         DC    CL4'0132',CL4'827',CL4'0142'                                     
*                                                                               
         DC    AL2(12470),AL1(0)                      YNCKOUT                   
         DC    CL4'0138',CL4'857',CL4'0139'                                     
*                                                                               
         DC    AL2(14754),AL1(0)                      MGCKGM                    
         DC    CL4'0132',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(14753),AL1(0)                      MGCKM                     
         DC    CL4'0140',CL4'858',CL4'0155'                                     
*                                                                               
         DC    AL2(14743),AL1(0)                      MGCKOUT                   
         DC    CL4'0150',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(16538),AL1(0)                      MCWWCK                    
         DC    CL4'0135',CL4'834',CL4'0141'                                     
*                                                                               
         DC    AL2(16539),AL1(0)                      MGWWCK                    
         DC    CL4'0140',CL4'858',CL4'0155'                                     
*                                                                               
         DC    AL2(16540),AL1(0)                      MGSWCK                    
         DC    CL4'0132',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(16541),AL1(0)                      MSMXWWCK                  
         DC    CL4'0128',CL4'440',CL4'0141'                                     
*                                                                               
         DC    AL2(16542),AL1(0)                      MSGSCK                    
         DC    CL4'0132',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(16543),AL1(0)                      MSGTCK                    
         DC    CL4'0132',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(15025),AL1(0)                      MSCKUN                    
         DC    CL4'0108',CL4'861',CL4'0141'                                     
*                                                                               
         DC    AL2(15026),AL1(0)                      MSGMCKUN                  
         DC    CL4'0132',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(15524),AL1(0)                      MSCKTMEX                  
         DC    CL4'0145',CL4'883',CL4'0141'                                     
*                                                                               
         DC    AL2(15523),AL1(0)                      MCCK                      
         DC    CL4'0135',CL4'834',CL4'0141'                                     
*                                                                               
         DC    AL2(15522),AL1(0)                      MCCKGM                    
         DC    CL4'0132',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(15545),AL1(0)                      MSCKCT                    
         DC    CL4'0146',CL4'884',CL4'0141'                                     
*                                                                               
         DC    AL2(16000),AL1(0)                      MSCKXA                    
         DC    CL4'0147',CL4'886',CL4'0141'                                     
*                                                                               
         DC    AL2(16001),AL1(0)                      MSCKWW                    
         DC    CL4'0108',CL4'425',CL4'0107'                                     
*                                                                               
         DC    AL2(16002),AL1(0)                      MSCKTRCO                  
         DC    CL4'0132',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(16003),AL1(0)                      MSCKTRUS                  
         DC    CL4'0132',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(16225),AL1(0)                      MCCKOC                    
         DC    CL4'0145',CL4'883',CL4'0141'                                     
*                                                                               
         DC    AL2(16226),AL1(0)                      MGCKMOE                   
         DC    CL4'0145',CL4'883',CL4'0141'                                     
*                                                                               
         DC    AL2(16227),AL1(0)                      MSCKOS                    
         DC    CL4'0145',CL4'883',CL4'0141'                                     
*                                                                               
         DC    AL2(16228),AL1(0)                      MSMXOX                    
         DC    CL4'0145',CL4'883',CL4'0141'                                     
*                                                                               
         DC    AL2(16229),AL1(0)                      MSCKTROU                  
         DC    CL4'0145',CL4'883',CL4'0141'                                     
*                                                                               
         DC    AL2(16634),AL1(0)                      MSPECK                    
         DC    CL4'0132',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(16651),AL1(0)                      MSCKLION                  
         DC    CL4'0108',CL4'861',CL4'0107'                                     
*                                                                               
         DC    AL2(16652),AL1(0)                      MSGMCKLN                  
         DC    CL4'0132',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(16713),AL1(0)                      MSCKNM                    
         DC    CL4'0132',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(16714),AL1(0)                      MSGMCKNM                  
         DC    CL4'0132',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(16902),AL1(0)                      JOCK                      
         DC    CL4'0152',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(16930),AL1(0)                      JOGMCK                    
         DC    CL4'0132',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(17011),AL1(0)                      MODIGMCK                  
         DC    CL4'0153',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(17012),AL1(0)                      MODIMCCK                  
         DC    CL4'0153',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(17013),AL1(0)                      MODIMGCK                  
         DC    CL4'0153',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(17014),AL1(0)                      MODIMXCK                  
         DC    CL4'0153',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(17015),AL1(0)                      MODIMSCK                  
         DC    CL4'0153',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(17016),AL1(0)                      MODINMCK                  
         DC    CL4'0153',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(17019),AL1(0)                      MODIMUCK                  
         DC    CL4'0153',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(17020),AL1(0)                      MODIMLCK                  
         DC    CL4'0153',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(17197),AL1(0)                      MSIXCK                    
         DC    CL4'0132',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(17281),AL1(0)                      CHCK                      
         DC    CL4'0154',CL4'827',CL4'0155'                                     
         DC    AL2(17283),AL1(0)                      CHGMCK                    
         DC    CL4'0132',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(18333),AL1(0)                      GAINCK                    
         DC    CL4'0156',CL4'897',CL4'0141'                                     
*                                                                               
         DC    AL2(2635),AL1(0)                       DDSB (FOR TESTNG)         
         DC    CL4'0108',CL4'861',CL4'0107'                                     
*                                                                               
         DC    AL2(18762),AL1(0)                      MSTACK                    
         DC    CL4'0132',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(19711),AL1(0)                      MSCKNE                    
         DC    CL4'0108',CL4'425',CL4'0141'                                     
*                                                                               
         DC    AL2(19712),AL1(0)                      MSCKNG                    
         DC    CL4'0108',CL4'425',CL4'0141'                                     
*                                                                               
         DC    AL2(19734),AL1(0)                      MSCKGMNE                  
         DC    CL4'0132',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(19918),AL1(0)                      MSCKNW                    
         DC    CL4'0108',CL4'425',CL4'0141'                                     
*                                                                               
         DC    AL2(19919),AL1(0)                      MSCGCK                    
         DC    CL4'0132',CL4'827',CL4'0141'                                     
*                                                                               
         DC    AL2(20206),AL1(0)                      MSCKGU                    
         DC    CL4'0132',CL4'827',CL4'0141'                                     
*                                                                               
         DC    XL2'FFFF',XL1'FF'                      END OF TABLE              
         DC    CL4'????',CL4'????',CL4'????'                                    
*                                                                               
***********************************************************************         
*        820 3040 OUPTUT DETAILS                                                
***********************************************************************         
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
PADDR3Q  EQU   17                   PAYEE ADDRESS LINE 3                        
PADDR4Q  EQU   18                   PAYEE ADDRESS LINE 4                        
PCITYQ   EQU   19                   PAYEE CITY                                  
PSTATEQ  EQU   20                   PAYEE STATE                                 
PZIPQ    EQU   21                   PAYEE POSTAL CODE                           
MLTYPEQ  EQU   22                   MAILING TYPE CODE                           
PCTRYQ   EQU   23                   COUNTRY CODE                                
E8LOGOQ  EQU   24                   LOGO CODE                                   
E8RACOQ  EQU   25                   RETURN ADDRESS CODE                         
E8SIGCOQ EQU   26                   SIGNATURE CODE                              
E8NARRQ  EQU   27                   NARRATIVE                                   
*                                                                               
XDROUT   EQU   X'80'                EXECUTE A ROUTINE                           
*                                                                               
XDATA    DS    0F                   THE ORDER MUST MATCH THE EQU                
         DC    AL1(L'E8CHAMT),AL3(E8CHAMT-E820D)   1-CHECK AMOUNT               
         DC    AL1(L'E8CHDTE),AL3(E8CHDTE-E820D)   2-CHECK DATE                 
         DC    AL1(L'E8CHNUM),AL3(E8CHNUM-E820D)   3-CHECK NUMBER               
         DC    AL1(L'E8BNKNO),AL3(E8BNKNO-E820D)   4-BANK ACCOUNT #             
         DC    AL1(L'E8RMRSEQ),AL3(E8RMRSEQ-E820D) 5-RMR SEQUENCE #             
         DC    AL1(L'E8INFO),AL3(E8INFO-E820D)     6-RMR REFERENCE DATA         
         DC    AL1(L'E8INFO2),AL3(E8INFO2-E820D)   7-RMR REFERENCE DATA         
         DC    AL1(L'E8INVDTE),AL3(E8INVDTE-E820D) 8-IVOICE DATE                
         DC    AL1(L'E8INVNO),AL3(E8INVNO-E820D)   9-INVOICE NUMBER             
         DC    AL1(L'E8INVGRS),AL3(E8INVGRS-E820D) 10-NET PAYABLE               
         DC    AL1(L'E8CD),AL3(E8CD-E820D)         11-CASH DISCOUNT             
         DC    AL1(L'E8INVNET),AL3(E8INVNET-E820D) 12-INVOICE LESS DISC         
         DC    AL1(L'E8CHGRSS),AL3(E8CHGRSS-E820D) 13-GROSS FOR CHECK           
         DC    AL1(L'E8ANME),AL3(E8ANME-E820D)     14-PAYEE NAME                
         DC    AL1(L'E8ADR1),AL3(E8ADR1-E820D)     15-PAYEE ADDRESS 1           
         DC    AL1(L'E8ADR2),AL3(E8ADR2-E820D)     16-PAYEE ADDRESS 2           
         DC    AL1(L'E8ADR3),AL3(E8ADR3-E820D)     17-PAYEE ADDRESS 3           
         DC    AL1(L'E8ADR4),AL3(E8ADR4-E820D)     18-PAYEE ADDRESS 4           
         DC    AL1(L'E8CITY),AL3(E8CITY-E820D)     19-CITY                      
         DC    AL1(L'E8STATE),AL3(E8STATE-E820D)   20-STATE                     
         DC    AL1(L'E8ZIP),AL3(E8ZIP-E820D)       21-POSTAL CODE               
         DC    AL1(L'E8MTC),AL3(E8MTC-E820D)       22-MAILING TYPE CODE         
         DC    AL1(L'E8CTRY),AL3(E8CTRY-E820D)     23-COUNTRY CODE              
         DC    AL1(L'E8LOGO),AL3(E8LOGO-E820D)     24-LOGO CODE                 
         DC    AL1(L'E8RACO),AL3(E8RACO-E820D)     25-RETURN ADDRSS CDE         
         DC    AL1(L'E8SIGCO),AL3(E8SIGCO-E820D)   26-SIGNATURE CODE            
         DC    AL1(L'E8NARR1),AL3(E8NARR-E820D)    27-NARRATIVE                 
*                                                                               
E820WRK  DC    (E820DX-E820D)C' '                                               
*&&DO                                                                           
***********************************************************************         
*       MINDSHARE EDI 820 3040 RECORD LAYOUT TABLE                              
***********************************************************************         
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
         DC    CL2'1'                        TRACE TYPE CODE                    
         DC    AL1(ESC),AL1(30),AL2(CHKNOQ)  REFERENCE NUMBER                   
         DC    CL10' '        NOT USED       - ORIG. CO. ID                     
         DC    AL1(ESC),AL1(15),AL2(PNAMEQ)  PAYEE NAME                         
         DC    AL1(EOR)                                                         
*                                          REF-REFERENCE NUMBERS                
         DC    C'REF'                                                           
         DC    C'006'                                                           
         DC    CL2'ZZ'                        REF NO. QUALIFIER                 
         DC    CL1'S'                         REF NO.-CHECK STYLE TYPE          
         DC    CL2'03'                          PRINT SITE (PHOENIX)            
         DC    CL2'48'                          CHECK STYLE ID                  
         DC    CL4'0048'                        REMITTANCE FORMAT ID            
         DC    AL1(ESC),AL1(4),AL2(E8LOGOQ)     LOGO CODE                       
         DC    AL1(ESC),AL1(3),AL2(E8RACOQ)     RETURN ADDRESS ID-CHK           
         DC    AL1(ESC),AL1(1),AL2(MLTYPEQ)     MAILING TYPE CODE               
         DC    CL1' '                           BLANK                           
         DC    AL1(ESC),AL1(3),AL2(E8RACOQ)     RETURN ADDRESS ID-CHK           
         DC    AL1(ESC),AL1(4),AL2(E8SIGCOQ)    SIGNATURE #1 ID                 
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
*                                          N3-ADDRESS INFORMATION               
         DC    C'N3 '                                                           
         DC    C'010'                                                           
         DC    AL1(ESC),AL1(35),AL2(PADDR1Q)  ADDRESS INFROMATION               
         DC    AL1(ESC),AL1(35),AL2(PADDR2Q)  ADDRESS INFROMATION               
         DC    AL1(EOR)                                                         
*                                                                               
         DC    AL1(EOT)                                                         
*                                          N3-ADDRESS INFORMATION               
E8H7N3   DC    C'N3 '                        FOR FOREIGN ADDRESSES              
         DC    C'010'                                                           
         DC    AL1(ESC),AL1(35),AL2(PADDR3Q)  ADDRESS INFROMATION               
         DC    AL1(ESC),AL1(35),AL2(PADDR4Q)  ADDRESS INFROMATION               
         DC    AL1(EOR)                                                         
*                                          N4-ADDRESS INFORMATION               
E8H7N4   DC    C'N4 '                                                           
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
RMRTEXT  DC    C'RMR'                                                           
         DC    C'037'                                                           
         DC    CL2'ZZ'                        REF NO. QUALIFIER                 
         DC    AL1(ESC),AL1(6),AL2(RMRSEQQ)   SEQUENCE NUMBER                   
         DC    CL3'002'                       RMR TEXT LINE                     
         DC    AL1(EOR)                                                         
*                                          REF-REFERENCE NUMBERS                
RMRTEXR  DC    C'REF'                                                           
         DC    C'039'                                                           
         DC    CL2'ZZ'                        REF. NUMBER QUALIFIER             
         DC    CL30'0007'                     REF. NUMBER                       
         DC    AL1(ESC),AL1(80),AL2(E8NARRQ)  TEXT COMMENT                      
         DC    AL1(EOR)                                                         
*                                          REF-REFERENCE NUMBERS                
         DC    AL1(EOT)                        END OF TABLE                     
*                                          REF-REFERENCE NUMBERS                
RMRTOT   DC    C'RMR'                                                           
         DC    C'037'                                                           
         DC    CL2'ZZ'                        REF NO. QUALIFIER                 
         DC    AL1(ESC),AL1(6),AL2(RMRSEQQ)   SEQUENCE NUMBER                   
         DC    CL3'003'                       RMR TOTAL LINE                    
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
*&&                                                                             
*                                                                               
***********************************************************************         
* EDI EQUATES FOR SOFT DATA FIELDS                                              
* EQUATE NUMBER RELATES TO THE POSITION OF DATA INFORMATION IN THE              
* EIDDATA TABLE.                                                                
***********************************************************************         
*                                                                               
EDIFDTQ  EQU   1                   FILE CREATION DATE                           
EDIALPHQ EQU   2                   ALPHA ID                                     
EDICNMQ  EQU   3                   COMPANY NAME                                 
EDIUSRQ  EQU   4                   REQUEST USER                                 
EDIUNMQ  EQU   5                   REQUEST USER NAME                            
EDILDGQ  EQU   6                   LEDGER CODE                                  
EDILNMQ  EQU   7                   LEDGER NAME                                  
EDICURRQ EQU   8                   CURRENCY CODE                                
EDIREQ#Q EQU   9                   REQUEST NUMBER                               
EDICHNQ  EQU   10                  CHECK NUMBER                                 
EDICHDQ  EQU   11                  CHECK DATE                                   
EDICHAQ  EQU   12                  CHECK AMOUNT                                 
EDIPCDQ  EQU   13                  PAYEE CODE                                   
EDIPNMQ  EQU   14                  PAYEE NAME                                   
EDIMDCQ  EQU   15                  MEDIA CODE                                   
EDIAD1Q  EQU   16                  ADDRESS 1                                    
EDIAD2Q  EQU   17                  ADDRESS 2                                    
EDIAD3Q  EQU   18                  ADDRESS 3                                    
EDIAD4Q  EQU   19                  ADDRESS 4                                    
EDISA1Q  EQU   20                  STREET ADDRESS LINE 1                        
EDISA2Q  EQU   21                  STREET ADDRESS LINE 2                        
EDICTYQ  EQU   22                  CITY/TOWN                                    
EDISTQ   EQU   23                  STATE/PROVINCE                               
EDIZIPQ  EQU   24                  ZIP CODE                                     
EDICTRYQ EQU   25                  COUNTRY                                      
EDICHAWQ EQU   26                  CHECK AMOUNT IN WORDS                        
EDIINQ   EQU   27                  INVOICE NUMBER                               
EDIINVDQ EQU   28                  INVOICE DATE                                 
EDITGRQ  EQU   29                  TRUE GROSS AMOUNT (FOR MEDIA)                
EDIGRQ   EQU   30                  GROSS AMOUNT                                 
EDIDCQ   EQU   31                  DISCOUNT AMOUNT                              
EDINTQ   EQU   32                  NET AMOUNT                                   
EDIGSTQ  EQU   33                  GST AMOUNT                                   
EDIPSTQ  EQU   34                  PST AMOUNT                                   
EDICAQ   EQU   35                  SOURE CODE (CONTRA)                          
EDICANQ  EQU   36                  SOURCE CODE NAME (CONTRA NAME)               
EDICLTQ  EQU   37                  CLIENT CODE                                  
EDICLTNQ EQU   38                  CLIENT CODE NAME                             
EDIPRDQ  EQU   39                  PRODUCT CODE                                 
EDIPRDNQ EQU   40                  PRODUCT CODE NAME                            
EDIJOBQ  EQU   41                  JOB CODE                                     
EDIJOBNQ EQU   42                  JOB CODE NAME                                
EDINARQ  EQU   43                  DESCRIPTION (TRANSACTION NARRATIVE)          
EDIMOSQ  EQU   44                  MOS OR ADVERTISING MONTH                     
EDIVCDQ  EQU   45                  TRUE VENDOR CODE                             
EDIVNMQ  EQU   46                  TRUE VENDOR NAME                             
EDIPERQ  EQU   47                  PERIOD DATE (MEDIA)                          
EDIOFCQ  EQU   48                  OFFICE CODE                                  
EDIOFNQ  EQU   49                  OFFICE NAME                                  
EDIMOAQ  EQU   50                  MOA                                          
EDITRCQ  EQU   51                  TOTAL RECORDS                                
EDITDLQ  EQU   52                  TOTAL DOLLARS (NET VALUE OF FILE)            
EDITCCQ  EQU   53                  TOTAL CHECK COUNT                            
*                                                                               
*                                                                               
EDIDATA  DS    0F                                                               
         DC    AL1(L'EDIFDTE),AL3(EDIFDTE-EDIDD)   FILE CREATION DATE           
         DC    AL1(L'EDIALPH),AL3(EDIALPH-EDIDD)   ALPHA ID                     
         DC    AL1(L'EDICNM),AL3(EDICNM-EDIDD)     COMPANY NAME                 
         DC    AL1(L'EDIUSR),AL3(EDIUSR-EDIDD)     REQUEST USER ID              
         DC    AL1(L'EDIORGN),AL3(EDIORGN-EDIDD)   ORIGIN ID NAME               
         DC    AL1(L'EDILDG),AL3(EDILDG-EDIDD)     LEDGER CODE                  
         DC    AL1(L'EDILDGN),AL3(EDILDGN-EDIDD)   LEDGER NAME                  
         DC    AL1(L'EDICURR),AL3(EDICURR-EDIDD)   CURRENCY                     
         DC    AL1(L'EDIREQ#),AL3(EDIREQ#-EDIDD)   REQUEST NUMBER               
         DC    AL1(L'EDICHKN),AL3(EDICHKN-EDIDD)   CHECK NUMBER                 
         DC    AL1(L'EDICDTE),AL3(EDICDTE-EDIDD)   CHECK DATE                   
         DC    AL1(L'EDICAMT),AL3(EDICAMT-EDIDD)   CHECK AMOUNT                 
         DC    AL1(L'EDIPACC),AL3(EDIPACC-EDIDD)   PAYEE CODE                   
         DC    AL1(L'EDIPNME),AL3(EDIPNME-EDIDD)   PAYEE NAME                   
         DC    AL1(L'EDIMEDC),AL3(EDIMEDC-EDIDD)   MEDIA CODE                   
         DC    AL1(L'EDIRADD1),AL3(EDIRADD1-EDIDD) ADDRESS 1                    
         DC    AL1(L'EDIRADD2),AL3(EDIRADD2-EDIDD) ADDRESS 2                    
         DC    AL1(L'EDIRADD3),AL3(EDIRADD3-EDIDD) ADDRESS 3                    
         DC    AL1(L'EDIRADD4),AL3(EDIRADD4-EDIDD) ADDRESS 4                    
         DC    AL1(L'EDISPACE),AL3(EDISPACE-EDIDD) ADDR LINE 1 (N/A)            
         DC    AL1(L'EDISPACE),AL3(EDISPACE-EDIDD) ADDR LINE 2 (N/A)            
         DC    AL1(L'EDISPACE),AL3(EDISPACE-EDIDD) CITY (N/A)                   
         DC    AL1(L'EDISPACE),AL3(EDISPACE-EDIDD) STATE (N/A)                  
         DC    AL1(L'EDISPACE),AL3(EDISPACE-EDIDD) ZIP (N/A)                    
         DC    AL1(L'EDISPACE),AL3(EDISPACE-EDIDD) COUNTRY (N/A)                
         DC    AL1(L'EDICAMTW),AL3(EDICAMTW-EDIDD) CHECK AMT IN WORDS           
         DC    AL1(L'EDIINVN),AL3(EDIINVN-EDIDD)   INVOICE NUMBER               
         DC    AL1(L'EDIINVD),AL3(EDIINVD-EDIDD)   INVOICE DATE                 
         DC    AL1(L'EDIGAMT),AL3(EDITGAMT-EDIDD)  TRUE GROSS AMOUNT            
         DC    AL1(L'EDIGAMT),AL3(EDIGAMT-EDIDD)   GROSS AMOUNT                 
         DC    AL1(L'EDIDAMT),AL3(EDIDAMT-EDIDD)   DISCOUNT AMOUNT              
         DC    AL1(L'EDINAMT),AL3(EDINAMT-EDIDD)   NET AMOUNT                   
         DC    AL1(L'EDIGST),AL3(EDIGST-EDIDD)     GST AMOUNT                   
         DC    AL1(L'EDIPST),AL3(EDIPST-EDIDD)     PST AMOUNT                   
         DC    AL1(L'EDICAC),AL3(EDICAC-EDIDD)     SOURCE (CONTRA ACCT)         
         DC    AL1(L'EDICACN),AL3(EDICACN-EDIDD)   SOURCE NAME (C/A NM)         
         DC    AL1(L'EDICLT),AL3(EDICLT-EDIDD)     CLIENT CODE                  
         DC    AL1(L'EDICLTN),AL3(EDICLTN-EDIDD)   CLIENT CODE NAME             
         DC    AL1(L'EDIPRD),AL3(EDIPRD-EDIDD)     PRODUCT CODE                 
         DC    AL1(L'EDIPRDN),AL3(EDIPRDN-EDIDD)   PRODUCT CODE NAME            
         DC    AL1(L'EDIJOB),AL3(EDIJOB-EDIDD)     JOB CODE                     
         DC    AL1(L'EDIJOBN),AL3(EDIJOBN-EDIDD)   JOB CODE NAME                
         DC    AL1(L'EDINARR),AL3(EDINARR-EDIDD)   NARRATIVE                    
         DC    AL1(L'EDIMOS),AL3(EDIMOS-EDIDD)     MOS OR ADV MONTH             
         DC    AL1(L'EDIVCDE),AL3(EDIVCDE-EDIDD)   TRUE VENDOR CODE             
         DC    AL1(L'EDIVNME),AL3(EDIVNME-EDIDD)   TRUE VENDOR NAME             
         DC    AL1(L'EDIPERD),AL3(EDIPERD-EDIDD)   PERIOD DATE                  
         DC    AL1(L'EDIOFFC),AL3(EDIOFFC-EDIDD)   OFFICE CODE                  
         DC    AL1(L'EDIOFFN),AL3(EDIOFFN-EDIDD)   OFFICE NAME                  
         DC    AL1(L'EDIMOA),AL3(EDIMOA-EDIDD)     MOA                          
         DC    AL1(L'EDIRECS),AL3(EDIRECS-EDIDD)   TOTAL RECORDS                
         DC    AL1(L'EDITOTD),AL3(EDITOTD-EDIDD)   TOTAL DOLLARS                
         DC    AL1(L'EDITCHK),AL3(EDITCHK-EDIDD)   TOTAL CHECK COUNT            
*                                                                               
***********************************************************************         
* EDI DATA TRANSMISSION RECORD LAYOUT TABLE                                     
***********************************************************************         
EDIHED   DC    C'A',AL1(EOL)                   HEADER RECORD                    
         DC    AL1(ESC),AL1(8),AL2(EDIFDTQ)    FILE CREATION DATE               
         DC    AL1(ESC),AL1(2),AL2(EDIALPHQ)   COMPANY ALPHA ID                 
         DC    AL1(ESC),AL1(36),AL2(EDICNMQ)   COMPANY NAME                     
         DC    AL1(ESC),AL1(8),AL2(EDIUSRQ)    REQUEST USER ID                  
         DC    AL1(ESC),AL1(33),AL2(EDIUNMQ)   REQUEST USER ID NAME             
         DC    AL1(ESC),AL1(1),AL2(EDILDGQ)    LEDGER CODE                      
         DC    AL1(ESC),AL1(36),AL2(EDILNMQ)   LEDGER NAME                      
         DC    AL1(ESC),AL1(1),AL2(EDICURRQ)   CURRENCY 1=USD, 2=CAD            
         DC    AL1(EOR)                                                         
         DC    AL1(EOT)                                                         
*                                                                               
EDICTR   DC    C'B',AL1(EOL)                   CHECK TRANSACTION RECORD         
         DC    AL1(ESC),AL1(3),AL2(EDIREQ#Q)   REQUEST NUMBER                   
         DC    AL1(ESC),AL1(8),AL2(EDICHNQ)    CHECK NUMBER                     
         DC    AL1(ESC),AL1(8),AL2(EDICHDQ)    CHECK DATE                       
         DC    AL1(ESC),AL1(12),AL2(EDICHAQ)   CHECK AMOUNT                     
         DC    AL1(ESC),AL1(14),AL2(EDIPCDQ)   PAYEE CODE                       
         DC    AL1(ESC),AL1(36),AL2(EDIPNMQ)   PAYEE NAME                       
         DC    AL1(ESC),AL1(1),AL2(EDIMDCQ)    MEDIA CODE                       
         DC    AL1(ESC),AL1(35),AL2(EDIAD1Q)   ADDRESS 1                        
         DC    AL1(ESC),AL1(35),AL2(EDIAD2Q)   ADDRESS 2                        
         DC    AL1(ESC),AL1(35),AL2(EDIAD3Q)   ADDRESS 3                        
         DC    AL1(ESC),AL1(35),AL2(EDIAD4Q)   ADDRESS 4                        
* THESE FIELDS ARE NOT USED YET.  THAT'S WHY THEY ARE ONLY ONE BYTE             
* LONG.  THEY ARE JUST PLACEHOLDERS FOR NOW.                                    
         DC    AL1(ESC),AL1(1),AL2(EDISA1Q)    STREET ADDRESS LINE 1            
         DC    AL1(ESC),AL1(1),AL2(EDISA2Q)    STREET ADDRESS LINE 2            
         DC    AL1(ESC),AL1(1),AL2(EDICTYQ)    CITY/TOWN                        
         DC    AL1(ESC),AL1(1),AL2(EDISTQ)     STATE/PROVINCE                   
         DC    AL1(ESC),AL1(1),AL2(EDIZIPQ)    ZIP CODE                         
         DC    AL1(ESC),AL1(1),AL2(EDICTRYQ)   COUNTRY                          
* THESE FIELDS ARE NOT USED YET.  THAT'S WHY THEY ARE ONLY ONE BYTE             
* LONG.  THEY ARE JUST PLACEHOLDERS FOR NOW.                                    
         DC    AL1(ESC),AL1(150),AL2(EDICHAWQ) CHECK AMOUNT IN WORDS            
         DC    AL1(EOR)                                                         
         DC    AL1(EOT)                                                         
*                                                                               
EDISDR   DS    0C                                                               
         DC    C'C',AL1(EOL)                   STUB DETAIL RECORD               
         DC    AL1(ESC),AL1(8),AL2(EDICHNQ)    CHECK NUMBER                     
         DC    AL1(ESC),AL1(20),AL2(EDIINQ)    INVOICE NUMBER                   
         DC    AL1(ESC),AL1(8),AL2(EDIINVDQ)   INVOICE DATE                     
         DC    AL1(ESC),AL1(12),AL2(EDITGRQ)   TRUE GROSS AMOUNT                
         DC    AL1(ESC),AL1(12),AL2(EDIGRQ)    GROSS AMOUNT                     
         DC    AL1(ESC),AL1(11),AL2(EDIDCQ)    DISCOUNT AMOUNT                  
         DC    AL1(ESC),AL1(12),AL2(EDINTQ)    NET AMOUNT (PAYABLE)             
         DC    AL1(ESC),AL1(11),AL2(EDIGSTQ)   GST AMOUNT                       
         DC    AL1(ESC),AL1(11),AL2(EDIPSTQ)   PST AMOUNT                       
         DC    AL1(ESC),AL1(14),AL2(EDICAQ)    SOURCE CODE (CONTRA)             
         DC    AL1(ESC),AL1(36),AL2(EDICANQ)   SOURCE CODE NAME(CONTRA)         
         DC    AL1(ESC),AL1(3),AL2(EDICLTQ)    CLIENT CODE                      
         DC    AL1(ESC),AL1(36),AL2(EDICLTNQ)  CLIENT CODE NAME                 
         DC    AL1(ESC),AL1(3),AL2(EDIPRDQ)    PRODUCT CODE                     
         DC    AL1(ESC),AL1(36),AL2(EDIPRDNQ)  PRODUCT CODE NAME                
         DC    AL1(ESC),AL1(6),AL2(EDIJOBQ)    JOB CODE                         
         DC    AL1(ESC),AL1(36),AL2(EDIJOBNQ)  JOB CODE NAME                    
         DC    AL1(ESC),AL1(200),AL2(EDINARQ)  DESCRIPTION (NARRATIVE)          
         DC    AL1(ESC),AL1(6),AL2(EDIMOSQ)    MOS OR ADVERTISING MONTH         
         DC    AL1(ESC),AL1(14),AL2(EDIVCDQ)   TRUE VENDOR CODE                 
         DC    AL1(ESC),AL1(36),AL2(EDIVNMQ)   TRUE VENDOR NAME                 
         DC    AL1(ESC),AL1(17),AL2(EDIPERQ)   PERIOD DATE                      
         DC    AL1(ESC),AL1(2),AL2(EDIOFCQ)    OFFICE CODE                      
         DC    AL1(ESC),AL1(36),AL2(EDIOFNQ)   OFFICE NAME                      
         DC    AL1(ESC),AL1(6),AL2(EDIMOAQ)    MOA                              
         DC    AL1(EOR)                                                         
         DC    AL1(EOT)                                                         
*                                                                               
EDITRR   DC    C'Z',AL1(EOL)                   TRAILER RECORD                   
         DC    AL1(ESC),AL1(15),AL2(EDITRCQ)   TOTAL RECORDS                    
         DC    AL1(ESC),AL1(12),AL2(EDITDLQ)   TOTAL DOLLARS                    
         DC    AL1(ESC),AL1(15),AL2(EDITCCQ)   TOTAL CHECK COUNT                
         DC    AL1(EOR)                                                         
         DC    AL1(EOT)                                                         
EOT      EQU   X'FF'               END OF TABLE                                 
EOR      EQU   X'00'               END OF RECORD                                
ESC      EQU   X'01'               ESCAPE                                       
EOL      EQU   X'02'               END OF LINE                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
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
*                                                                               
PDRFT    DC    0CL38                                                            
         DC    CL38'*   DDDD   RRRR   AAA  FFFFF TTTTT   *'                     
         DC    CL38' *  D   D  R   R A   A F       T    * '                     
         DC    CL38' ** D    D RRRR  AAAAA FFF     T   ** '                     
         DC    CL38' *  D   D  R  R  A   A F       T    * '                     
         DC    CL38'*   DDDD   R   R A   A F       T     *'                     
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* SET FIELDS FOR EDI:                                                *          
* STATION/PUB/REP INFO                                                          
* CLIENT NAME, PRODUCT NAME AND ESTIMATE                                        
* INVOICE DATE                                                                  
**********************************************************************          
         USING EDIDD,R9                                                         
EDIINF   NTR1  BASE=*,LABEL=*                                                   
         MVC   EDIBLK1,SPACES                                                   
         MVC   EDIBLK2,SPACES                                                   
         MVC   EDIBSTY,SPACES                                                   
*                                                                               
         CLI   SYEQU,MEDQ              IS IT A MEDIA CHECK?                     
         BNE   EDIINF50                 NO                                      
         CLI   SYLDG,C'P'              IS IT PRINT?                             
         BE    EDIINF20                 YES                                     
         CLI   SYLDG,C'Q'                                                       
         BE    EDIINF20                 YES                                     
*                                                                               
         LA    R2,SRACC+4              R4 TO STATION                            
         CLI   TYCDE,00                SPOT STATION?                            
         BE    *+8                      YES                                     
         LA    R2,SRCNTR+4             FOR REP STATION IN CONTR                 
         LA    R3,EDIBLK1             R3 TO OUTPUT AREA                         
         MVC   0(L'AC@STA,R3),AC@STA   'STATION'                                
         LA    R3,EDIBLK1+L'EDIBLK1-1                                           
         ST    R3,FULL                                                          
         BRAS  RE,FNDNXT               R3 TO NEXT SPACE                         
         L     R3,FULL                                                          
         MVI   0(R3),C':'              REPLACE WITH COLON                       
         MVC   1(5,R3),0(R2)           STATION CALL LETTERS                     
         MVC   5(3,R3),=C'-AM'         AND ADD THE BAND                         
         CLI   4(R2),C'A'              -AM                                      
         BE    EDIINF10                                                         
         MVI   6(R3),C'F'                                                       
         CLI   4(R2),C'F'              -FM                                      
         BE    EDIINF10                                                         
         MVC   5(3,R3),=C'-TV'         OR -TV                                   
*                                                                               
EDIINF10 OC    SRXPY(SRXPYL),SRXPY     EXTRA PAYMENT ELEMENT                    
         BZ    EDIINF60                                                         
*                                                                               
         LA    R3,EDIBLK2                                                       
         MVC   0(L'SRXCLI,R3),SRXCLI   CLIENT NAME                              
*                                                                               
         CLC   ALPHAID,=C'H7'     GROUPM CANNOT TRANSLATE THE *                 
         BNE   EDIINF15           IN THE CLIENT NAME                            
         LA    R6,L'SRXCLI        CUSTENH-2858 FOR E*TRADE                      
         BCTR  R6,0                                                             
         CLI   0(R3),C'*'                                                       
         BNE   *+8                                                              
         MVI   0(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         BCT   R6,*-16                                                          
*                                                                               
EDIINF15 LA    R3,EDIBLK2+L'EDIBLK2-1                                           
         ST    R3,FULL                                                          
         BRAS  RE,FNDNXT                                                        
         L     R3,FULL                                                          
         MVC   1(L'SRXPRD,R3),SRXPRD   PRODUCT NAME                             
         LA    R3,EDIBLK2+L'EDIBLK2-1                                           
         ST    R3,FULL                                                          
         BRAS  RE,FNDNXT                                                        
         L     R3,FULL                                                          
         AHI   R3,1                                                             
         EDIT  SRXEST,(3,(R3)),FILL=0  ESTIMATE                                 
         B     EDIINF60                                                         
*                                                                               
EDIINF20 MVC   EDIBLK1(L'SRANME),SRANME PRINT PUB NAME                          
         CLI   TYCDE,50                PRINT REP                                
         BNE   EDIINF40                NO                                       
*                                                                               
         MVC   EDIBLK1(L'SRCNME),SRCNME   CONTRA ACCOUNT NAME                   
         LA    R3,EDIBLK1+L'EDIBLK1-1                                           
         ST    R3,FULL                                                          
         BRAS  RE,FNDNXT                                                        
         L     R3,FULL                                                          
         PACK  DUB(6),SRCNTR+1(11)                                              
         MVC   DUB+5(1),SRCNTR+11   EDITION                                     
         GOTO1 PUBEDIT,DMCB,DUB,(C'S',1(R3))                                    
         CLC   SRCNTR+9(3),=C'ZZZ'                                              
         BNE   EDIINF30                                                         
         LA    RF,17(R3)                                                        
         CLI   0(RF),C','                                                       
         BE    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   1(3,RF),=C'ALL'                                                  
*                                                                               
EDIINF30 MVI   0(R3),C'('                                                       
         LA    R3,17(R3)                                                        
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         MVI   1(R3),C')'                                                       
*                                                                               
EDIINF40 OC    SRXPY(SRXPYL),SRXPY       EXTRA PAYMENT ELEMENT                  
         BZ    EDIINF50                                                         
*                                                                               
         LA    R3,EDIBLK2                                                       
         MVC   0(L'SRXCLI,R3),SRXCLI     USE CLIENT NAME                        
*                                                                               
         CLC   ALPHAID,=C'H7'     GROUPM CANNOT TRANSLATE THE *                 
         BNE   EDIINF45           IN THE CLIENT NAME                            
         LA    R3,EDIBLK2                                                       
         MVC   0(L'SRXCLI,R3),SRXCLI   CLIENT NAME                              
         LA    R6,L'SRXCLI        CUSTENH-2858 FOR E*TRADE                      
         BCTR  R6,0               CANNOT SKIP THIS FOR LEDGERS P/Q              
         CLI   0(R3),C'*'                                                       
         BNE   *+8                                                              
         MVI   0(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         BCT   R6,*-16                                                          
*                                                                               
EDIINF45 LA    R3,EDIBLK2+L'EDIBLK2-1                                           
         ST    R3,FULL                                                          
         BRAS  RE,FNDNXT                                                        
         L     R3,FULL                                                          
         MVC   1(L'SRXPRD,R3),SRXPRD                                            
         B     EDIINF60                                                         
*                                                                               
EDIINF50 MVC   EDIBLK1,SRCNME                                                   
         CLI   SROPRD,0                  OTHERS ELEMENT                         
         BE    EDIINF60                                                         
         LA    R3,EDIBLK2                                                       
         MVC   0(L'AC@PRO,R3),AC@PRO     C'PRODUCT='                            
         LA    R3,EDIBLK2+L'EDIBLK2-1                                           
         ST    R3,FULL                                                          
         BRAS  RE,FNDNXT                                                        
         L     R3,FULL                                                          
         MVI   0(R3),C'='                                                       
         MVC   1(L'SROPRD,R3),SROPRD     PRODUCT CODE                           
         LA    R3,EDIBLK2+L'EDIBLK2-1                                           
         ST    R3,FULL                                                          
         BRAS  RE,FNDNXT                                                        
         L     R3,FULL                                                          
         LA    RE,DICO           NEED ADDRESSABILITY TO THE                     
         AHI   RE,AC@ASYS-DICO   DATA DICTIONARY                                
         MVC   1(L'AC@JOB,R3),0(RE)      C'JOB='                                
         LA    R3,EDIBLK2+L'EDIBLK2-1                                           
         ST    R3,FULL                                                          
         BRAS  RE,FNDNXT                                                        
         L     R3,FULL                                                          
         MVI   0(R3),C'='                                                       
         MVC   1(L'SROJOB,R3),SROJOB     JOB CODE                               
*                                                                               
         USING E8ATABD,RF                                                       
EDIINF60 L     RF,=A(E8ATAB)        820 AGENCY TABLE                            
EDIINF65 CLI   0(RF),X'FF'          VALID AGENCY TO OUTPUT                      
         BE    EDIINF70                                                         
         CLC   ORIGINUM,E8AONUM     MATCH USER ID                               
         BE    EDIINF70                                                         
         LA    RF,E8ATBLNQ(RF)                                                  
         B     EDIINF65                                                         
EDIINF70 MVC   EDILOGO,E8ALOGO        SAVE LOGO CODE                            
         MVC   EDIRADC,E8ARACO        SAVE RETURN ADDRESS CODE                  
         MVC   EDISIG,E8ASIGCO      SAVE SIGNATURE CODE                         
         XIT1                                                                   
         DROP  RF                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* GET THE OFFICE NAME FOR BOTH ONE AND TWO CHARACTER FILES           *          
**********************************************************************          
         USING EDIDD,R9                                                         
OFFNAM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING ACMD,RF                                                          
         L     RF,AMONACC                                                       
         L     R2,AIO                                                           
         TM    ACMINDS,ACMINEWO        TWO BYTE OFFICE FILE?                    
         BZ    OFFNM10                 NO THEN READ 2D                          
         DROP  RF                                                               
*                                                                               
         USING OFFRECD,R2                                                       
         MVC   OFFRECD(L'OFFKEY),SPACES                                         
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,QCOMPANY                                                 
         MVC   OFFKOFF,SRTANL                                                   
         MVC   TEMPKEY(OFFKEND),0(R2)                                           
         B     OFFNM20                                                          
*                                                                               
         USING ACTRECD,R2                                                       
OFFNM10  MVC   ACTRECD(L'ACTKEY),SPACES                                         
         MVC   ACTKCPY,QCOMPANY                                                 
         MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),=C'2D'                              
         MVC   ACTKACT(L'SRKOFC),SRTANL                                         
         MVC   TEMPKEY(ACTKEND),0(R2)                                           
*                                                                               
OFFNM20  GOTO1 DATAMGR,DMCB,DMRDHI,ACCFIL,AIO,AIO                               
         CLC   0(L'ACTKCULA,R2),TEMPKEY                                         
         BNE   OFFNMX                                                           
*                                                                               
         L     R3,AIO                                                           
         AH    R3,DATADISP                                                      
*                                                                               
OFFNM30  CLI   0(R3),0                                                          
         BE    OFFNMX                                                           
         CLI   0(R3),X'20'                                                      
         BE    OFFNM40                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     OFFNM30                                                          
*                                                                               
         USING NAMELD,R3                                                        
OFFNM40  MVC   WORK,SPACES                                                      
         ZIC   R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)    HEADER+1 FOR EXMVC                           
         EXMVC R1,WORK,NAMEREC                                                  
OFFNMX   XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
**********************************************************************          
* BUILD RUNIT BLOCK AND CALL ACGETFORM TO PUT TO OUTPUT LINE         *          
* R4 POINTS TO FORMAT ENTRY                                                     
**********************************************************************          
         USING ACBANKD,R2                                                       
         USING EDIDD,R9                                                         
GORUNIT  NTR1  BASE=*,LABEL=*                                                   
         L     R9,AEDIWRK                                                       
         L     R2,AFTROUTB         CLEAR ACGETFORM BLOCK                        
         LR    RE,R2                                                            
         LA    RF,ACBLNQ                                                        
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,AIO              CLEAR IO TO SPACES                           
         LHI   RF,2000                                                          
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0                                                            
*                                                                               
         MVC   ACBCKDLV,EDICKDLV      CHECK DELIVERY FLAG  SPEC-40627           
         MVC   ACBIDICD,IDABBR        CHARACTER IDI SIGNON                      
         ZAP   ACBFULC5,=P'0'                                                   
*                                                                               
*MN MOVE THIS TO SEPARATE ROUTINE                                               
*                                                                               
         BRAS  RE,RPVEND              POPULATE REP VENDOR FIELDS                
*                                                                               
*        CLI   SRTYPE,34                                                        
*        BNE   *+16                                                             
*        MVC   ACBREPVN,SPACES        CLEAR                                     
*        MVC   ACBREPVN(8),SRCNTR+4   STORE STATION CALL LETTERS                
*                                                                               
*        CLI   SRTYPE,50                                                        
*        BNE   *+16                                                             
*        MVC   ACBREPVN,SPACES        CLEAR                                     
*        MVC   ACBREPVN,SRCNME                                                  
*                                                                               
*        MVC   ACBREPV3,ACBVACC                                                 
*                                                                               
*        CLI   SRTYPE,45                                                        
*        BNE   *+10                                                             
*        MVC   ACBREPV2(3),SRCNTR+3                                             
*                                                                               
*        CLI   SRTYPE,33              SS,ST                                     
*        BNE   *+10                                                             
*        MVC   ACBREPV2(12),SRACC+3                                             
*                                                                               
*        CLI   SRTYPE,34              SS,ST                                     
*        BNE   *+10                                                             
*        MVC   ACBREPV2(9),SRCNTR+3                                             
*                                                                               
*        CLI   SRTYPE,35              SS,ST                                     
*        BNE   *+10                                                             
*        MVC   ACBREPV2(9),SRCNTR+3                                             
*                                                                               
*        CLI   SRTYPE,49              SP,SQ                                     
*        BNE   *+22                                                             
*        MVC   ACBREPV2,SRANME                                                  
*        MVC   ACBREPV3,SPACES                                                  
*        MVC   ACBREPV3(11),SRACC+4                                             
*                                                                               
*        CLI   SRTYPE,50              SP,SQ                                     
*        BNE   *+22                                                             
*        MVC   ACBREPV2,SRCNME                                                  
*        MVC   ACBREPV3,SPACES                                                  
*        MVC   ACBREPV3(8),SRCNTR+1                                             
*                                                                               
*        CLI   SRTYPE,13              SP,SQ                                     
*        BNE   GORN01B                                                          
*        CLI   SRCNTR+3,C' '                                                    
*        BNE   GORN01A                                                          
*        MVC   ACBREPV2,SRANME                                                  
*        MVC   ACBREPV3(11),SRACC+4                                             
*        B     GORN01B                                                          
*                                                                               
*ORN01A  MVC   ACBREPV2,SRCNME                                                  
*        MVC   ACBREPV3(9),SRCNTR                                               
*                                                                               
*ORN01B  DS    0H                                                               
*MN MOVE THIS TO SEPARATE ROUTINE                                               
*                                                                               
         MVC   ACBCUL,SRCNTR+1        CONTRA UNIT/LEDGER                        
         MVC   ACBCNTR,SRCNTR+3       CONTRA UNIT/LEDGER                        
         CLI   SRACC+2,C'P'           IF SP THERE IS NO COMPANY                 
         BNE   *+16                   UNIT LEDGER                               
         MVC   ACBCUL,SPACES          CONTRA UNIT/LEDGER                        
         MVC   ACBCNTR,SRCNTR         CONTRA UNIT/LEDGER                        
         MVC   ACBMOS,SRTMMOS         MOS FROM TRANSACTION BATCH                
*                                                                               
                                                                                
         MVC   ACBOUTR,AIO         USE AIO1,2,3 AS OUTPUT AREA FOR TAPE         
         MVC   ACBOUTRL,=AL2(IOTTLNQ) OUTPUT AREA LENGTH OF AIO 1,2,3           
         ST    R4,ACBFRMT          ADDRESS OF FORMAT                            
         MVC   ACBRECLN,EDIRECLN   RECORD LENGTH                                
         MVC   ACBALPHA,ALPHAID                                                 
         MVC   ACBRMODE,BYTE       RUNIT MODE                                   
         MVC   ACBRSTAT,EDIRSTAT   STATUS BYTE                                  
         MVC   ACBORIGN,EDIORGN    IDI ORIGIN ID NAME                           
         MVC   ACBIDIDN,EDIDIDN    IDI DESTINATION NAME                         
         MVC   ACBTIMEP,EDITIMEP   TIME (IN PACKED)                             
         MVC   ACBBLCNT,EDIBLCNT   BLOCK COUNT                                  
* DSFTK-150                                                                     
         MVC   ACBPCTY,EDIPTYPE    PCARD SWIPE TYPE                             
* DSFTK-150                                                                     
         MVI   WORK,0              CLEAR FIRST BYTE OF WORK                     
         OC    SRTINTA1+(1*L'SRTINTA1),SPACES  MAKE SURE STATE IS UPC           
         OC    SRTINTA2+(2*L'SRTINTA2),SPACES  ON LINES 2,3 OR 4                
         OC    SRTINTA3+(3*L'SRTINTA3),SPACES                                   
         LA    R5,SRTINTA1        POINT TO ADDRESS                              
         LA    R3,L'SRTINTA1      LENGTH OF ONE ADDRESS LINE                    
         LA    R4,SRTINTA3        THIRD ADDRESS LINE                            
*                                                                               
GORN02   CR    R5,R4              POINTING TO FIRST LINE?                       
         BE    GORN08             MUST BE AN ERROR                              
         CLC   0(L'SRTINTA3,R4),SPACES                                          
         BE    GORN03                                                           
         GOTO1 ADFORM,DMCB,((R3),(R4)),(25,INTBCTY),                   +        
               INTBST,(10,INTBZIP),(2,INTBCTRY),(30,INTBCNME)                   
         TM    0(R1),X'80'        80=SERIOUS ERROR                              
         BO    GORN08                                                           
         CLC   INTBCTY,SPACES                                                   
         BNE   GORN08                                                           
         CLC   0(L'SRTINTA1,R4),SPACES   IF ZERO, THEN NO ADDRESS LINE          
         BNH   *+8                                                              
GORN03   AHI   R3,L'SRTINTA1      SEND ANOTHER LINE                             
         AHI   R4,-L'SRTINTA1     BACK UP                                       
         B     GORN02                                                           
*                                                                               
GORN08   MVC   ACBSWIFT,SRTSWIFT     INTL SWIFT CODE                            
         MVC   ACBINTB#,SRTINTB#     INTL BANK ACCOUNT NUMBER                   
         MVC   ACBINTNM,SRTINTNM     INTL BANK NAME                             
*        INTL BANK ADDRESS LINES 1,2,3                                          
         MVC   ACBINTA1(L'ACBINTA1+L'ACBINTA2+L'ACBINTA3),SRTINTA1              
*                                                                               
         MVC   ACBIBCTY,INTBCTY      INTL BANK CITY                             
         MVC   ACBIBST,INTBST        INTL BANK STATE                            
         MVC   ACBIBZIP,INTBZIP      INTL BANK ZIP                              
         MVC   ACBIBCTR,INTBCTRY     INTL BANK COUNTRY                          
         MVC   ACBIBCTN,INTBCNME     INTL BANK COUNTRY NAME                     
         OC    ACBIBCTY(ACBREPV3-ACBIBCTY),SPACES                               
         MVC   ACBOFFCT,SRTANL       TRANSACTION OFFICE                         
*                                                                               
         CLI   ACBRMODE,HDR        CLEAR NAMES FOR EACH HEADER                  
         BNE   GORN10                                                           
*DSFTK-137                                                                      
         AP    HDRTOT,=P'1'                                                     
*DSFTK-137                                                                      
         CLC   EDIFORMT,=CL10'BOM80' FOR BANK OF MONTREAL DO NOT                
         BE    *+10                  CLEAR DETAIL COUNTER                       
         ZAP   PKCNT4,=P'0'         INIT COUNTER FOR ALL HEADERS                
         MVC   LSTFLDS(LSTLNQ),SPACES                                           
*                                                                               
GORN10   CLI   EDIMAIL,C'4'        IS ADDRESS INVALID?                          
         BNE   *+12                NO-SKIP THE REST                             
         OI    ACBRSTAT,ACBINVML   SET INVALID ADDRESS BIT                      
         B     GORN30                                                           
         CLI   EDIMAIL,C'2'        DO WE HAVE A USA ADDRESS?                    
         BE    GORN30              YES-SKIP THE REST                            
         CLI   EDIMAIL,C'1'        DO WE HAVE A CAN ADDRESS?                    
         BNE   GORN20                                                           
         OI    ACBFLAG,ACBCAN                                                   
         TM    EDIRSTAT,EDICANF    DOES THIS BANK TREAT CANADIAN ADDRS          
         BZ    GORN30                                                           
GORN20   OI    ACBFLAG,ACBFRGN                                                  
*        EDIT  EDIISEQ,ACBISEQ,ZERO=NOBLANK,FILL=0                              
*                                                                               
GORN30   TM    DTRSW,DTR820        EDI820 DATA TRANSFER?                        
         BZ    *+8                                                              
         OI    ACBFLAG,ACB820                                                   
         TM    DTRSW,DTREFT        EFT TRANSMISSION?                            
         BZ    *+8                                                              
         OI    ACBFLAG,ACBEFT                                                   
         TM    LGSW,CANAD          CANADIAN FILE?                               
         BZ    *+8                                                              
         OI    ACBFLAG,ACBCADC     THAN IT'S CANADIAN CURRENCY                  
         MVC   ACBBLK2,EDIBLK2                                                  
*SPEC-28147                                                                     
         CLC   EDIFORMT,=CL10'BOA4010'   FOR NEW VERSION WE NEED TO             
         BE    *+14                      ALWAYS SEND ACBBLK1                    
*SPEC-28147                                                                     
         CLC   EDIFORMT,=CL10'BOA4004'                                          
         BE    *+14                                                             
         MVC   ACBBLK1,EDIBLK1         FOR BOA ONLY PUT OUT EDIBLK1             
         B     GORN60                  INFO IF INFO HAS CHANGED                 
         CLC   EDIBLKS,EDIBLK1                                                  
         BE    GORN50                                                           
         CLC   EDIBLK2,SPACES          IS INFO2 SPACES?                         
         BNE   GORN40                  NO, CONTINUE                             
         MVC   EDIBLK2,EDIBLK1         YES, COPY INFO TO INFO2                  
         B     GORN50                                                           
***                                                                             
GORN40   MVC   ACBBLK1,EDIBLK1                                                  
*                                                                               
GORN50   CLI   ACBRMODE,DTL                                                     
         BNE   *+10                                                             
         MVC   EDIBLKS,EDIBLK1                                                  
*                                                                               
GORN60   GOTO1 DATCON,DMCB,(1,TODAY1),(0,WORK)                                  
         MVC   ACBFSEQ#(5),WORK+1          SEQ# IS YMMDD                        
*MN SPEC-47579                                                                  
         MVC   AREA(6),WORK                                                     
*MN SPEC-47579                                                                  
         L     RF,=A(RTGEN)                                                     
         CLI   0(RF),C'G'                                                       
         BNE   *+20                                                             
         MVC   ACBFSEQ#+5(4),1(RF)         SIGNIFICANT GENERATION #             
         MVC   ACBFLIDM,1(RF)              STORE SIGNIFICANT GEN#               
         B     GORN90                                                           
*                                                                               
         TIME  DEC,DUB,LINKAGE=SYSTEM                                           
         UNPK  WORK(15),DUB                                                     
         MVC   ACBFSEQ#+5(4),WORK+2        USES MINS AND SECS AS DFT            
         MVC   ACBRSEQ#,WORK+4             STORE SECONDS FOR RANDOM SEQ         
*                                                                               
         L     R1,=A(RTDSN)                                                     
         LA    R0,L'RTDSN+L'RTGEN                                               
GORN70   CLC   =C'.G',0(R1)        FIND THE BEGINNING OF THE GENER #            
         BE    *+16                                                             
         AHI   R1,1                                                             
         BCT   R0,GORN70                                                        
         B     GORN90                                                           
*                                                                               
         LA    RE,2(R1)                                                         
         LA    RF,WORK                                                          
         LA    R0,4                                                             
GORN80   CLI   0(RE),C'0'          MAKE SURE ITEM IS NUMERIC                    
         BL    GORN90                                                           
         CLI   0(RE),C'9'                                                       
         BH    GORN90                                                           
         MVC   0(1,RF),0(RE)       SIGNIFICANT GENERATION #                     
         AHI   RF,1                                                             
         AHI   RE,1                                                             
         BCT   R0,GORN80                                                        
         MVC   ACBFSEQ#+5(4),WORK                                               
         MVC   ACBFLIDM,WORK               STORE SIGNIFICANT GEN#               
*                                                                               
GORN90   MVC   ACBBSEQ#,ACBFSEQ#                                                
*MN SPEC-47579                                                                  
         TM    RNSW,SOON                                                        
         BO    GORN95                                                           
         TM    DTRSW,DTRMQ                                                      
         BNO   GORN95                                                           
         MVC   ACBFSEQ#(4),AREA+2                                               
         MVC   ACBFSEQ#+4(5),SVJOBNO                                            
GORN95   DS    0H                                                               
*                                                                               
*MN SPEC-47579                                                                  
         EDIT  TRNSEQ#,ACBTSEQ#,FILL=0     TRANSACTION SEQUENCE #               
         MVC   ACBBSTY,EDIBSTY     LOGO/RETURN ADDR/SIG CODES                   
*                                                                               
         MVC   BYTE,EDIMAIL                                                     
         TM    SRSTAT,SRMAIL       IS THIS OVERNIGHT TO AGENCY?                 
         BNO   *+8                                                              
         MVI   BYTE,C'5'           SHOW OVERNIGHT TO AGENCY                     
*SPEC-44172                                                                     
         TM    SRSTAT,SRMAIL2      OV DIRECT TO VENDOR                          
         BNO   *+8                                                              
         MVI   BYTE,C'6'           SHOW OV DIRECT TO VENDOR                     
*SPEC-44172                                                                     
         L     R3,=A(MAILTAB)                                                   
GORN100  CLI   0(R3),0             DEFAULT                                      
         BE    GORN120                                                          
         CLC   RCCOMPFL,0(R3)      MATCH ON COMPANY                             
         BNE   GORN110                                                          
         SR    R1,R1                                                            
         LHI   R1,L'EDIFORMT                                                    
                                                                                
         CLI   RCCOMPFL,X'B2'          THIS IS AN EXCEPTION TO THE              
         BNE   *+14                    WACH300? RULE                            
         CLC   =C'WACH300I',EDIFORMT                                            
         BE    GORN105                                                          
                                                                                
         CLC   =C'WACH300',EDIFORMT    THIS IS SO CHANGES APPLY TO ALL          
         BNE   *+8                     WACH300? FORMATS                         
         LHI   R1,7                                                             
GORN105  BCTR  R1,0                                                             
         EXCLC R1,EDIFORMT,1(R3)                                                
         BNE   GORN110                                                          
         CLI   11(R3),C'0'          IS THIS THE COMPANY DEFAULT                 
         BE    GORN120                                                          
         CLC   BYTE,11(R3)                                                      
         BE    GORN120                                                          
GORN110  SR    R1,R1                                                            
         IC    R1,12(R3)                                                        
         LA    R3,13(R1,R3)         ADD OVERHEAD AND VARIABLE LENGTH            
         B     GORN100                                                          
*                                                                               
GORN120  SR    R1,R1                                                            
         IC    R1,12(R3)            GET LENGTH OF DATA                          
         CHI   R1,L'ACBMLCDE                                                    
         BL    *+8                  GET DISPL INTO DATA                         
         LA    R1,L'ACBMLCDE                                                    
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACBMLCDE(0),13(R3)   MAIL ID (TEMPORARILY)                       
*                                                                               
         MVC   ACBLDG,SRACC+2      LEDGER                                       
         MVC   ACBSBNK#,EDISBNKA   SOURCE BANK ACCT # (FROM SC ACCT)            
         OC    ACBSBNK#,SPACES                                                  
         MVC   ACBSRTE#,EDISROUT   SOURCE ROUTING NUMBER                        
         OC    ACBSRTE#,SPACES                                                  
         MVC   ACBDBNK#,EDIDBNKA   DEST BANK ACCT# (FROM VENDOR)                
         OC    ACBDBNK#,SPACES                                                  
         MVC   ACBDRTE#,EDIDROUT   DESTINATION ROUTING NUMBER                   
         OC    ACBDRTE#,SPACES                                                  
         TM    DTRSW,DTR820        FOR 820 DO NOT HAVE ROUTING #'S              
         BO    GORUN122                                                         
         CLI   ACBRMODE,HDR                                                     
         BNE   GORUN122                                                         
* DSFTK-150                                                                     
         CLC   ACBDRTE#,SPACES                                                  
         BNE   *+10                                                             
         MVC   ACBDRTE#,=CL9'000000000'                                         
* DSFTK-150                                                                     
         PACK  DUB,ACBDRTE#(8)                                                  
         AP    EDIENTH(6),DUB(8)   KEEP ADDING THE ROUTING NUMBERS              
GORUN122 MVC   ACBCSHAC,SVBANK+3   CASH ACCOUNT                                 
         ZAP   DUB,CHNUM           CHECK NUMBER                                 
         AP    DUB,=P'1'           BUMP UP BY ONE FOR REAL NUMBER               
         EDIT  (P8,DUB),ACBCHECK,FILL=0                                         
         MVC   ACBVULA,SRACC+1     VENDOR U/L/ACC                               
         MVC   ACBBNKNM,EDIBKNME   BANK NAME                                    
*                                                                               
         MVC   ACBPAYEE,SRANME     VENDOR NAME                                  
         MVC   ACBTVENC,SPACES                                                  
         LA    RF,SRANME           VENDOR NAME IF CNTRA STARTS W/SPACES         
         CLC   SRCNTR+4(8),SPACES                                               
         BE    *+14                                                             
         MVC   ACBTVENC,SRCNTR     TRUE VENDOR                                  
         LA    RF,SRCNME           REP IF CONTRA IS NOT SPACES                  
         MVC   ACBPUBNM,0(RF)      PUBLICATION NAME                             
         MVC   ACBCLI,SPACES                                                    
         TM    SYEQU,MEDQ          MEDIA LEDGER?                                
         BZ    *+20                NO GET CLIENT NAME FROM SJ ACCT              
         MVC   ACBCLI,SRTCLIC                                                   
         MVC   ACBCLINM(L'SRXCLI),SRXCLI  ELSE GET CLIENT NME FROM SORT         
         B     GORN130                                                          
*                                                                               
         CLC   SRCNTR+1(2),=C'SJ'                                               
         BNE   GORN130                                                          
         CLC   SRTCLIC,SPACES                                                   
         BNH   GORN130                                                          
         MVC   ACBCLI,SRTCLIC      CLIENT                                       
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'SRTCLIC),SRTCLIC                                          
         BRAS  RE,SJNAM                                                         
         MVC   ACBCLINM,WORK                                                    
*                                                                               
GORN130  MVC   ACBPRO,SPACES       PRODUCT                                      
         TM    SYEQU,MEDQ          MEDIA LEDGER?                                
         BZ    GORN150                                                          
         MVC   ACBPRO,SROPRD                                                    
         CLC   SROPRD,SPACES                                                    
         BH    GORN140                                                          
         MVC   ACBPRO,SRTREF       1ST 3 CHARACTERS ARE REFERENCE               
GORN140  MVC   ACBPRDNM(L'SRXPRD),SRXPRD  ELSE GET PROD NME FROM SORT           
         B     GORN160                                                          
GORN150  CLC   SROPRD,SPACES                                                    
         BNH   GORN160                                                          
         MVC   ACBPRO,SROPRD                                                    
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'SRTCLIC),SRTCLIC                                          
         MVC   WORK+L'SRCLI(L'SROPRD),SROPRD                                    
         BRAS  RE,SJNAM                                                         
         MVC   ACBPRDNM,WORK                                                    
*                                                                               
GORN160  MVC   ACBJOB,SPACES                                                    
         TM    SYEQU,MEDQ          MEDIA LEDGER?                                
         BZ    GORN170                                                          
         CLC   SROEST,SPACES                                                    
         BNH   GORN180                                                          
         MVC   ACBJOB(L'SROEST),SROEST     ESTIMATE IS THE JOB                  
         B     GORN180                                                          
GORN170  CLC   SROJOB,SPACES                                                    
         BNH   GORN180                                                          
         MVC   ACBJOB(L'SROJOB),SROJOB                                          
         MVC   WORK,SPACES              GET JOB NAME FROM SJ ACCOUNT            
         MVC   WORK(L'SRCLI),SRTCLIC                                            
         MVC   WORK+L'SRCLI(L'SROPRD),SROPRD                                    
         MVC   WORK+L'SRCLI+L'SROPRD(L'SROJOB),SROJOB                           
         BRAS  RE,SJNAM                                                         
         MVC   ACBJOBN,WORK                                                     
*                                                                               
GORN180  OC    ACBCLINM(L'ACBCLINM+L'ACBPRDNM),SPACES                           
         MVC   ACBEST#,SRXEST      ESTIMATE NUMBER                              
*                                                                               
         CLI   ACBRMODE,DTL           ONLY CHECK NAMES IN DTL                   
         BNE   GORN210                                                          
         AP    PKCNT4,=P'1'           ADD ONE TO COUNTER                        
         ZAP   ACBFULC3,PKCNT4                                                  
         MVI   ACBPUBIN,ACBPUBRP      DEFAULT IS REPEATING                      
         CLC   LSTPUBNM,ACBPUBNM      SAME VENDOR NAME AS BEFORE?               
         BE    GORN190                                                          
         MVI   ACBPUBIN,ACBPUBNW      NAME IS NEW SO MARK IT AS SUCH            
         MVC   LSTPUBNM,ACBPUBNM      SAVE OFF VENDOR NME FOR NEXT TIME         
         B     GORN200                                                          
*                                                                               
GORN190  MVI   ACBNMEIN,ACBNMERP      DEFAULT IS REPEATING                      
         CLC   LSTCLINM,ACBCLINM      SAME CLIENT NAME AS BEFORE?               
         BE    *+14                                                             
GORN200  MVI   ACBNMEIN,ACBNMENW      NAME IS NEW SO MARK IT AS SUCH            
         MVC   LSTCLINM(L'SRXCLI),SRXCLI   CLIENT NAME                          
*                                                                               
GORN210  MVC   ACBCKAMT,SRCAMT      CHECK AMOUNT                                
*                                                                               
         ZAP   ACBTRNCT,SRTRNCT     TRANSACTION COUNT SPEC-40627                
*MN SPEC-47975                                                                  
         CP    SRTRNCT,=P'800'      IS TRANS COUNT OVER 800 LIMIT               
         BNH   GORN215                                                          
         CLI   ACBRMODE,HDR         AND PROCESSING HEADER RECORD                
         BNE   GORN215                                                          
         CLC   SVSRACC,SRACC        HAS ACCOUNT CHANGED                         
         BE    GORN212                                                          
         MVC   SVSRACC,SRACC        SAVE ACCOUNT                                
         ZAP   SVSRTCNT,SRTRNCT     SAVE COUNT                                  
         SP    SVSRTCNT,=P'800'     SUBTRACT FIRST 800 PROCESSED                
         ZAP   ACBTRNCT,=P'800'     REPORT CHECK COUNT                          
         B     GORN215                                                          
*                                                                               
GORN212  DS    0H                                                               
         CP    SVSRTCNT,=P'800'     IS THERE STILL > 800 LEFT                   
         BNH   GORN214                                                          
         SP    SVSRTCNT,=P'800'     SUBTRACT NEXT 800 PROCESSED                 
         ZAP   ACBTRNCT,=P'800'     REPORT CHECK COUNT                          
         B     GORN215                                                          
*                                                                               
GORN214  ZAP   ACBTRNCT,SVSRTCNT    FINAL PROCESSING COUNT                      
GORN215  DS    0H                                                               
*MN SPEC-47975                                                                  
*                                                                               
         CLC   EDIFORMT,=CL10'BOM80'  FOR BANK OF MONTREAL NO REMITT            
         BNE   GORN220                                                          
         CLI   ACBRMODE,CTRL                                                    
         BNE   GORN220                                                          
         ZAP   ACBFULC3,PKCNT4                                                  
GORN220  CLI   ACBRMODE,HDR        ONLY TOTAL CHECK AMOUNTS IN HDR              
         BNE   *+10                                                             
         AP    TOTCASH,SRCAMT      KEEP RUNNING TOTAL OF ALL CASH               
         ZAP   ACBTNET,TOTCASH                                                  
         ZAP   ACBFULC2,TOTTRNS                                                 
         ZAP   ACBNET,SRTAMT       TRANSACTION AMT (NET)                        
         ZAP   ACBCD,SRTCD         CASH DISCOUNT                                
         ZAP   ACBNETCD,ACBNET                                                  
         SP    ACBNETCD,ACBCD                                                   
         ZAP   DUB,SRTAMT                                                       
         AP    DUB,SRTCD                                                        
         ZAP   ACBGROSS,DUB        GROSS AMOUNT                                 
         ZAP   ACBENTH,EDIENTH     ENTRY HASH                                   
         CLI   ACBRMODE,CTRL       TOTAL C/D AND GROSS IN CHK TRAILER           
         BNE   GORN230                                                          
         ZAP   ACBTCSD,PRDCD       HOLDS TOTAL C/D FOR CHECK                    
         AP    PRDPAY,PRDCD        ADD TOTAL C/D FOR CHECK TO TOTAL NET         
         ZAP   ACBTGRS,PRDPAY      FOR CHECK TO GET TOTAL GROSS FOR CHK         
         ZAP   ACBBTOT,BATAMT      BATCH AMOUNT                                 
         ZAP   ACBBCNT,BATCNT      BATCH RECORD COUNT                           
GORN230  CLI   ACBRMODE,TRL                                                     
         BNE   GORN232                                                          
         ZAP   ACBBTOTA,TOTBAMT    TOTAL OF ALL BATCHES                         
         ZAP   ACBBCNTA,TOTBCNT    TOTAL BATCH RECORD COUNT                     
*                                                                               
GORN232  CLI   ACBRMODE,TRL                                                     
         BE    *+12                                                             
         CLI   ACBRMODE,TRL2                                                    
         BNE   GORN235                                                          
         USING FORMD,RF                                                         
         USING FRMTABD,RE                                                       
         L     RF,AFRMBLK                                                       
         LA    RE,FFRMFRM                                                       
         TM    FRMSTAT,FRMHTCNT    INCLUDE THDR/TRAILER IN COUNT?               
         BZ    *+10                                                             
         AP    PKCNT5,=P'1'                                                     
         CLI   ACBRMODE,TRL2       FOR TRL2 JUST REPEAT THE COUNTER             
         BE    *+10                DON'T ADD THE CHECKS AGAIN                   
         AP    PKCNT5,EFTCK#                                                    
         ZAP   ACBFULC5,PKCNT5                                                  
         DROP  RE,RF                                                            
*                                                                               
GORN235  MVC   ACBTDTE,SRTDTE      TRANSACTION DATE                             
         MVC   ACBCDTE,SRCDTE      CHECK DATE (RCDATE OR QSTART)                
*        MVC   ACBPCDTE,PRCKDTE    PREV CHK DTE (FOR DEPENDENCIES)              
         MVC   ACBPDTE,ACBTDTE     USES TRANSACTION DATE AS DEFAULT             
*SPEC-28147                                                                     
         CLC   EDIFORMT,=CL10'BOA4010'   FOR BOA-ACBPERD MUST BE INITED         
         BE    GORN238                                                          
*SPEC-28147                                                                     
         CLC   EDIFORMT,=CL10'BOA4004'   FOR BOA-ACBPERD MUST BE INITED         
         BNE   GORN240                                                          
GORN238  GOTO1 DATCON,DMCB,(1,SRTDTE),(10,ACBPERDT)  TRANSACTION DATE           
*                                                                               
GORN240  CLC   SRXPER,SPACES                                                    
         BNH   GORN290                                                          
         CLC   ACBPERDT,SPACES       IF ACBPERDT FILLED IN ABOVE FOR            
         BNH   *+10                  BOA MUST CLEAR PART FILLED IN              
         XC    ACBPERDT(8),ACBPERDT                                             
         MVC   WORK(6),SRXPER                                                   
         CLI   SRXPER+6,C' '       ANY END DATE                                 
         BNH    *+10                                                            
         MVC   WORK(6),SRXPER+6                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,ACBPDTE)                                 
         CLC   =C'WACH300',EDIFORMT  FORCE DAY OF 01 FOR WACHOVIA               
         BNE   *+16                  IF DAY IS BLANK                            
         CLI   ACBPDTE+2,X'00'                                                  
         BNE   *+8                                                              
         MVI   ACBPDTE+2,X'01'                                                  
         CLI   SRXPER+6,C' '                                                    
         BH    GORN250             IF NO END DO MMMDD/YY                        
         GOTO1 DATCON,DMCB,(0,SRXPER),(8,ACBPERDT)                              
         B     GORN290                                                          
*                                                                               
GORN250  CLI   SRACC+3,C'N'                                                     
         BE    GORN270             NETWORK IS SPECIAL                           
*                                                                               
GORN260  GOTO1 DATCON,DMCB,(0,SRXPER+6),(9,ACBPERDT) MMM/YY BROADCAST           
         B     GORN290                                                          
*                                                                               
GORN270  CLC   SRXPER(6),SRXPER+6     FOR NETWORK IF START=END                  
         BNE   GORN280                                                          
         GOTO1 DATCON,DMCB,(0,SRXPER),(8,ACBPERDT) MMMDD/YY                     
         B     GORN290                                                          
*                                                                               
GORN280  DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,SRXPER),(0,SRXPER)                                
         GOTO1 DATCON,DMCB,(0,SRXPER+6),(0,SRXPER+6)                            
         GOTO1 GETBROAD,DMCB,(1,SRXPER),WORK,GETDAY,ADDAY                       
         GOTO1 GETBROAD,DMCB,(1,SRXPER+6),WORK+12,GETDAY,ADDAY                  
         CLC   WORK(12),WORK+12         IS IT ONE BROADCAST MONTH               
         BE    GORN260                  IF IT IS DO IT LIKE SPOT                
         GOTO1 DATCON,DMCB,(0,SRXPER),(8,ACBPERDT) IF START NOT = END           
         GOTO1 DATCON,DMCB,(0,SRXPER+6),(8,WORK)  MMMDD-DD/YY                   
         MVI   ACBPERDT+5,C'-'                                                  
         MVC   ACBPERDT+6(5),WORK+3                                             
*                                                                               
GORN290  MVC   ACBTTYP,CHKTYPE     TRANSACTION TYPE                             
         MVC   ACBPTYP,EDIREMDL    PAYMENT TYPE                                 
         CLC   SRXINV,SPACES       FOR MEDIA CHKS INV COMES FROM                
         BNH   *+14                46 ELEM.  FOR PROD CHKS INV COMES            
         MVC   ACBINV#(L'SRXINV),SRXINV  FROM REFERENCE                         
         B     GORN300                                                          
         MVC   ACBINV#(L'SRTREF),SRTREF                                         
         CLC   SRPLINV,SPACES      LONGER INVOICE EXISTS?                       
         BNH   GORN300             USE IT BUT THEN DON'T FILL IN TRUE           
         MVC   ACBINV#(L'SRPLINV),SRPLINV REF OR ELSE WILL NOT USE THIS         
         B     GORN302                                                          
*                                                                               
GORN300  MVC   ACBTREF,SRTRURF     TRUE REFERENCE                               
GORN302  OC    ACBINV#,SPACES                                                   
         CLI   SRTNRL,0            ANY NARRATIVE?                               
         BE    GORN310                                                          
         ZIC   R1,SRTNRL                                                        
         BCTR  R1,0                                                             
         EXMVC R1,ACBINVDS,SRTNRR  NARRATIVE                                    
         TM    EDIRSTAT,AGYNASK    ARE ASTERISKS INVALID IN NARRATIVE?          
         BZ    GORN310             NO                                           
         MVC   SVNARRWK,SPACES     YES SO STRIP OUT THE ASTERISKS *             
         MVC   SVNARRWK(L'ACBINVDS),ACBINVDS                                    
         MVC   ACBINVDS,SPACES                                                  
         ZIC   R1,SRTNRL                                                        
         LA    RE,ACBINVDS                                                      
         LA    RF,SRTNRR                                                        
GORN305  CLI   0(RF),C'*'                                                       
         BE    *+14                                                             
         MVC   0(1,RE),0(RF)                                                    
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         BCT   R1,GORN305                                                       
*                                                                               
GORN310  MVC   ACBVCNME,EDICNTN    VENDOR CONTACT NAME                          
         MVC   ACBVFAX#,EDIFAX     VENDOR FAX NUMBER                            
         MVC   ACBVEML,EDIEMAIL    VENDOR EMAIL ADDRESS                         
         MVC   ACBCPYNM,EDICNM     COMPANY NAME                                 
         MVC   ACBPHN,EDICPHN      VENDOR PHONE NUMBER                          
         MVC   ACBRMTDV,EDIREMDC                                                
*                                                                               
*MN SPEC-46328                                                                  
         MVC   ACBSCFF1(ACBSCFFL),EDIFFTX1   MOVE IN ALL 8 FIELDS               
*MN SPEC-46328                                                                  
         MVC   ACBIADR,EDIIADDR    IDI ORIGIN ADDRESS                           
*SPEC-40627                                                                     
         MVC   ACBIDAD1(L'ACBIDAD1+L'ACBIDAD2),EDIIDAD1                         
*SPEC-40627                                                                     
         MVC   ACBIDCTY,EDIICTY    IDI DESTINATION CITY                         
         MVC   ACBIDST,EDIIST      IDI DESTINATION STATE                        
         MVC   ACBIDZIP,EDIIZIP    IDI DESTINATION ZIP                          
         MVC   ACBIDCRY,EDIICTRY   IDI DESTINATION COUNTRY                      
         MVC   ACBIDCRN,EDIICNME   IDI DESTINATION COUNTRY NAME                 
*                                                                               
         TM    EDIRSTAT,EDIADDRB   DOES THIS FORMAT BREAK UP THE ADDR?          
         BZ    GORN320             NO SO MOVE IN THE WHOLE ADDR THEN            
         ZIC   R1,EDICADDL         R1 CONTAINS # OF STREET ADDR LINES           
         MHI   R1,L'ADRADD1                                                     
         BCTR  R1,0                                                             
         EXMVC R1,ACBADDR1,EDICADD1  JUST MOVE IN THE STREET ADDRESS            
         B     *+10                                                             
GORN320  MVC   ACBADDR1(L'ADRADD1*4),EDICADD1   PAYER ADDR LNS 1,2,3,4          
         OC    ACBADDR1(L'ADRADD1*4),SPACES                                     
                                                                                
         TM    ACBRSTAT,AGYADDRD          EXCLUDE DUPLICATE CITY NAME           
         BZ    GORN325                                                          
                                                                                
         CLC   EDICCTY,SPACES             COULD BE SPACES DEPENDENT             
         BE    GORN325                    ON MODE                               
                                                                                
         LA    R1,L'ACBCTY-1              NEED COMPARE LENGTH FOR CITY          
         LA    RF,EDICCTY+L'ACBCTY-1      POINT TO LAST CHAR IN CITY            
GORN321  CLI   0(RF),C' '                                                       
         BH    GORN322                                                          
         BCTR  RF,0                                                             
         BCT   R1,GORN321                                                       
                                                                                
GORN322  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ACBADDR2(0),EDICCTY                                              
         BNE   GORN322A                                                         
         MVC   ACBADDR2(L'ACBADDR2+L'ACBADDR3+L'ACBADDR4+L'ACBADDR5),SPX        
               ACES                                                             
         B     GORN325                                                          
                                                                                
GORN322A EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ACBADDR3(0),EDICCTY                                              
         BNE   GORN322B                                                         
         MVC   ACBADDR3(L'ACBADDR3+L'ACBADDR4+L'ACBADDR5),SPACES                
         B     GORN325                                                          
                                                                                
GORN322B EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ACBADDR4(0),EDICCTY                                              
         BNE   GORN322C                                                         
         MVC   ACBADDR4(L'ACBADDR4+L'ACBADDR5),SPACES                           
         B     GORN325                                                          
                                                                                
GORN322C EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ACBADDR5(0),EDICCTY                                              
         BNE   GORN325                                                          
         MVC   ACBADDR5,SPACES                                                  
                                                                                
GORN325  DS    0H                                                               
         MVC   ACBCTY,EDICCTY                   PAYER CITY                      
         MVC   ACBST,EDICST                     PAYER STATE                     
         MVC   ACBZIP,EDICZIP                   PAYER ZIP                       
         MVC   ACBCTRY,EDICCTRY                 PAYER COUNTRY                   
         MVC   ACBCTRN(L'EDICCNME),EDICCNME     PAYER COUNTRY NAME              
*                                                                               
         CLC   =C'WACH300',EDIFORMT                                             
*        CLC   EDIFORMT,=CL10'WACH300'   SPECIAL FOREIGN ADDRESS CODE           
         BNE   GORN340                                                          
         TM    ACBFLAG,ACBFRGN     IS THIS A FOREIGN ADDRESS?                   
         BNO   GORN340                                                          
         LA    R0,4                                                             
         LA    RE,ACBDADR1         START AT BEGINNING OF ADDRESS LINES          
         LA    RF,EDIRADD1                                                      
GORN330  CLC   0(L'ADRADD1,RF),SPACES DO WE HAVE AN ADDRESS LINE?               
         BNH   *+20                                                             
         MVC   0(L'ADRADD1,RE),0(RF)                                            
         OC    0(L'ADRADD1,RE),SPACES                                           
         AHI   RE,L'ADRADD1                                                     
         AHI   RF,L'ADRADD1                                                     
         BCT   R0,GORN330                                                       
         B     GORN360                                                          
*                                                                               
GORN340  CLI   EDIMAIL,C'3'        FOREIGN ADDRESS?                             
         BE    GORN350             ALWAYS MOVE IN ALL ADDRESS LINES             
         TM    EDIRSTAT,EDIADDRB   DOES THIS FORMAT BREAK UP THE ADDR?          
         BZ    GORN350             NO SO MOVE IN THE WHOLE ADDR THEN            
         ZIC   R1,EDIVADDL         R1 CONTAINS # OF STREET ADDR LINES           
         MHI   R1,L'ADRADD1                                                     
         BCTR  R1,0                                                             
         EXMVC R1,ACBDADR1,SRADDR  JUST MOVE IN THE STREET ADDRESS              
         B     *+10                                                             
GORN350  MVC   ACBDADR1(L'ADRADD1*4),SRADDR     PAYEE ADDR LNS 1,2,3,4          
         OC    ACBDADR1(L'ADRADD1*4),SPACES                                     
*                                                                               
GORN360  MVC   ACBDCTY,EDIRCTY                  PAYEE CITY                      
         OC    ACBDCTY,SPACES                   UPPERCASE                       
         MVC   ACBDST,EDIRST                    PAYEE STATE                     
         OC    ACBDST,SPACES                    UPPERCASE                       
         MVC   ACBDZIP,EDIRZIP                  PAYEE ZIP                       
         MVC   ACBDCTRY,EDIRCTRY                PAYEE COUNTRY CODE              
         MVC   ACBDCTRN(L'EDIRCNME),EDIRCNME    PAYEE COUNTRY NAME              
*                                                                               
         TM    ACBRSTAT,AGYADDRD          EXCLUDE DUPLICATE CITY NAME           
         BZ    GORN369                                                          
                                                                                
         CLC   ACBDCTY,SPACES             COULD BE SPACES DEPENDENT             
         BE    GORN369                    ON MODE                               
                                                                                
         LA    R1,L'ACBDCTY-1             NEED COMPARE LENGTH FOR CITY          
         LA    RF,ACBDCTY+L'ACBDCTY-1     POINT TO LAST CHAR IN CITY            
GORN361  CLI   0(RF),C' '                                                       
         BH    GORN362                                                          
         BCTR  RF,0                                                             
         BCT   R1,GORN361                                                       
                                                                                
GORN362  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ACBDADR2(0),ACBDCTY                                              
         BNE   GORN362A                                                         
         MVC   ACBDADR2(L'ACBDADR2*4),SPACES                                    
         B     GORN369                                                          
                                                                                
GORN362A EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ACBDADR3(0),ACBDCTY                                              
         BNE   GORN362B                                                         
         MVC   ACBDADR3(L'ACBDADR3*3),SPACES                                    
         B     GORN369                                                          
                                                                                
GORN362B EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ACBDADR4(0),ACBDCTY                                              
         BNE   GORN362C                                                         
         MVC   ACBDADR4(L'ACBDADR4*2),SPACES                                    
         B     GORN369                                                          
                                                                                
GORN362C EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ACBDADR5(0),ACBDCTY                                              
         BNE   GORN369                                                          
         MVC   ACBDADR5,SPACES                                                  
         B     GORN369                                                          
                                                                                
GORN369  DS    0H                                                               
         MVC   ACBCHKTY,NUMTYP     CHECK NUMBER TYPE (PREFIX)                   
         GOTO1 ADLN,1              DEADLINE RETURNED IN P                       
         LA    R1,L'P                                                           
         LA    RF,P+L'P-1                                                       
GORN370  CLI   0(RF),X'40'         FIND LAST SPACE                              
         BH    *+16                                                             
         AHI   RF,-1               BACK UP 1 SPOT                               
         BCT   R1,GORN370                                                       
         B     GORN390             NOTHING TO PRINT                             
*                                                                               
         CHI   R1,L'ACBREQDT       CAN IT FIT IN 1ST FIELD                      
         BH    *+14                                                             
         MVC   ACBREQDT,P          YES MOVE IT IN AND FINISH                    
         B     GORN390                                                          
*                                                                               
         CLI   P+L'ACBREQDT-1,X'40' DO WE HAVE A CLEAR BREAK?                   
         BNE   *+20                                                             
         MVC   ACBREQDT,P          YES MOVE IT IN AND FINISH                    
         MVC   ACBREQD2,P+L'ACBREQDT                                            
         B     GORN390                                                          
*                                                                               
         LA    RF,P+L'ACBREQDT-1   POINT TO LAST POSITION                       
         LA    R1,L'ACBREQDT                                                    
GORN380  CLI   0(RF),X'40'         FIND LAST SPACE                              
         BE    *+14                                                             
         AHI   RF,-1                                                            
         BCT   R1,GORN380                                                       
         DC    H'0'                                                             
*                                                                               
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACBREQDT(0),P                                                    
         MVC   ACBREQD2,1(RF)                                                   
*                                                                               
GORN390  MVC   P,SPACES            CLEAR PRINT LINE AFTER ADLN CALL             
*                                  B/C WAS BEING PRINTED IN RUNL                
         ZAP   ACBRECT,RECTOT                                                   
         ZAP   ACBRECTL,RECTOTL                                                 
*DSFTK-137                                                                      
         ZAP   ACBFULC4,HDRTOT                                                  
*DSFTK-137                                                                      
*                                                                               
         AP    TOTRECS,=P'1'                                                    
         ZAP   ACBFULC,TOTRECS                                                  
         GOTO1 VGETFORM,DMCB,(X'02',(R2)),ADCOMFAC,0                            
         PACK  DUB,ACBTSEQ#                                                     
         CVB   R1,DUB                                                           
         STCM  R1,3,TRNSEQ#        RESET SEQUENCE NUMBER                        
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,ACBRCNT        RECORD COUNT FROM GETFORM                    
         BZ    GORUNITX                                                         
         AHI   R0,-1               ALREADY ADDED 1 TO TOTRECS SO -1             
         CVD   R0,DUB                                                           
         AP    TOTRECS,DUB         ADD TOTAL RECS                               
         AHI   R0,1                BUMP BACK TO WRITE RECORDS TO TAPE           
         L     R5,AIO                                                           
         SR    RE,RE                                                            
GORN400  AR    R5,RE                                                            
         STC   R0,BYTE                                                          
         TM    RQSW,RQPQ           PUT TO PQ INSTEAD OF TAPE?                   
         BO    GORUNITX                                                         
*                                                                               
         L     R3,=A(FTROUT)       DEFAULT DCB                                  
         TM    EDIRSTAT,EDIVRLN    VARIABLE RECORD FORMAT?                      
         BZ    *+8                                                              
         L     R3,=A(EDIOUT)                                                    
* DSFTK-150                                                                     
         TM    EDIRSTAT,EDISPCL    SPECIAL ROUTINE?                             
         BZ    *+8                                                              
         L     R3,=A(CHASEIN)                                                   
* DSFTK-150                                                                     
GORN410  DS    0H                                                               
                                                                                
*DSFTK-109                                                                      
         USING FORMD,RF                                                         
         USING FRMTABD,RE                                                       
         CLI   ACBRMODE,HDR         SKIP BLANK HEADER RECORDS                   
         BNE   GORN420                                                          
         L     RF,AFRMBLK                                                       
         LA    RE,FFRMFRM                                                       
         TM    FRMSTAT2,FRMSPHDR    EXCLUDE HEADER IN TAPE OUTPUT               
*MN  THIS REALLY SHOULD NOW BE INSTRUCTION BELOW - FRMSTAT2 IS ONLY             
*MN  SET ONCE PERFORMAT, NOT ONCE PER RECORD DEFINITION - THEREFORE             
*MN  IT NEVER CLEARS FROM THE SETTINGS IT PICKED UP ON THE FIRST                
*MN  RECORD - I HAVE SINCE SET THEM EACH TIME IN FIELD 'THREE'                  
*MN      TM    THREE,FRMSPHDR                                                   
         BO    GORUNITX                                                         
         DROP  RE,RF                                                            
*DSFTK-109                                                                      
*                                                                               
GORN420  DS    0H                                                               
         AP    RECTOT,=P'1'                                                     
         TM    THREE,FRMHTCNT                                                   
         BO    *+10                                                             
         AP    RECTOTL,=P'1'                                                    
         PUT   (R3),(R5)                                                        
         LH    RE,ACBRECLN                                                      
         TM    EDIRSTAT,EDIVRLN    IS THIS A VARIABLE RECORD LENGTH?            
         BNO   *+12                  IF NOT-USE ACBRECLN                        
         ICM   RE,3,0(R5)            ELSE USE FIRST 2 BYTES OF RECORD           
         AHI   RE,1                  BUMP PAST LAST BYTE                        
         SR    R0,R0                                                            
         IC    R0,BYTE             RECORD COUNT                                 
         BCT   R0,GORN400                                                       
*                                                                               
GORUNITX XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* STORAGE                                                             *         
***********************************************************************         
LSTFLDS  DS    0C                                                               
LSTPUBNM DC    CL(L'ACBPAYEE)' '      PUBLICATION NAME                          
LSTCLINM DC    CL(L'ACBCLINM)' '      CLIENT NAME                               
LSTLNQ   EQU   *-LSTFLDS                                                        
                                                                                
INTBCTY  DS    CL30                INTL BANK CITY                               
INTBST   DS    CL2                 INTL BANK STATE                              
INTBZIP  DS    CL10                INTL BANK ZIP                                
INTBCTRY DS    CL3                 INTL BANK COUNTRY                            
INTBCNME DS    CL30                INTL BANK COUNTRY NAME                       
                                                                                
*MN SPEC-47975                                                                  
SVSRTCNT DS    PL3                                                              
SVSRACC  DS    CL15                                                             
*MN SPEC-47975                                                                  
                                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* PROCESS REP VENDOR FIELDS                                         *           
*********************************************************************           
         USING EDIDD,R9                                                         
RPVEND   NTR1  BASE=*,LABEL=*                                                   
         L     R9,AEDIWRK                                                       
         ST    R9,SVWRKREG                                                      
*                                                                               
         CLI   SRTYPE,34                                                        
         BNE   *+16                                                             
         MVC   ACBREPVN,SPACES        CLEAR                                     
         MVC   ACBREPVN(8),SRCNTR+4   STORE STATION CALL LETTERS                
                                                                                
         CLI   SRTYPE,50                                                        
         BNE   *+16                                                             
         MVC   ACBREPVN,SPACES        CLEAR                                     
         MVC   ACBREPVN,SRCNME                                                  
                                                                                
         MVC   ACBREPV3,ACBVACC                                                 
                                                                                
         CLI   SRTYPE,45                                                        
         BNE   *+10                                                             
         MVC   ACBREPV2(3),SRCNTR+3                                             
                                                                                
         CLI   SRTYPE,33              SS,ST                                     
         BNE   *+10                                                             
         MVC   ACBREPV2(12),SRACC+3                                             
                                                                                
         CLI   SRTYPE,34              SS,ST                                     
         BNE   *+10                                                             
         MVC   ACBREPV2(9),SRCNTR+3                                             
                                                                                
         CLI   SRTYPE,35              SS,ST                                     
         BNE   *+10                                                             
         MVC   ACBREPV2(9),SRCNTR+3                                             
                                                                                
         CLI   SRTYPE,49              SP,SQ                                     
         BNE   *+22                                                             
         MVC   ACBREPV2,SRANME                                                  
         MVC   ACBREPV3,SPACES                                                  
         MVC   ACBREPV3(11),SRACC+4                                             
                                                                                
         CLI   SRTYPE,50              SP,SQ                                     
         BNE   *+22                                                             
         MVC   ACBREPV2,SRCNME                                                  
         MVC   ACBREPV3,SPACES                                                  
         MVC   ACBREPV3(8),SRCNTR+1                                             
                                                                                
         CLI   SRTYPE,13              SP,SQ                                     
         BNE   RPVEND20                                                         
         CLI   SRCNTR+3,C' '                                                    
         BNE   RPVEND10                                                         
         MVC   ACBREPV2,SRANME                                                  
         MVC   ACBREPV3(11),SRACC+4                                             
         B     RPVEND20                                                         
                                                                                
RPVEND10 MVC   ACBREPV2,SRCNME                                                  
         MVC   ACBREPV3(9),SRCNTR                                               
                                                                                
RPVEND20 DS    0H                                                               
                                                                                
RPVENDX  XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* PROCESS SPECIAL ROUTINE FOR JPMCHASE CSV FILE FOR PCARD PAYMENTS  *           
*********************************************************************           
         USING EDIDD,R9                                                         
CHSCSV   NTR1  BASE=*,LABEL=*                                                   
         L     R9,AEDIWRK                                                       
         ST    R9,SVWRKREG                                                      
*                                                                               
*                                                                               
* FOR NOW, THIS ALLOCATES A HARD-CODED TEST INPUT FILE DSN. IN                  
* PRODUCTION, THIS MUST ALLOCATE A *TEMPORARY* FILE THAT IS PRODUCED            
* BY MARLENE'S FORMAT TABLE. I.E., THIS ALLOCATES THE INPUT FILE TO             
* ICETOOL.                                                                      
* //CHASEIN  DD DISP=SHR,DON'T WANT-> FREE=CLOSE,DO WANT ->DSN=<INPUT>          
*    ON FIRST PASS - WHEN A STANDARD PROCESS FILE IS TYPICALL OPENED            
*        GOTO1 DYNALLOC,DMCB,(X'80',=CL8'FTROUT'),                              
*              (X'05',=CL44'TEAM.ACC.DEIS.MNAS.AC55.JPMCHSSU')                  
*                                                                               
* FOR NOW, THIS ALLOCATES A HARD-CODED TEST OUTPUT FILE DSN. IN                 
* PRODUCTION, THIS MUST ALLOCATE THE +1 GENERATION OF THE TRUE OUTPUT           
*                                                                               
*        GOTO1 DYNALLOC,DMCB,(C'D',DYDCB),RTDSN   FULLY-QUALIFIED DSN           
*        CLI   DMCB+4,0                                                         
*        JE    *+2                 UNSUCCESSFUL DSN RETRIEVAL !?!               
*                                                                               
* GDG TO BE TRANSMITTED TO CHASE. I.E., THIS ALLOCATES THE ICETOOL              
* OUTPUT FILE.                                                                  
* //CHASEOUT DD UNIT=SYSDA,SPACE=(CYL,(10,10)),FREE=CLOSE,DSN=<OUTPUT>          
* --> WE LET THE A55 ALLOCATE FTROUT AS IT ALWAYS DID AND WILL USE THAT         
* --> INSTEAD OF CHASE OUT - SO ICETOOL CALL WILL NEED TO DO A NAME             
* --> REPLACE : FTROUT INSTEAD OF CHASEOUT                                      
*        GOTO1 DYNALLOC,DMCB,(X'80',=CL8'CHASEOUT'),                            
*              (X'45',=AL3(10,10)),                                             
*              (X'80',=CL44'TEAM.ACC.DEIS.CHASE.OUT')                           
*                                                                               
* ALLOCATE DFSORT AND ICETOOL MESSAGE DATASETS.                                 
*  OVERNIGHT: DD SYSOUT=*                                                       
*  SOON:      DD DUMMY                                                          
*                                                                               
         L     RF,ADMASTC                                                       
         OC    MCREMPQK-MASTD(,RF),MCREMPQK-MASTD(RF)   SOON RUN?               
         BNZ   ICECH10             YES                                          
*                                                                               
         GOTO1 DYNALLOC,DMCB,(X'FD',=CL8'DFSMSG'),(X'80',=CL21' ')              
         GOTO1 DYNALLOC,DMCB,(X'FD',=CL8'TOOLMSG'),(X'80',=CL21' ')             
         GOTO1 DYNALLOC,DMCB,(X'FD',=CL8'SYMNOUT'),(X'80',=CL21' ')             
         B     ICECH20                                                          
*                                                                               
ICECH10  DS    0H                                                               
         GOTO1 DYNALLOC,DMCB,(C'N',=CL8'DFSMSG'),0                              
         GOTO1 DYNALLOC,DMCB,(C'N',=CL8'TOOLMSG'),0                             
         GOTO1 DYNALLOC,DMCB,(C'N',=CL8'SYMNOUT'),0                             
*                                                                               
ICECH20  DS    0H                                                               
* //T1 DD UNIT=SYSDA,SPACE=(CYL,(10,10))     DFSORT TEMP DATASET                
* //T2 DD UNIT=SYSDA,SPACE=(CYL,(10,10))     DFSORT TEMP DATASET                
         GOTO1 DYNALLOC,DMCB,(X'80',=CL8'T1'),(X'40',=AL3(10,10)),0             
         GOTO1 DYNALLOC,DMCB,(X'80',=CL8'T2'),(X'40',=AL3(10,10)),0             
*                                                                               
* THE DSN (AND MEMBER NAMES) IN THESE ALLOCATIONS MUST REFER TO THE             
* TRUE PRODUCTION LOCATION OF THE ICETOOL/DFSORT CONTROL CARDS.                 
* //SYMNAMES DD DISP=SHR,FREE=CLOSE,DSN=<PARMS LOCATION>                        
* //TOOLIN   DD DISP=SHR,FREE=CLOSE,DSN=<PARMS LOCATION>                        
* //COP1CNTL DD DISP=SHR,FREE=CLOSE,DSN=<PARMS LOCATION>                        
* //COP2CNTL DD DISP=SHR,FREE=CLOSE,DSN=<PARMS LOCATION>                        
* //COP3CNTL DD DISP=SHR,FREE=CLOSE,DSN=<PARMS LOCATION>                        
         GOTO1 DYNALLOC,DMCB,(X'FF',=CL8'SYMNAMES'),                   +        
               (X'05',=CL44'DDS.ACC.PARMS(AC55CHSY)')                           
         GOTO1 DYNALLOC,DMCB,(X'FF',=CL8'TOOLIN'),                     +        
               (X'05',=CL44'DDS.ACC.PARMS(AC55CHTI)')                           
         GOTO1 DYNALLOC,DMCB,(X'FF',=CL8'COP1CNTL'),                   +        
               (X'05',=CL44'DDS.ACC.PARMS(AC55CHC1)')                           
         GOTO1 DYNALLOC,DMCB,(X'FF',=CL8'COP2CNTL'),                   +        
               (X'05',=CL44'DDS.ACC.PARMS(AC55CHC2)')                           
         GOTO1 DYNALLOC,DMCB,(X'FF',=CL8'COP3CNTL'),                   +        
               (X'05',=CL44'DDS.ACC.PARMS(AC55CHC3)')                           
*                                                                               
* CALL ICETOOL TO PRODUCE THE CHASE FILE.                                       
         SR    R1,R1               CONTROL CARDS ARE IN TOOLIN                  
         LINK  EP=ICETOOL                                                       
*        LTR   RF,RF          NEED TO UNALLOCATE BEFORE CHECKING                
*        BZ    *+6            RETURN CODE - IF PROGRAMS ABENDS ON A             
*        DC    H'0'           SOON RUN THESE WILL NOT GET UNALLOCATED           
         ST    RF,FULL                                                          
*                                                                               
* DYNAMICALLY UNALLOCATE THE TEMP DATASETS.                                     
         GOTO1 DYNALLOC,DMCB,(C'U',=CL8'T1'),0                                  
         GOTO1 DYNALLOC,DMCB,(C'U',=CL8'T2'),0                                  
         GOTO1 DYNALLOC,DMCB,(C'U',=CL8'CHASEIN'),0                             
*                                                                               
         L     RF,FULL                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     CCSVXIT                                                          
         EJECT                                                                  
*                                                                               
*                                                                               
CCSVXIT  XIT1                                                                   
         DROP  R9                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
*                                                                               
* MAILTAB  - 1-COMPANY 2-EDIMAIL 3-DATA LENGTH 4+-CODE AND THEN SPARE           
*                                                                               
MAILTAB  DS    0C                                                               
*                                         Y&R                                   
         DC    X'4A',CL10'BOA4004',C'5',X'01',C'D'        OVERNIGHT             
         DC    X'4A',CL10'BOA4004',C'0',X'01',C'S'        STANDARD              
         DC    X'4A',CL10'WACH300',C'5',X'05',C'51100'    OV TO AGENCY          
         DC    X'4A',CL10'WACH300',C'0',X'05',C'10100'    STANDARD              
*                                                                               
*                                         SUDLER                                
         DC    X'B2',CL10'WACH300',C'5',X'05',C'51100'    OV TO AGENCY          
         DC    X'B2',CL10'WACH300',C'0',X'05',C'10000'    STANDARD              
* THIS ENTRY MUST REMAIN LAST IN THE TABLE FOR COMPANY CODE B2                  
* THERE ARE SEVERAL WACH300 FORMATS AND WACH300I IS THE EXCEPTION               
* TO THE RULE - IF NOT LAST ALL OTHER WACHOVIA FORMATS WILL                     
* PICK UP THIS EXCEPTION                                                        
         DC    X'B2',CL10'WACH300I',C'5',X'04',C'1000'    OV TO AGENCY          
         DC    X'B2',CL10'WACH300I',C'0',X'04',C'1000'    STANDARD              
*                                                                               
*                                         MINDSHARE                             
         DC    X'50',CL10'BOA4004',C'5',X'01',C'D'        OVERNIGHT             
         DC    X'50',CL10'BOA4004',C'0',X'01',C'S'        STANDARD              
*                                                                               
*SPEC-28147                                                                     
         DC    X'50',CL10'BOA4010',C'1',X'02',C'FM'       CANADIAN              
         DC    X'50',CL10'BOA4010',C'3',X'02',C'FM'       FOREIGN               
         DC    X'50',CL10'BOA4010',C'5',X'02',C'OD'       OVERNIGHT             
*SPEC-44172                                                                     
         DC    X'50',CL10'BOA4010',C'6',X'02',C'RD'       OV TO VENDOR          
*SPEC-44172                                                                     
         DC    X'50',CL10'BOA4010',C'0',X'02',C'US'       STANDARD              
*SPEC-28147                                                                     
*                                                                               
*                                         MCCANN                                
         DC    X'60',CL10'CHS210',C'0',X'05',C'00000'    DEFAULT                
*                                                                               
*                                         OGCK                                  
         DC    X'90',CL10'WACH300',C'1',X'04',C'3000'     CANADIAN              
         DC    X'90',CL10'WACH300',C'2',X'04',C'1000'     US MAIL               
         DC    X'90',CL10'WACH300',C'3',X'04',C'3000'     FOREIGN               
         DC    X'90',CL10'WACH300',C'5',X'04',C'2100'     OV TO AGENCY          
         DC    X'90',CL10'WACH300',C'0',X'04',C'1000'     DEFAULT               
*                                                                               
*                                         MEDIACOM                              
         DC    X'93',CL10'BOA4004',C'5',X'01',C'D'        OVERNIGHT             
         DC    X'93',CL10'BOA4004',C'0',X'01',C'S'        STANDARD              
*                                                                               
*                                                                               
         DC    X'70',CL10'WFG100',C'1',X'03',C'300'      CANADIAN               
         DC    X'70',CL10'WFG100',C'2',X'03',C'100'      US MAIL                
         DC    X'70',CL10'WFG100',C'3',X'03',C'300'      FOREIGN                
         DC    X'70',CL10'WFG100',C'5',X'03',C'400'      OV TO AGENCY           
*                                                                               
*                                                                               
         DC    X'9D',CL10'HS600J',C'5',X'07',C'DAXAXIS'  OV                     
         DC    X'9D',CL10'HS600J',C'0',X'07',C'       '  STD                    
*                                                                               
         DC    X'9D',CL10'HS600P',C'5',X'07',C'DAXAXIS'  OV                     
         DC    X'9D',CL10'HS600P',C'0',X'07',C'       '  STD                    
*                                                                               
         DC    X'87',CL10'STR500A',C'5',X'02',C'10'       OVERNIGHT             
         DC    X'87',CL10'STR500A',C'3',X'02',C'03'       FOREIGN               
         DC    X'87',CL10'STR500A',C'0',X'02',C'00'       STANDARD              
         DC    X'87',CL10'STR500B',C'5',X'02',C'10'       OVERNIGHT             
         DC    X'87',CL10'STR500B',C'3',X'02',C'03'       FOREIGN               
         DC    X'87',CL10'STR500B',C'0',X'02',C'00'       STANDARD              
         DC    X'87',CL10'STR500C',C'5',X'02',C'10'       OVERNIGHT             
         DC    X'87',CL10'STR500C',C'3',X'02',C'03'       FOREIGN               
         DC    X'87',CL10'STR500C',C'0',X'02',C'00'       STANDARD              
         DC    X'87',CL10'STR500D',C'5',X'02',C'10'       OVERNIGHT             
         DC    X'87',CL10'STR500D',C'3',X'02',C'03'       FOREIGN               
         DC    X'87',CL10'STR500D',C'0',X'02',C'00'       STANDARD              
         DC    X'6A',CL10'CHS300A1',C'5',X'04',C'EHQ1'     OVERNIGHT            
         DC    X'6A',CL10'CHS300A1',C'0',X'04',C'V   '     STANDARD             
         DC    X'6A',CL10'CHS300A2',C'5',X'04',C'EHQ1'     OVERNIGHT            
         DC    X'6A',CL10'CHS300A2',C'0',X'04',C'V   '     STANDARD             
         DC    X'6A',CL10'CHS300A3',C'5',X'04',C'EHQ1'     OVERNIGHT            
         DC    X'6A',CL10'CHS300A3',C'0',X'04',C'V   '     STANDARD             
         DC    X'6A',CL10'CHS300A4',C'5',X'04',C'EHQ1'     OVERNIGHT            
         DC    X'6A',CL10'CHS300A4',C'0',X'04',C'V   '     STANDARD             
         DC    X'6A',CL10'CHS300A5',C'5',X'04',C'EHQ1'     OVERNIGHT            
         DC    X'6A',CL10'CHS300A5',C'0',X'04',C'V   '     STANDARD             
         DC    X'6A',CL10'CHS300A6',C'5',X'04',C'EHQ1'     OVERNIGHT            
         DC    X'6A',CL10'CHS300A6',C'0',X'04',C'V   '     STANDARD             
         DC    X'6A',CL10'CHS300A7',C'5',X'04',C'EHQ1'     OVERNIGHT            
         DC    X'6A',CL10'CHS300A7',C'0',X'04',C'V   '     STANDARD             
         DC    X'6A',CL10'CHS300A8',C'5',X'04',C'EHQ1'     OVERNIGHT            
         DC    X'6A',CL10'CHS300A8',C'0',X'04',C'V   '     STANDARD             
         DC    X'00',CL10'XXXXXXX',C'0',X'04',C'1000'   DEFAULT                 
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
EDIWRK   DC    (EDILNQ-EDIDD)C' '                                               
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
IOLNQ    EQU   *-IO                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'**IO2*'                                                      
IO2      DS    2000C                                                            
IO2LNQ   EQU   *-IO2                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'**IO3*'                                                      
IO3      DS    2000C                                                            
IO3LNQ   EQU   *-IO3                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'**IO4*'                                                      
IO4      DS    2000C                                                            
IO4LNQ   EQU   *-IO4                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'**IO5*'                                                      
IO5      DS    2000C                                                            
IO5LNQ   EQU   *-IO5                                                            
IOTTLNQ  EQU   *-IO                                                             
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
         DS    0D                                                               
         DC    CL8'*FRMBLK*'                                                    
FRMBLK   DS    CL(FRTNLNQ)       HOLDS THE FORMAT FOR FILE TRANSFERS            
*                                                                               
         DS    0D                                                               
         DC    CL8'FTROUTB*'     BLOCK FOR ACGETBANK TO OUTPUT A FILE           
FTROUTB  DS    CL(ACBLNQ)        TRANSFER LINE (EFT AND OTHERS)                 
         EJECT                                                                  
***********************************************************************         
* PROGRAM EQUATES AND DSECTS'                                         *         
***********************************************************************         
TOTCHAT  EQU   27                                                               
TOTFIGS  EQU   39                                                               
TOTFIGS2 EQU   43                                                               
*                                                                               
*              SYSTEM EQUATES                                                   
*                                                                               
MEDQ     EQU   X'80'               MEDIA                                        
PRDQ     EQU   X'40'               PRODUCTION                                   
EXPQ     EQU   X'20'               EXPENSE                                      
CEXQ     EQU   X'10'               COKE EXPENDITURE                             
*                                                                               
PTNMX    EQU   3000                MAXIMUM NUMBER OF TRANSACTIONS               
EOF      EQU   X'FF'                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* DSECT FOR LOCAL STORAGE                                             *         
***********************************************************************         
AC55D    DSECT                                                                  
*                                                                               
ATYPES   DS    0A                                                               
ACLIST   DS    V                                                                
DATVAL   DS    V                                                                
GETBROAD DS    V                                                                
NUMTOLET DS    V                                                                
PUBEDIT  DS    V                                                                
RIGHT    DS    V                                                                
DLFLD    DS    V                                                                
PQPROF   DS    V                                                                
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
AEDTRN   DS    A                                                                
AFTRRTN  DS    A                                                                
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
AFRMBLK  DS    A                   A(FRMBLK) HOLDS EFT FORMAT                   
AFTROUTB DS    A                   A(FTROUTB) GETFORM BLOCK                     
AEDIWRK  DS    A                   A(EDIWRK) EDI WORK BLOCK                     
APDRFT   DS    A                   A(PDRFT) DRAFT CHECK MASK                    
*                                                                               
VGETBANK DS    V                   A(GETBANK)                                   
VGETFORM DS    V                   A(GETFORM)                                   
*                                                                               
MAINLEN  DS    F                   LENGTH OF GETMAIN AREA                       
MAINBGN  DS    A                   A(START OF GET MAIN)                         
RECNXT   DS    F                   NEXT REC DATA AREA (FOR DATA TR)             
RECLEN   DS    H                   LENGTH OF RECORD FIELD FOR DATASET           
SVWRKREG DS    A                   A(CURRENT WORKING STORAGE)                   
*                                                                               
VMQRPT   DS    A                   A(MQ FILE INTERFACE)                         
ADFORM   DS    A                   ADDRESS VALIDATION ROUTINE                   
*                                                                               
TRNSEQ#  DS    XL2                 TRANSACTION SEQ #                            
*                                                                               
LUID     DS    XL8                 LUID                                         
PQID     DS    XL7                 PQID                                         
WKID     DS    XL(L'WKINDEX)       WORKER FILE ID - CHECK FILE                  
WKIDP    DS    XL(L'WKINDEX)       WORKER FILE ID - POSTING FILE                
*                                                                               
UPSI     DS    CL1                 UPSI SWITCH                                  
NOMPQ    EQU   X'80'               NO MPQ MESSAGE                               
*                                                                               
MMOSSTR  DS    XL3                 MEDIA MONTH OF SVC START FROM REQ            
MMOSEND  DS    XL3                 MEDIA MONTH OF SVC END FROM REQ              
LOCMMOS  DS    XL3                 MEDIA MONTH OF SVC FROM TRANSACTION          
SVEST    DS    CL6                 SAVED ESTIMATE                               
SVOFF    DS    CL2                 SAVED OFFICE                                 
TYPCHK   DS    CL1                 TYPE OF CHECK RUN                            
TYPDDS   EQU   C'C'                DDS OVERNIGHT                                
TYPSOON  EQU   C'S'                SOON                                         
TYPLOCL  EQU   C'L'                LOCAL CHECKS                                 
VTYPE    DS    XL1                 TYPE OF VENDOR                               
VREG     EQU   C'1'                REGULAR VENDOR (NON EFT)                     
VEFT     EQU   C'2'                EFT VENDOR                                   
* DSFTK-150                                                                     
VPCARD   EQU   C'3'                PCARD VENDOR                                 
* DSFTK-150                                                                     
VCCARD   EQU   C'4'                CCARD VENDOR                                 
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
RUNUAT   EQU   X'20'               RUNNING ON UAT FILE                          
*                                                                               
RQSW     DS    XL1                 REQUEST CONTROL                              
RQURG    EQU   X'80'               URGENT                                       
RQAPP    EQU   X'40'               APPROVED                                     
RQCRD    EQU   X'20'               CREDITS                                      
RQACT    EQU   X'10'               ACTIVITY                                     
RQCSD    EQU   X'08'               CASH DISCOUNT ONLY                           
RQPQ     EQU   X'04'               PUT TO PQ INSTEAD OF DATASET/TAPE            
*                                  FOR EDI FLAT FILES                           
LGSW     DS    XL1                 LEDGER CONTROL                               
APPRV    EQU   X'80'               USE APPROVAL SYSTEM                          
CANAD    EQU   X'40'               CANADIAN LEDGER                              
UNAUTH   EQU   X'08'               UNAUTHORIZED USER                            
*                                                                               
ACCSW    DS    XL1                 GETBANK ACCOUNT CONTROL                      
EFTON    EQU   X'80'               EFT BANK SETUP PRESENT                       
EDION    EQU   X'40'               820 BANK SETUP PRESENT                       
*                                                                               
* DSFTK-150                                                                     
ACCSW1   DS    XL1                 VENDORS PROCESSED IN THIS RUN                
EDIINCL  EQU   X'80'               EDI VENDORS INCLUDED                         
EFTINCL  EQU   X'40'               EFT VENDORS INCLUDED                         
PCRDINCL EQU   X'20'               PCARD VENDORS INCLUDED                       
CCRDINCL EQU   X'10'               CCARD VENDORS INCLUDED                       
*                                                                               
* DSFTK-150                                                                     
PRNTSW   DS    XL1                 PRINT CONTROL                                
NUMBER   EQU   X'80'               PRINT NUMBER ON CHECK                        
SHUTTLE  EQU   X'40'               SHUTTLE                                      
LASER    EQU   X'20'               LASER                                        
WEBWSP   EQU   X'10'               LASERWSP - BLT WEBSERIES CHECKS              
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
DTRSW    DS    XL1                 DATA TRANSFER SWITCH                         
DTRANY   EQU   X'80'               ANY KIND OF DATA XFER (EDI,DATA FIL)         
DTRINT   EQU   X'40'               FILE INITIALIZE                              
DTR820   EQU   X'20'               FILE SENT X12-820 STANDARD FORMAT            
DTRFLAT  EQU   X'10'               FLAT FILE (DDS CHCKS DATA FILE)              
DTREFT   EQU   X'08'               EFT                                          
DTRNEFT  EQU   X'04'               NON-EFT                                      
DTRMQ    EQU   X'02'               WRITE OUT MQ REQ INSTEAD OF EDICT            
* DSFTK-150                                                                     
DTRPCARD EQU   X'01'               VENDOR IS PCARD VENDOR                       
* DSFTK-150                                                                     
* IF BOTH THE 08 AND 04 BITS ARE ON THAN IT'S A SPLIT FILE CHECK RUN            
*                                                                               
DTXSW    DS    XL1                 DATA FILE SWITCH                             
FTRREAD  EQU   X'80'               FTR BANK/FORMAT RECORDS READ                 
FILOPN   EQU   X'40'               DATA FILE OPEN                               
EDIFAIL  EQU   X'20'               EFT/EDI820 CANNOT PROCESS-BANK INFO          
FFLOPN   EQU   X'10'               DLCB OPEN (FOR FLATFILE)                     
*                                  MISSING                                      
*                                                                               
*MN SPEC-47579                                                                  
SVJOBNO  DS    CL5                 JOB# FOR UNIQUE# ON MQ/RTGEN SUB             
*MN SPEC-47579                                                                  
SYENT    DS    0CL17               SYSTEM ENTRY                                 
SYLDG    DS    CL1                 LEDGER CODE                                  
SYEQU    DS    CL1                 EQUATE                                       
SYNME    DS    CL11                NAME                                         
SYTYP    DS    AL3                 TYPE TABLE                                   
SYFMT    DS    CL1                 FORMAT OPTION                                
*                                                                               
PKCNT4   DS    PL4                 TRANSACTION/CHECK COUNTER                    
PKCNT5   DS    PL4                 CHECKS ONLY COUNTER                          
BATCNT   DS    PL4                 BATCH COUNT (INDIVIDUAL BATCH)               
TOTBCNT  DS    PL4                 BATCH COUNT (ALL BATCHES)                    
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
TOTTRNS  DS    PL6                 TOTAL TRANACTIONS                            
TOTRECS  DS    PL6                 TOTAL RECORDS (FOR EDI BANK SPEC)            
TOTRECFF DS    PL6                 TOTAL RECORDS (FOR FLATFILE)                 
TOTCASH  DS    PL6                 TOTAL DOLLARS (FOR EDI BANK SPEC)            
TCASHFF  DS    PL6                 TOTAL DOLLARS (FOR FLATFILE)                 
TOTRMR   DS    PL6                 TOTAL RMR RECS (BOA BANK SPEC)               
EFTTOT   DS    PL6                 TOTAL EFT DOLLARS                            
EFTCK#   DS    PL6                 TOTAL # OF EFT CHECKS                        
RECTOT   DS    PL6                 FILE TOTAL                                   
RECTOTL  DS    PL6                 FILE TOTAL MINUS HEADER/TRAILER              
*DSFTK-137                                                                      
HDRTOT   DS    PL6                 TOTAL HEADER RECORDS                         
*DSFTK-137                                                                      
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
BATAMT   DS    PL6                 TOTAL CASH FOR A BATCH                       
TOTBAMT  DS    PL6                 TOTAL CASH FOR ALL BATCHES                   
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
TXT8     DS    CL(ETXT9-ETXT8)     TOTALS FOR CHECK/CHEQUE                      
TXT9     DS    CL(ETXT10-ETXT9)    TOTALS FOR EFT                               
TXT10    DS    CL(ETXT11-ETXT10)   TOTALS EFT CHECKS/CHEQUES                    
TXT11    DS    CL(ETXT12-ETXT11)   TOTALS FOR PRINTED                           
TXT12    DS    CL(ETXTX-ETXT12)    TOTAL PRINTED CHECKS/CHEQUES                 
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
SVMEDIA  DS    CL1                                                              
*                                                                               
ASBRTN   DS    A                   A(SUB-ROUTINE FOR THIS ACCOUNT)              
ANXTRN   DS    A                   A(NEXT TRANSACTION IN TABLE)                 
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
PRDLST   DS    CL6                 PRODUCT LIST CODE FILTER                     
*                                                                               
MINCHK   DS    PL6                 MINIMUM CHECK                                
MAXCHK   DS    PL6                 MAXIMUN CHECK                                
*                                                                               
ACLIPRO  DS    A                   A(CLIENT PROFILE)                            
CNTFK    DS    CL25                CTFILE KEY                                   
OFFLIST  DS    CL25                                                             
SVBANK   DS    CL15                BANK ACC C/U/L/ACC FROM CHECK REC            
THISBRD  DS    CL3                 FIRST DAY OF THIS BROADCAST MONTH            
LASTBRD  DS    CL3                 FIRST DAY OF LAST BROADCAST MONTH            
NEXTBRD  DS    CL3                 LAST DAY OF NEXT BROADCAST MONTH             
DATECK   DS    CL1                                                              
CHKDATE  DS    CL3                                                              
PRCKDTE  DS    CL3                 PREVIOUS CHECK DATE                          
IDABBR   DS    CL7                 ORIGIN ID                                    
POWCODE  DS    CL2                 POWER CODE                                   
LASRCODE DS    CL2                 LASER CODE FROM CONTROL FILE                 
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
SVSTA    DS    CL5                                                              
CLINO    DS    CL3                                                              
DUBX     DS    D                                                                
*                                                                               
TEST5    DS    XL1                 GETBANK A/B/C                                
TESTLVL  EQU   X'03'               PHASE TEST LEVEL (BLANK/A/B/C)               
TRLPRCD  EQU   X'80'               PROCESSED THE TRL MODE                       
TRL2PRCD EQU   X'40'               PROCESSED THE TRL2 MODE                      
*                                                                               
ELCODE   DS    XL1                                                              
PUBNO    DS    CL12                                                             
CLISAVE  DS    CL12                                                             
CLIPRJB  DS    CL39                                                             
LASTPRD  DS    CL6                                                              
LASTJOB  DS    CL6                                                              
INVNO    DS    CL6                                                              
THISACC  DS    CL(TRNKCULC+5-TRNRECD)                                           
LASTACC  DS    CL(TRNKCULC+5-TRNRECD)                                           
RECWRK   DS    CL100               WORK AREA FOR TABLE RECORD                   
CLSAVE   DS    (L'RECLI)C          SAVE FOR CLIENT CHANGE TOTALS                
DIR      DS    CL8                                                              
ACDCB    DS    A                   A(DCB)                                       
DLCB     DS    XL(DLCBXLX)         FOR DOWNLOADING                              
STKKEY   DS    XL42                STACK KEY                                    
TEMPKEY  DS    XL42                TEMPORARY KEY                                
*                                                                               
SREC     DS    0CL(SRLNQ)          SORT RECORD                                  
SRVAR    DS    CL50                VARIABLE SORT FIELDS                         
SRVTYP   DS    CL1                 VENDOR TYPE (EFT OR NON-EFT)                 
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
SRPLINV  DS    CL20                PRODUCTION LONG INVOICE NUMBER               
*                                                                               
SRADV    DS    XL2                 MONTH OF ADVERTISING (MEDIA MOS)             
SRROFC   DS    CL2                 REQUEST OFFICE                               
SRSTAT   DS    XL1                 REQUEST STATUS BYTE                          
SRSURG   EQU   X'80'               REQUEST FOR URGENT                           
SRSZRO   EQU   X'40'               ZERO REQUEST                                 
SRMAIL   EQU   X'20'               SPECIAL MAIL OPTION FOR 820 TRANS            
*SPEC-44172                                                                     
SRMAIL2  EQU   X'10'               OV DIRECT TO VENDOR SPEC-44172               
*SPEC-44172                                                                     
SROPT    DS    XL1                 OPTIONS                                      
SROWRD   EQU   X'80'               PRINT AMOUNT IN WORDS                        
SRMNTH   DS    CL2                 MONTH OF ACTIVITY                            
SRCDTE   DS    XL3                 CHECK DATE                                   
SRCAMT   DS    PL6                 CHECK AMOUNT                                 
SRTRNCT  DS    PL3                 TRANS PER CHECK COUNT SPEC-40627             
SRCCD    DS    PL6                 CHECK CASH DISCOUNT                          
SRTYPE   DS    XL1                 TRANSACTION TYPE                             
SR#INV   DS    XL2                 # OF INVOICES                                
SRSYENT  DS    CL(L'SYENT)         SYSTEM ENTRY                                 
SRTYENT  DS    CL(L'TYENT)         TYPE ENTRY                                   
SRTMMOS  DS    XL3                 MEDIA MONTH OF SERVICE                       
SRTRURF  DS    CL6                 TRUE REFERENCE                               
* DSFTK-150                                                                     
SRTSWIPE DS    XL1                 PCARD SWIPE TYPE                             
* DSFTK-150                                                                     
SRTSWIFT DS    CL11                SWIFT CODE                                   
SRTINTB# DS    CL35                INTERNATIONAL BANK ACCT NUMBER               
SRTINTNM DS    CL35                INTERNATIONAL BANK NAME                      
SRTINTAD DS    0CL35               IB BANK ADDRESS LINE 1,2,3                   
SRTINTA1 DS    CL35                IB BANK ADDRESS LINE 1                       
SRTINTA2 DS    CL35                IB BANK ADDRESS LINE 2                       
SRTINTA3 DS    CL35                IB BANK ADDRESS LINE 3                       
*SPEC-35871                                                                     
SRTCURR  DS    CL3                 CURRENCY CODE FROM CLEARANCES                
*SPEC-35871                                                                     
SRLNQ    EQU   *-SRVAR                                                          
         EJECT                                                                  
***********************************************************************         
* DATA DICTIONARY ITEMS                                               *         
***********************************************************************         
DICO     DS    0C                                                               
         DSDDL PRINT=YES                                                        
         DS    XL1                                                              
         EJECT                                                                  
***********************************************************************         
* WORK AREAS                                                          *         
***********************************************************************         
NARRWRK  DS    CL250                                                            
SVNARRWK DS    CL(L'NARRWRK)                                                    
*                                                                               
PBLK     DS    5CL132                                                           
*                                                                               
*                                                                               
AC55DX   EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* DSECT FOR A PAYABLE TRANSACTION TABLE ENTRY                         *         
***********************************************************************         
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
PTNMMOS  DS    XL3                 MEDIA MONTH OF SERVICE                       
PTNEST   DS    CL6                 ESTIMATE                                     
PTNTYP   DS    XL1                 TRANSACTION TYPE                             
PTNLQ    EQU   *-PTNKEY                                                         
         EJECT                                                                  
***********************************************************************         
* DSECT FOR BINSRCH PARAMETERS                                        *         
***********************************************************************         
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT INTO RECORD                     
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAX NUMBER IN TABLE                          
BINTABLE DS    A                                                                
                                                                                
***********************************************************************         
* DSECT FOR ADDRESS BLOCK                                             *         
***********************************************************************         
ADDBLKD  DSECT                                                                  
ADDBADD1 DS    CL(L'ADRADD1)      ADDRESS 1                                     
ADDBADD2 DS    CL(L'ADRADD1)      ADDRESS 2                                     
ADDBADD3 DS    CL(L'ADRADD1)      ADDRESS 3                                     
ADDBADD4 DS    CL(L'ADRADD1)      ADDRESS 4                                     
ADDBCTY  DS    CL30               CITY/TOWN                                     
ADDBST   DS    CL2                STATE/PROVINCE                                
ADDBZIP  DS    CL10               POSTAL CODE                                   
ADDBCTRY DS    CL3                COUNTRY                                       
ADDBCNME DS    CL30               COUNTRY NAME                                  
ADDBLNQ  EQU   *-ADDBLKD                                                        
                                                                                
***********************************************************************         
* DSECT FOR REMITTANCE LINE TABLE                                     *         
***********************************************************************         
REMITD   DSECT                                                                  
RECLI    DS    CL20                CLIENT                                       
REEST    DS    CL2                 ESTIMATE                                     
REMOS    DS    CL11                MONTH OF SERVICE                             
REINV    DS    CL10                INVOICE NUMBER                               
REKEY    EQU   *-REMITD                                                         
REAMT    DS    PL6                 AMOUNT                                       
RENUM    EQU   (*-REAMT)/(L'REAMT)                                              
RELEN    EQU   *-REMITD                                                         
                                                                                
***********************************************************************         
* DSECT FOR DEFAULT VS OVERRIDE TABLE                                 *         
***********************************************************************         
EDITABD  DSECT                                                                  
EDIFDSP  DS    XL2                 EDI FIELD DISPLACEMENT                       
EDIFLN   DS    XL2                 EDI FIELD LENGTH                             
EDIDDSP  DS    XL2                 EDI FIELD DISPLACEMENT                       
EDISTAT  DS    XL1                 EDI FIELD STATUS                             
EDISZERO EQU   X'80'                 - COMPARE TO ZERO                          
EDITABLN EQU   *-EDITABD                                                        
                                                                                
***********************************************************************         
* DSECT FOR PROFILE RECORDS                                           *         
***********************************************************************         
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
                                                                                
***********************************************************************         
* DSECT FOR A DETAIL TABLE ENTRY                                      *         
***********************************************************************         
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
                                                                                
***********************************************************************         
* EDI 820 AGENCY TRANSMISSION DATA TABLE                                        
***********************************************************************         
E8ATABD  DSECT                                                                  
E8AONUM  DS    AL2                                                              
E8ALEDS  DS    AL1                                                              
E8ALOGO  DS    CL4                 LOGO CODE                                    
E8ARACO  DS    CL4                 RETURN ADDRESS CODE                          
E8ASIGCO DS    CL4                 SIGNATURE CODE                               
E8ATBLNQ EQU   *-E8ATABD                                                        
*                                                                               
***********************************************************************         
* EDI 820 DATA TRANSMISSION WORK AREA                                 *         
***********************************************************************         
*                                  820 RECORD DATA                              
E820D    DSECT                                                                  
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
E8ADR3   DS    CL(L'ADRADD1)       ADDRESS LINE 3                               
E8ADR4   DS    CL(L'ADRADD1)       ADDRESS LINE 4                               
E8CITY   DS    CL30                CITY                                         
E8STATE  DS    CL2                 STATE                                        
E8ZIP    DS    CL9                 ZIP OR POSTAL CODE                           
E8CTRY   DS    CL3                 COUNTRY CODE                                 
E8FORF   DS    CL1                 FOREIGN COUNTRY CODE FLAG                    
E8CSZQ   EQU   *-E8CITY            LENGTH OF CITY,STATE,ZIP,ETC FIELDS          
E8ADRQ   EQU   *-E8ADR1            LENGTH OF ADDRESS FIELDS                     
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
E8LOGO   DS    CL4                 LOGO CODE                                    
E8RACO   DS    CL3                 RETURN ADDRESS CODE                          
E8SIGCO  DS    CL4                 SIGNATURE CODE                               
E8NARR   DS    0CL200              NARRATIVE                                    
E8NARR1  DS    CL80                NARRATIVE LINE 1                             
E8NARR2  DS    CL80                NARRATIVE LINE 2                             
E8NARR3  DS    CL40                NARRATIVE LINE 3                             
E820DX   EQU   *                                                                
         EJECT                                                                  
*                                                                               
***********************************************************************         
* DSECT FOR AN EDI 820 OUTPUT RECORD HEADER                                     
***********************************************************************         
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
***********************************************************************         
* EFT DATA SET NAME DSECT                                             *         
* 'ACCTAPE.EFT.AA.UUUUUUUU.LL                                         *         
* WHERE AA=ALPHA CODE, U'S=USER ID, LL=LEDGER                         *         
***********************************************************************         
DSNMD    DSECT                                                                  
DSNACC   DS    CL8                 ACCTAPE.                                     
DSNVAR   DS    0CL18                                                            
DSNTRN   DS    CL4                 EFT.                                         
         DS    C                   A - ALPHA PREFIX                             
DSNALPH  DS    CL2                 2 CHAR ALPHA CODE                            
DSND1    DS    CL1                 .                                            
DSNVAR2  DS    0CL10                                                            
DSNUSID  DS    CL8                 8 CHAR USER ID                               
DSND2    DS    CL1                 .                                            
DSNLDG   DS    CL1                 LEDGER                                       
         ORG   DSNACC                                                           
DSNDISK  DS    CL9                 DISK NAME - SFTPDISK.                        
DSNDENV  DS    CL5                 ENVIRONMENT - PROD./TEST.                    
DSNDMQ   DS    0C                  START OF MQ FILENAME                         
         DS    C                                                                
DSNDTRNT DS    CL4                 TRANSMISSION TYPE - 820./EFT.                
DSNDVAR  DS    0CL10               VARIABLE FROM EDIDSN                         
DSNDUSID DS    CL8                 8 CHAR USER ID                               
DSNDD2   DS    CL1                 .                                            
DSNDLDG  DS    CL1                 LEDGER                                       
DSNDD3   DS    CL1                 .                                            
DSNDDPRF DS    CL1                 D - DATE PREFIX                              
DSNDDTE  DS    CL6                 DATE                                         
DSNDD4   DS    CL1                 .                                            
DSNDTPRF DS    CL1                 T - TIME PREFIX                              
DSNDTME  DS    CL7                 TIME                                         
DSNDVLNQ EQU   *-DSNDMQ            LENGTH VARIABLE + DATE/TIME                  
DSNDSLNQ EQU   *-DSNDMQ            LENGTH W/O DSN CONSTANTS                     
DSNSLNQ  EQU   *-DSNVAR            LENGTH W/O DSNACC                            
DSNLNQ   EQU   *-DSNMD                                                          
         EJECT                                                                  
***********************************************************************         
* ELECTRONIC DATA TRANMISSION AGENCY TABLE                            *         
***********************************************************************         
EDTTABD  DSECT                                                                  
EDTTAGY  DS    CL2                 AGENCY ALPHA                                 
EDTTRN   DS    AL3                 DATA TRANMISSION ROUTINE                     
EDTTLNQ  EQU   *-EDTTABD                                                        
         EJECT                                                                  
***********************************************************************         
* EDI - DSECT FOR EDI820 (FOR O&M-WACHOVIA), EFT AND FLAT FILES       *         
***********************************************************************         
EDIDD    DSECT                                                                  
EDIFDTE  DS    CL8                FILE CREATION DATE                            
EDICHKN  DS    CL8                CHECK NUMBER                                  
EDICDTE  DS    CL8                CHECK DATE                                    
EDITIMEP DS    PL8                TIME                                          
EDICAMT  DS    CL12               CHECK AMOUNT                                  
EDICAMTW DS    CL150              CHECK AMOUNT IN WORDS                         
EDIVCDE  DS    CL14               TRUE VENDOR CODE                              
EDIVNME  DS    CL36               TRUE VENDOR NAME                              
EDIPACC  DS    CL14               PAYEE CODE                                    
EDIPNME  DS    CL36               PAYEE NAME                                    
EDIINVN  DS    CL20               INVOICE NUMBER                                
EDINARR  DS    CL200              NARRATIVE                                     
EDITGAMT DS    CL12               TRUE GROSS AMOUNT (MEDIA CHKS ONLY)           
EDIGAMT  DS    CL12               NET+C/D (FOR ALL CHECKS)                      
EDIDAMT  DS    CL11               DISCOUNT AMOUNT                               
EDINAMT  DS    CL12               NET AMOUNT                                    
EDIGST   DS    CL11               GST AMOUNT                                    
EDIPST   DS    CL11               PST AMOUNT                                    
EDIRECS  DS    CL15               TOTAL RECORDS                                 
EDITOTD  DS    CL12               TOTAL DOLLARS                                 
EDITCHK  DS    CL15               TOTAL CHECK COUNT                             
EDIALPH  DS    CL2                AGENCY ALPHA                                  
EDICNM   DS    CL36               COMPANY NAME                                  
EDIUSR   DS    CL7                REQUEST USER ID                               
EDIORGN  DS    CL33               IDI ORIGIN NAME (REQUESTOR)                   
EDILDG   DS    CL2                LEDGER CODE                                   
EDILDGN  DS    CL36               LEDGER NAME                                   
EDICURR  DS    CL1                CURRENCY 1=USD, 2=CAD                         
EDIREQ#  DS    CL3                REQUEST NUMBER                                
EDIMEDC  DS    CL1                MEDIA CODE                                    
EDIINVD  DS    CL10               INVOICE DATE                                  
EDICAC   DS    CL14               SOURCE CODE (CONTRA ACCOUNT)                  
EDICACN  DS    CL36               SOURCE CODE NAME (C/A NAME)                   
EDICLT   DS    CL3                CLIENT CODE                                   
EDICLTN  DS    CL36               CLIENT NAME                                   
EDIPRD   DS    CL3                PRODUCT CODE                                  
EDIPRDN  DS    CL36               PRODUCT NAME                                  
EDIJOB   DS    CL6                JOB/ESTIMATE CODE                             
EDIJOBN  DS    CL36               JOB/ESTIMATE NAME                             
EDIMOS   DS    CL6                ADVERTISING MONTH OR MOS MMM/YYYY             
EDIPERD  DS    CL17               PERIOD DATE (MEDIA)                           
EDIOFFC  DS    CL2                OFFICE CODE                                   
EDIOFFN  DS    CL36               OFFICE NAME                                   
EDIMOA   DS    CL6                MOA MMM/YY                                    
*                                                                               
EDIBNKC  DS    CL10               BANK AND BRANCH CODE FROM SC                  
EDIBKNME DS    CL36               BANK NAME                                     
EDIDROUT DS    CL9                DESTINATION ROUTING # FROM VENDOR             
EDIDBNKA DS    CL20               DESTINATION BANK ACCT # FROM VENDOR           
EDISROUT DS    CL9                SOURCE ROUTING NUMBER FROM SC                 
EDISBNKA DS    CL20               SOURCE BANK ACCOUNT FROM SC                   
EDIREMDL DS    XL2                REMITT DELIVERY SETTING BASED ON SPEC         
EDIREMDC DS    CL1                F=FAX E=EMAIL                                 
*                                                                               
* RECORD SETUP RETURNED FROM GETFORM                                            
*                                                                               
EDIRECLN DS    XL2                RECORD LENGTH                                 
EDIBLKSZ DS    XL2                BBLOCK SIZE                                   
EDIRSTAT DS    XL1                RECORD STATUS (MATCHES FFRMRST)               
EDIVRLN  EQU   X'80'              VARIABLE LENGTH RECORD (FFRMVRL)              
EDIADDRB EQU   X'40'              FORMAT BREAKS UP ADDRESS (FFRADDRB)           
EDICANF  EQU   X'20'              CANADA ADDR IS FOREIGN (FFRCANF)              
EDINASK  EQU   X'10'              STRIP OUT ASTERISKS                           
EDIADDRD EQU   X'08'              REMOVE DUPLICATE CITY IN ADDRESS              
EDICSV   EQU   X'04'              CSV FILE : STRIP COMMAS FROM TEXT             
* DSFTK-150                                                                     
EDISPCL  EQU   X'02'              SPECIAL ROUTINE FOR THIS FORMAT               
* DSFTK-150                                                                     
EDICKDLV DS    CL1                CHECK DELIVERY FLAG-QOPT8 SPEC-40627          
*                                                                               
EDIFORMT DS    CL10               FORMAT KEY                                    
EDIDSN   DS    CL8                DATASET NAME                                  
EDITRNKY DS    CL7                TRANSMISSION KEY                              
EDIADVID DS    CL7                ADVANTIS ID                                   
EDIADVAC DS    CL4                ADVANTIS ACCOUNT NUMBER                       
EDIMSGCL DS    CL8                MESSAGE CLASS                                 
EDICHRG  DS    CL1                CHARGE                                        
EDITRNTY DS    CL1                TRANSMISSION TYPE                             
EDITEDT  EQU   BKTEDT               EDICT TYPE TRANSMISSION                     
EDITMQ   EQU   BKTMQ                MQ TYPE TRANSMISSION                        
EDITRNMD DS    CL1                TRANSMISSION MOD (0,1)                        
EDICADDL DS    CL1                # OF STREET ADDRESS LINES (COMPANY)           
EDIVADDL DS    CL1                # OF STREET ADDRESS LINES (VENDOR)            
EDIRADD1 DS    CL(L'ADRADD1)      RECIPIENT (PAYEE) ADDRESS 1                   
EDIRADD2 DS    CL(L'ADRADD1)      RECIPIENT (PAYEE) ADDRESS 2                   
EDIRADD3 DS    CL(L'ADRADD1)      RECIPIENT (PAYEE) ADDRESS 3                   
EDIRADD4 DS    CL(L'ADRADD1)      RECIPIENT (PAYEE) ADDRESS 4                   
EDIRCTY  DS    CL30               RECIPIENT (PAYEE) CITY/TOWN                   
EDIRST   DS    CL2                RECIPIENT (PAYEE) STATE/PROVINCE              
EDIRZIP  DS    CL10               RECIPIENT (PAYEE) POSTAL CODE                 
EDIRCTRY DS    CL3                RECIPIENT (PAYEE) COUNTRY                     
EDIRCNME DS    CL30               RECIPIENT (PAYEE) COUNTRY NAME                
EDIMAIL  DS    CL1                MAIL CODE (1=CA, 2=US, 3=FOREIGN,             
*                                  4=ERROR)                                     
EDICADD1 DS    CL(L'ADRADD1)      COMPANY (PAYER) ADDRESS 1                     
EDICADD2 DS    CL(L'ADRADD1)      COMPANY (PAYER) ADDRESS 2                     
EDICADD3 DS    CL(L'ADRADD1)      COMPANY (PAYER) ADDRESS 3                     
EDICADD4 DS    CL(L'ADRADD1)      COMPANY (PAYER) ADDRESS 4                     
EDICCTY  DS    CL30               COMPANY (PAYER) CITY/TOWN                     
EDICST   DS    CL2                COMPANY (PAYER) STATE/PROVINCE                
EDICZIP  DS    CL10               COMPANY (PAYER) POSTAL CODE                   
EDICCTRY DS    CL3                COMPANY (PAYER) COUNTRY                       
EDICCNME DS    CL30               COMPANY (PAYER) COUNTRY NAME                  
EDIBSTY  DS    0CL12                                                            
EDILOGO  DS    CL4                LOGO CODE                                     
EDIRADC  DS    CL4                RETURN ADDR CODE                              
EDISIG   DS    CL4                SIGNATURE CODE                                
EDISPACE DS    CL1                ONE BYTE SPACE FIELD WHICH IS USED            
*                                 AS PLACEHOLDERS FOR THE ADDR FIELDS           
EDIFLAG  DS    XL1                                                              
EDIACCF  EQU   X'80'              FIRST TIME FOR ACCOUNT                        
EDIFNID  EQU   X'40'              USING ORIGIN ID FOR FILENAME DEF              
EDIFNNM  EQU   X'20'              USING ORIGIN NAME FOR FILENAME DEF            
*                                                                               
EDIVEN   DS    0C                 VENDOR RECORD INFORMATION (MAX 132)           
EDICNTN  DS    CL36               CONTACT NAME                                  
EDIFAX   DS    CL15               FAX NUMBER                                    
EDIEMAIL DS    CL80               EMAIL                                         
EDIBLK1  DS    CL36               STATION/REP/PUB INFORMATION                   
EDIBLK2  DS    CL60               CLIENT & PRODUCT NAME AND ESTIMATE            
EDIBLKS  DS    CL36               INFORMATION SAVE FIELD                        
EDICPHN  DS    CL11               VENDOR TELEPHONE NUMBER                       
EDIVENLN EQU   *-EDIVEN                                                         
*                                                                               
EDIDIDN  DS    CL33               IDI DESTINATION NAME (REQUESTOR)              
*                                                                               
EDIIADDR DS    CL(L'ACBIADR)      IDI ORIGIN ADDRESS 1                          
*                                                                               
EDIIDAD1 DS    CL(L'ACBIDAD1)     IDI DESTINATION ADDRESS 1                     
EDIIDAD2 DS    CL(L'ACBIDAD2)     IDI DESTINATION ADDRESS 2                     
EDIICTY  DS    CL30               IDI DESTINATION CITY/TOWN                     
EDIIST   DS    CL2                IDI DESTINATION STATE/PROVINCE                
EDIIZIP  DS    CL10               IDI DESTINATION POSTAL CODE                   
EDIICTRY DS    CL3                IDI DESTINATION COUNTRY                       
EDIICNME DS    CL30               IDI DESTINATION COUNTRY NAME                  
*                                                                               
EDIENTH  DS    PL6                ENTRY HASH                                    
EDIBLCNT DS    PL4                BLOCK COUNT                                   
* DSFTK-150                                                                     
EDIPTYPE DS    CL5                PCARD SWIPE TYPE                              
* ON THE BANK RECORD THE ADVANTIS ID FIELD IS NO LONGER USED FOR                
* ADVANTIS TRANSMISSION SO RE-PURPOSING TO INDICATE THIS BANK SET               
* IS PCARD BANK RECORD IF SET TO PCARD                                          
EDIEFADV DS    CL7                ON BANK RECORD ADVANTIS ID FIELD              
* DSFTK-150                                                                     
*MN SPEC-46328                                                                  
EDIFFTX1 DS    CL(L'FFTXSCF1)     FREEFORM TEXT FIELD 1 FROM SC ACC             
EDIFFTX2 DS    CL(L'FFTXSCF2)     FREEFORM TEXT FIELD 2 FROM SC ACC             
EDIFFTX3 DS    CL(L'FFTXSCF3)     FREEFORM TEXT FIELD 3 FROM SC ACC             
EDIFFTX4 DS    CL(L'FFTXSCF4)     FREEFORM TEXT FIELD 4 FROM SC ACC             
EDIFFTX5 DS    CL(L'FFTXSCF5)     FREEFORM TEXT FIELD 5 FROM SC ACC             
EDIFFTX6 DS    CL(L'FFTXSCF6)     FREEFORM TEXT FIELD 6 FROM SC ACC             
EDIFFTX7 DS    CL(L'FFTXSCF7)     FREEFORM TEXT FIELD 7 FROM SC ACC             
EDIFFTX8 DS    CL(L'FFTXSCF8)     FREEFORM TEXT FIELD 8 FROM SC ACC             
*MN SPEC-46328                                                                  
EDILNQ   EQU   *                                                                
                                                                                
***********************************************************************         
* REMITTANCE DELIVERY OPTION TABLE DSECT (BY FORMAT CODE)             *         
***********************************************************************         
RMTTABD  DSECT                                                                  
RMTFRM   DS    CL10               FORMAT CODE                                   
RMTBYTE  DS    XL1                                                              
RMTCTRY  EQU   X'80'              ENTRIES BY COUNTRY                            
RMTCAN   EQU   X'40'              CANADIAN ENTRY                                
RMTVSET  DS    XL1                REMITT SETTING ON VENDOR REC                  
RMTESET  DS    CL2                SETTING FOR EDI                               
RMTLNQ   EQU   *-RMTTABD                                                        
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
* DMPRTQL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQL                                                        
         PRINT ON                                                               
* ACQD                                                                          
         PRINT OFF                                                              
       ++INCLUDE ACQD                                                           
         PRINT ON                                                               
* DDDLCB                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
* DDGETBANKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGETBANKD                                                     
         PRINT ON                                                               
* ACGETFORMD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGETFORMD                                                     
         PRINT ON                                                               
* DDSMTPD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSMTPD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033ACREP5502 08/19/20'                                      
         END                                                                    
