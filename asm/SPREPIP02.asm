*          DATA SET SPREPIP02  AT LEVEL 082 AS OF 09/30/20                      
*PHASE SPIP02A                                                                  
         TITLE 'SPIP02 - CONVERTS I2 AUTOPAY RECS TO WORKER FILE'               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*  QOPT1=T =TST SYSTEM                                                *         
*  QOPT2=Y =WRKR FILE ON HOLD                                         *         
*  QOPT3=Y =TRACE                                                     *         
*  QOPT4=Y =RE-PROCESS AUTOPAY RECS                                   *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- INPUT TAPE RECORD                              *         
*                R3 -- OUTPUT WORKER FILE RECORD                      *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- WORK                                           *         
*                R7 -- WORK                                           *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-49975  09/30/20 ADD AGENCY AI TO THE SPOT AUTOPAY TABLE   *         
* AKAT SPEC-23002  04/25/18 CHANGE DDSUCAN FROM ADV2 TO ADV1          *         
* AKAT SPEC-22255  03/27/17 ADD AGENCY SO TO THE SPOT AUTOPAY TABLE   *         
* AKAT DSSTK-272   04/19/16 ADD AGENCY TD TO THE SPOT AUTOPAY TABLE   *         
* AKAT DSNTK-60    02/05/16 ADD AGY UB (CARPAY) TO NET AUTOPAY TABLE  *         
***********************************************************************         
SPIP02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPIP02,R8,RR=R2                                                
         ST    R2,RELO                                                          
                                                                                
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
                                                                                
         CLI   MODE,REQFRST                                                     
         BE    IP10                                                             
EXIT     XIT1                                                                   
RELO     DC    A(0)                                                             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
IP10     DS    0H                                                               
         MVI   SYSFLG,C'S'          DEFAULT TO SPOT                             
         XC    LASTKEY,LASTKEY      CLEAR AGY/USER-ID KEY                       
         MVI   XAUTOPAY,C'N'        DEFAULT TO NO XSPFIL AUTOPAY                
         L     RF,VMASTC                                                        
         USING MASTD,RF                                                         
         CLI   MCNETPAK,C'Y'        NET AUTOPAY?                                
         BE    IP100                                                            
         DROP  RF                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(1,TDAY)                                       
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,CTDAY)                                 
         XC    CTDAY,=X'FFFF'       COMPLEMENT                                  
         MVI   WRKRSTAT,0                                                       
*                                                                               
         CLI   QOPT1,C'T'          TEST SYSTEM                                  
         BNE   IP12                                                             
         CLI   QAREA+49,C'0'       TEST OUTPUT LIMIT                            
         BL    IP12                                                             
         PACK  DUB,QAREA+49(6)                                                  
         CVB   R0,DUB                                                           
         ST    R0,OUTLIMIT                                                      
*                                                                               
IP12     MVC   DATADISP,=H'24'     DATADISP IS 24 FOR THE SPOTFILE              
         USING APYRECD,R6                                                       
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   APYKTYP,APYKTYPQ    X'0D'                                        
         MVI   APYKSUB,APYKSUBQ    X'3A'                                        
         MVC   APYKDATE,CTDAY      TODAY                                        
         GOTO1 HIGH                                                             
         B     IP30                                                             
IP20     GOTO1 SEQ                                                              
IP30     CLC   KEY(APYKAGMD-APYKEY),KEYSAVE    SAME THRU DATE                   
         BNE   IP300               NO GO AN CHECK FOR XAUTOPAY RECS             
         LA    R6,KEY                                                           
         TM    APYKCNTL,APYKCMON   MULTIPLE MONTHS ON RECORD                    
         BO    IP32                HAVE TO CHECK EACH ELEM FOR ERRS             
         TM    APYKCNTL,APYKCERR   ERRORS ON RECORD (COKE)                      
         BO    IP20                DO NOT PAY                                   
IP32     CLI   QOPT4,C'Y'          RE-PROCESS?                                  
         BE    IP33                YES                                          
         TM    KEY+13,X'01'        ALREADY PROCESSED                            
         BO    IP20                THEN SKIP                                    
*                                                                               
IP33     XC    WORK,WORK           CLEAR WORK                                   
         MVC   WORK(1),APYKAGMD    SAVE AGENCY                                  
         NI    WORK,X'F0'          TURN OFF MEDIA BITS                          
         BAS   RE,CHKAGY           IS AGY IN TABLE                              
         BNE   IP20                NO - READ NEXT AUTOPAY REC                   
         GOTO1 GETBUY              GET THE RECORD                               
         L     R6,ADBUY            A(AUTOPAY RECORD)                            
         MVI   ELCODE,X'03'        ELEMENT X'03'                                
         BAS   RE,GETEL            HAVE A X'03' ELEMENT?                        
         BNE   IP34                NO                                           
         USING APYIDEL,R6          ID ELEMENT DSECT                             
         MVC   WORK+1(8),APYIDUSR  SAVE USER-ID                                 
         MVC   WORK+9(2),APYIDID   SAVE USER NUMBER                             
         DROP  R6                                                               
                                                                                
IP34     CLC   LASTKEY,WORK        SAME KEY AS LAST TIME?                       
         BE    IP50                YES                                          
         MVC   LASTKEY,WORK        SAVE THIS AS THE LAST KEY                    
*                                                                               
         TM    WRKRSTAT,WSTATOPN   FILE ALREADY OPEN?                           
         BNO   *+8                 NO                                           
         BAS   RE,WRKRCLSE         CLOSE LAST AGY                               
         BAS   RE,WRKROPEN         OPEN NEW AGY                                 
*                                                                               
IP50     L     R6,ADBUY                                                         
         USING APYRECD,R6                                                       
         TM    APYRCNTL,APYRCMON   MULTIPLE MONTHS                              
         BO    *+12                CHECK 01 ELEM FOR ERRORS                     
         TM    APYRCNTL,APYRCERR   ERROR - DO NOT PAY                           
         BO    IP20                                                             
         LA    R6,24(R6)                                                        
IP55     CLI   0(R6),0                                                          
         BE    IP70                                                             
         CLI   0(R6),X'01'         ALPHA ELEM                                   
         BE    IP60                                                             
IP58     LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     IP55                                                             
*                                                                               
         USING APYEL,R6                                                         
         USING WRECD,R4                                                         
IP60     CLI   APYERRS,0           SKIP IF ANY ERRORS                           
         BNE   IP58                                                             
         LA    R4,IO2              COPY TO WORKER RECORD                        
         XC    0(256,R4),0(R4)                                                  
         MVC   WRPAYER,APYPAYER                                                 
         MVC   WRMEDIA,APYMED                                                   
         MVC   WRCLIENT,APYCLT                                                  
         MVC   WRPRD,APYPRD                                                     
         MVC   WRPRD2,APYPRD2                                                   
         MVC   WREST,APYEST                                                     
         CLC   =C'ALL',APYEST                                                   
         BE    IP20                SKIP EST = ALL                               
         MVC   WRSTAT(L'APYSTA),APYSTA                                          
         MVC   WRSREP,APYSREP      <- ONLY FIRST 3 CHARS                        
         CLC   APYSREP,SPACES                                                   
         BNH   *+8                                                              
         MVI   WRSREP-1,C'S'       INDICATE SREP PRESENT                        
         MVC   WRMONTH,APYMONTH                                                 
***      CLI   APYLEN,APYLN2Q      NEW ELEM LEN                                 
***      BL    IP65                                                             
***      MVC   WRACN,APYACN        ACN FOR COKE                                 
*                                                                               
IP65     MVC   IO2L(2),=Y(WRLENQ+4)                                             
         BAS   RE,WRKR                                                          
         B     IP58                NEXT ELEM                                    
*                                                                               
IP70     OI    KEY+13,X'01'        MARK KEY PROCESSED                           
         GOTO1 WRITE                                                            
*                                                                               
         OC    OUTLIMIT,OUTLIMIT   TEST ANY OUTPUT LIMIT                        
         BZ    IP72                                                             
         CLC   OUTLIMIT,SEQNUM                                                  
         BH    IP300                                                            
*                                                                               
IP72     LA    R1,350                                                           
         C     R1,SEQNUM           IF AT LEAST 350 WRKR RECS                    
         BH    *+12                OPEN NEW WORKER FILE                         
         BAS   RE,WRKRCLSE                                                      
         BAS   RE,WRKROPEN                                                      
         B     IP20                SEQ                                          
*                                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                                                               
         USING APXRECD,R6          AUTOPAY RECORD DSECT                         
IP300    MVC   DATADISP,=H'42'     DATADISP IS 42 FOR THE XSPOTFILE             
         XC    LASTKEY,LASTKEY     CLEAR AGY/USER-ID KEY                        
         MVI   XAUTOPAY,C'Y'       AUTOPAY ON THE XSPFIL                        
         LA    R6,XKEY             R6 = XKEY                                    
         XC    XKEY,XKEY           CLEAR THE KEY                                
         MVI   APXKTYP,APXKTYPQ    X'0D'                                        
         MVI   APXKSUB,APXKSUBQ    X'3A'                                        
         MVC   APXKDATE,CTDAY      TODAY                                        
         MVC   XKEYSAVE,XKEY       SAVE OFF THE KEY                             
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMRDHI'),=C'XSPDIR',XKEY,XKEY,   *        
               DMWORK                                                           
         B     IP330               GO TEST KEY                                  
*                                                                               
IP320    MVC   XKEYSAVE,XKEY       SAVE OFF THE KEY                             
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMRSEQ'),=C'XSPDIR',XKEY,XKEY,   *        
               DMWORK                                                           
*                                                                               
IP330    CLC   XKEY(4),XKEYSAVE    SAME THRU DATE?                              
         BNE   IPX                 NO - DONE                                    
*                                                                               
         LA    R6,XKEY             R6 = XKEY                                    
         CLI   QOPT4,C'Y'          RE-PROCESS?                                  
         BE    IP333               YES                                          
         TM    APXKCNTL,APXKCPRC   NO - ALREADY PROCESSED?                      
         BO    IP320               YES - SKIP                                   
*                                                                               
IP333    XC    WORK,WORK           CLEAR WORK                                   
         MVC   WORK(1),APXKAGMD    SAVE AGENCY                                  
         NI    WORK,X'F0'          TURN OFF MEDIA BITS                          
         BAS   RE,CHKAGY           IS AGY IN TABLE                              
         BNE   IP320               NO - READ NEXT AUTOPAY REC                   
         DROP  R6                  DROP R6                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'XSPFIL',XKEY+36,     *        
               ADBUY,DMWORK                                                     
*                                                                               
         L     R6,ADBUY            A(AUTOPAY RECORD)                            
         MVI   ELCODE,X'03'        ELEMENT X'03'                                
         BAS   RE,GETEL            HAVE A X'03' ELEMENT?                        
         BNE   IP334               NO                                           
         USING APXIDEL,R6          ID ELEMENT DSECT                             
         MVC   WORK+1(8),APXIDUSR  SAVE USER-ID                                 
         MVC   WORK+9(2),APXIDID   SAVE USER NUMBER                             
         DROP  R6                  DROP R6                                      
*                                                                               
IP334    CLC   LASTKEY,WORK        SAME KEY AS LAST TIME?                       
         BE    IP350               YES                                          
         MVC   LASTKEY,WORK        SAVE THIS AS THE LAST KEY                    
*                                                                               
         TM    WRKRSTAT,WSTATOPN   FILE ALREADY OPEN?                           
         BNO   *+8                 NO                                           
         BAS   RE,WRKRCLSE         CLOSE LAST AGY                               
         BAS   RE,WRKROPEN         OPEN NEW AGY                                 
*                                                                               
IP350    L     R6,ADBUY            R6 = A(AIO)                                  
         USING APXRECD,R6          AUTOPAY RECORD DSECT                         
         TM    APXRCNTL,APXRCERR   ERROR?                                       
         BO    IP320               YES - DO NOT PAY                             
*                                                                               
         MVI   ELCODE,X'01'        ELEMENT X'01'                                
         BAS   RE,GETEL            HAVE A X'01' ELEMENT?                        
         BNE   IP370               NO                                           
*                                                                               
         USING APXEL,R6            ALPHA ELEMENT DSECT                          
         USING WRECDX,R4           WORKER FILE DSECT                            
         CLI   APXERRS,0           ANY ERRORS?                                  
         BNE   IP370               YES - SKIP                                   
         LA    R4,IO2              COPY TO WORKER RECORD                        
         XC    0(256,R4),0(R4)     CLEAR WORKER FILE ENTRY                      
         MVC   WRPAYERX,APXPAYER   PAYER                                        
         MVC   WRMEDIAX,APXMED     MEDIA                                        
         MVC   WRCLTX,APXCLT       CLIENT                                       
         MVC   WRPRDX,APXPRD       PRODUCT                                      
         MVC   WRPRD2X,APXPTN      PIGGY                                        
         MVC   WRESTX,APXEST       ESTIMATE                                     
         CLC   =C'ALL',APXEST      ESTIMATE = ALL?                              
         BE    IP320               YES - SKIP                                   
***      MVC   WRMKTX,APXMKT       MARKET                                       
         MVC   WRSTATX(5),APXSTA   STATION                                      
         MVC   WRSREPX,APXSREP     REP (ONLY FIRST 3 CHARS)                     
         CLC   APXSREP,SPACES      REP <= SPACES?                               
         BNH   IP351               YES                                          
         MVI   WRSREPX-1,C'S'      NO - INDICATE SREP PRESENT                   
         TM    APXFLAGS,APX_PREP   PAY THE REP USING A "P"?                     
         BZ    *+8                 NO                                           
         MVI   WRSREPX-1,C'P'      YES - USE "P" BEFORE REP                     
IP351    MVC   WRMONTHX,APXMONTH   MONTH                                        
***      LA    R3,WRLENQ           LENGTH OF WORKER FILE ENTRY THUS FAR         
         LA    R3,WRLENQX+4        LENGTH OF WORKER FILE ENTRY THUS FAR         
         LA    R2,WRINVNUM         R2 = INVOICE                                 
         BAS   RE,GETA0S           GET A0 PROFILE                               
         DROP  R4                  DROP R4                                      
*                                                                               
         L     R6,ADBUY            R6 = A(AUTOPAY RECORD)                       
*                                                                               
         MVI   ELCODE,X'10'        ELEMENT X'10'                                
         BAS   RE,GETEL            HAVE A X'10' ELEMENT?                        
         B     *+8                 GO TEST ELEMENT                              
*                                                                               
IP355    BAS   RE,NEXTEL           HAVE A X'10' ELEMENT?                        
         BNE   IP365               NO - DONE                                    
*                                                                               
         USING APXIEL,R6           INVOICE ELEMENT DSECT                        
         USING WRINVNUM,R2         INVOICE DSECT IN WORKER FILE                 
         MVC   WRINVNUM,APXIINV    INVOICE NUMBER                               
         ZAP   INVAMT,APXIDOLS     GROSS DOLLARS                                
         CLI   A0PROF,C'N'         PAY BY NET?                                  
         BNE   *+10                NO                                           
         ZAP   INVAMT,APXINET      NET DOLLARS                                  
         AP    INVAMT,APXITAX      ADD IN TAX                                   
         EDIT  INVAMT,WRINVAMT,2,ALIGN=LEFT                                     
         LA    R3,WRLEN2Q(R3)      BUMP WORKER FILE LENGTH                      
         LA    R2,WRLEN2Q(R2)      BUMP WORKER FILE ENTRY                       
         B     IP355               NEXT X'10' ELEMENT                           
         DROP  R6                                                               
*                                                                               
IP365    CHI   R3,L'IO2            WORKER FILE LENGTH > L'IO2?                  
         BNH   *+6                 NO                                           
         DC    H'0'                YES - INCREASE L'IO2                         
         STH   R3,IO2L             WORKER FILE ENTRY LENGTH                     
         BAS   RE,WRKR             ADD LINE TO WORKER FILE                      
*                                                                               
         USING APXRECD,R6          AUTOPAY RECORD DSECT                         
IP370    LA    R6,XKEY             R6 = XKEY                                    
         OI    APXKCNTL,APXKCPRC   MARK KEY PROCESSED                           
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMWRT '),=C'XSPDIR',XKEY,XKEY,   *        
               DMWORK                                                           
*                                                                               
         L     R6,ADBUY            R6 = AUTOPAY RECORD                          
         OI    APXRCNTL,APXRCPRC   MARK RECORD PROCESSED                        
         GOTO1 DATAMGR,DMCB,(X'80',=C'PUTREC'),=C'XSPFIL',XKEY+36,     *        
               ADBUY,DMWORK                                                     
*                                                                               
         OC    OUTLIMIT,OUTLIMIT   ANY OUTPUT LIMIT?                            
         BZ    IP372               NO                                           
         CLC   OUTLIMIT,SEQNUM     PAST OUTPUT LIMIT?                           
         BH    IPX                 YES - EXIT                                   
*                                                                               
IP372    CLC   SEQNUM,=F'350'      UP TO 350 WRKR RECS?                         
         BL    IP320               NO                                           
         BAS   RE,WRKRCLSE         CLOSE CURRENT WORKER FILE                    
         BAS   RE,WRKROPEN         OPEN A NEW WORKER FILE                       
         B     IP320               GO READ SEQ                                  
         DROP  R6                  DROP R6                                      
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                        NET AUTOPAY                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
IP100    DS    0H                                                               
         MVI   SYSFLG,C'N'          NETPAK                                      
         GOTO1 DATCON,DMCB,(5,0),(1,TDAY)                                       
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,CTDAY)                                 
         XC    CTDAY,=X'FFFF'       COMPLEMENT                                  
         MVI   WRKRSTAT,0                                                       
         XC    LASTAGY,LASTAGY                                                  
*                                                                               
         CLI   QOPT1,C'T'          TEST SYSTEM                                  
         BNE   IP112                                                            
         CLI   QAREA+49,C'0'       TEST OUTPUT LIMIT                            
         BL    IP112                                                            
         PACK  DUB,QAREA+49(6)                                                  
         CVB   R0,DUB                                                           
         ST    R0,OUTLIMIT                                                      
*                                                                               
         USING NAPRECD,R6                                                       
IP112    LA    R6,XKEY                                                          
         XC    XKEY,XKEY                                                        
         MVI   NAPKTYP,NAPKTYPQ    X'0D'                                        
         MVI   NAPKSUB,NAPKSUBQ    X'3F'                                        
         MVC   NAPKDATE,CTDAY      TODAY                                        
         MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMRDHI'),=C'XSPDIR',XKEY,XKEY,   *        
               DMWORK                                                           
         B     IP130                                                            
*                                                                               
IP120    MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMRSEQ'),=C'XSPDIR',XKEY,XKEY,   *        
               DMWORK                                                           
*                                                                               
IP130    CLC   XKEY(NAPKAGMD-NAPKEY),XKEYSAVE  SAME THRU DATE                   
         BNE   IPX                                                              
*                                                                               
         LA    R6,XKEY                                                          
         CLI   QOPT4,C'Y'          RE-PROCESS                                   
         BE    IP140                                                            
         TM    NAPKCNTL,NAPKCPRC    PROCESSED?                                  
         BO    IP120                                                            
*                                                                               
IP140    MVC   BYTE,NAPKAGMD                                                    
         NI    BYTE,X'F0'                                                       
         BAS   RE,CHKNAGY          CHECK NET AGENCY                             
         BNE   IP120                                                            
         DROP  R6                                                               
*                                                                               
         CLC   BYTE,LASTAGY        AGENCY BREAK?                                
         BE    IP150                                                            
         MVC   LASTAGY,BYTE                                                     
         TM    WRKRSTAT,WSTATOPN   IS WORKER FILE OPEN?                         
         BNO   *+8                                                              
         BAS   RE,WRKRCLSE         CLOSE LAST AGY WORKER FILE                   
         BAS   RE,WRKROPEN         OPEN NEW AGY WORKER FILE                     
*                                                                               
IP150    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'XSPFIL',XKEY+36,     *        
               ADBUY,DMWORK                                                     
*                                                                               
         LA    R6,XKEY                                                          
         USING NAPRECD,R6                                                       
         OI    NAPKCNTL,NAPKCPRC    MARK KEY PROCESSED                          
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMWRT '),=C'XSPDIR',XKEY,XKEY,   *        
               DMWORK                                                           
*                                                                               
         L     R6,ADBUY                                                         
         OI    NAPRCNTL,NAPRCPRC    MARK REC PROCESSED                          
         GOTO1 DATAMGR,DMCB,(X'80',=C'PUTREC'),=C'XSPFIL',XKEY+36,     *        
               ADBUY,DMWORK                                                     
*                                                                               
         BAS   RE,GETA0N           GET A0 PROFILE FOR NET                       
*                                                                               
         L     R6,ADBUY                                                         
         USING NAPRECD,R6                                                       
*                                                                               
         LA    R4,IO2                                                           
         USING NWRECD,R4                                                        
         XC    0(256,R4),0(R4)                                                  
         XC    256(256,R4),256(R4)                                              
         LA    R4,IO2                                                           
*                                                                               
         GOTO1 HEXOUT,DMCB,XKEY+2,NWRXKEY,30,=C'TOG'                            
*                                                                               
         MVC   NWRMED,NAPMED        MEDIA                                       
         CLI   NAPSMED,C' '                                                     
         BE    *+10                                                             
         MVC   NWRMED,NAPSMED                                                   
*                                                                               
         MVC   NWRCLI,NAPCLT        CLIENT                                      
         MVC   NWRSTA,NAPSTA        NETWORK                                     
*                                                                               
         MVC   NWRSREP,NAPSREP      SPECIAL REP                                 
         OC    NWRSREP,NWRSREP                                                  
         BZ    *+8                                                              
         MVI   NWRSREP,C'S'                                                     
*                                                                               
         MVC   NWRMONTH(6),NAPMONTH    MMM/YY                                   
*                                                                               
         CLI   NAPMNTYP,C'B'           BROADCAST?                               
         BNE   *+12                                                             
         MVI   NWRMONTH+6,C'-'                                                  
         MVI   NWRMONTH+7,C'B'                                                  
*                                                                               
         XC    OPTFLD,OPTFLD                                                    
         LA    R3,OPTFLD                                                        
         LA    R5,65                                                            
*                                                                               
IP200    DS    0H                   CREATE OPTIONS FIELD                        
         CLC   NAPEST,=C'000'       ESTIMATE?                                   
         BE    IP210                                                            
         MVC   0(2,R3),=C'E='                                                   
         AHI   R3,2                                                             
         SHI   R5,2                                                             
*                                                                               
         LA    RF,NAPEST                                                        
         LA    RE,L'NAPEST                                                      
IP201    CLI   0(RF),C' '                                                       
         BE    IP202                                                            
         CLI   0(RF),0                                                          
         BE    IP202                                                            
         MVC   0(1,R3),0(RF)                                                    
         AHI   RF,1                                                             
         AHI   R3,1                                                             
         SHI   R5,1                                                             
         BCT   RE,IP201                                                         
*                                                                               
IP202    MVI   0(R3),C','                                                       
         AHI   R3,1                                                             
         SHI   R5,1                                                             
*                                                                               
IP210    DS    0H                                                               
         OC    NAPPAK,NAPPAK        PACKAGE?                                    
         BZ    IP220                                                            
         MVC   0(3,R3),=C'PA='                                                  
         AHI   R3,3                                                             
         SHI   R5,3                                                             
*                                                                               
         LA    RF,NAPPAK                                                        
         LA    RE,L'NAPPAK                                                      
IP211    CLI   0(RF),C' '                                                       
         BE    IP212                                                            
         CLI   0(RF),0                                                          
         BE    IP212                                                            
         MVC   0(1,R3),0(RF)                                                    
         AHI   RF,1                                                             
         AHI   R3,1                                                             
         SHI   R5,1                                                             
         BCT   RE,IP211                                                         
*                                                                               
IP212    MVI   0(R3),C','                                                       
         AHI   R3,1                                                             
         SHI   R5,1                                                             
*                                                                               
IP220    DS    0H                                                               
         OC    NAPPGRP,NAPPGRP      PRODUCT GROUP?                              
         BZ    IP230                                                            
         MVC   0(4,R3),=C'PGR='                                                 
         AHI   R3,4                                                             
         SHI   R5,4                                                             
*                                                                               
         LA    RF,NAPPGRP                                                       
         LA    RE,L'NAPPGRP                                                     
IP221    CLI   0(RF),C' '                                                       
         BE    IP222                                                            
         CLI   0(RF),0                                                          
         BE    IP222                                                            
         MVC   0(1,R3),0(RF)                                                    
         AHI   RF,1                                                             
         AHI   R3,1                                                             
         SHI   R5,1                                                             
         BCT   RE,IP221                                                         
*                                                                               
IP222    MVI   0(R3),C','                                                       
         AHI   R3,1                                                             
         SHI   R5,1                                                             
*                                                                               
IP230    DS    0H                   PAY TYPE                                    
         MVI   0(R3),C'T'                                                       
         CLI   NAPPTYP,0            TIME?                                       
         BE    IP231                                                            
         CLI   NAPPTYP,C'T'                                                     
         BE    IP231                                                            
         MVI   0(R3),C'I'                                                       
         CLI   NAPPTYP,C'I'         INTEGRATION?                                
         BE    IP231                                                            
         MVC   0(4,R3),=C'TINT'       TIME + INTEGRATION                        
         AHI   R3,4                                                             
         SHI   R5,4                                                             
         B     IP232                                                            
*                                                                               
IP231    AHI   R3,1                                                             
         SHI   R5,1                                                             
IP232    MVI   0(R3),C','                                                       
         AHI   R3,1                                                             
         SHI   R5,1                                                             
*                                                                               
IP240    DS    0H                   PRODUCTS                                    
         OC    NAPPRD,NAPPRD                                                    
         BZ    IP250                                                            
         CLC   NAPPRD,=C'POL'                                                   
         BE    IP250                                                            
         MVC   0(2,R3),=C'P='                                                   
         AHI   R3,2                                                             
         SHI   R5,2                                                             
*                                                                               
         LA    RF,NAPPRD                                                        
         LA    RE,L'NAPPRD                                                      
IP241    CLI   0(RF),C' '           FILL IN FIRST PRODUCT                       
         BE    IP242                                                            
         MVC   0(1,R3),0(RF)                                                    
         AHI   RF,1                                                             
         AHI   R3,1                                                             
         SHI   R5,1                                                             
         BCT   RE,IP241                                                         
*                                                                               
IP242    OC    NAPPRD2,NAPPRD2      2ND PRODUCT?                                
         BZ    IP244                                                            
         MVI   0(R3),C'/'                                                       
         AHI   R3,1                                                             
         SHI   R5,1                                                             
*                                                                               
         LA    RF,NAPPRD2                                                       
         LA    RE,L'NAPPRD2                                                     
IP243    CLI   0(RF),C' '                                                       
         BE    IP244                                                            
         MVC   0(1,R3),0(RF)                                                    
         AHI   RF,1                                                             
         AHI   R3,1                                                             
         SHI   R5,1                                                             
         BCT   RE,IP243                                                         
*                                                                               
IP244    DS    0H                                                               
         MVI   0(R3),C','                                                       
         AHI   R3,1                                                             
         SHI   R5,1                                                             
*                                                                               
IP250    DS    0H                                                               
         ST    R3,AINVNUM                                                       
*                                                                               
         L     R6,ADBUY             NOW PROCESS INVOICE #'S AND $'S             
         USING NAPRECD,R6                                                       
         LA    R6,NAPEL                                                         
         LLC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         DROP  R6                                                               
*                                                                               
         CLI   0(R6),X'02'          NO MORE, DONE                               
         BE    *+6                                                              
         DC    H'00'                                                            
         USING NAPIEL,R6                                                        
*                                                                               
IP310    DS    0H                                                               
         L     RF,AINVNUM                                                       
         MVC   0(4,RF),=C'INV='                                                 
         MVC   4(10,RF),NAPIINV      MOVE INVOICE NUMBER TO OPT FIELD           
         MVC   NWROPT,OPTFLD        OPTIONS FIELD                               
         MVC   NWRINV1,NAPIINV      INVOICE #  INTO SCRIPT                      
*                                                                               
         MVC   FULL,NAPIDOLS        GROSS $$$                                   
         CLI   A0PROF,C'N'          PAY BY NET?                                 
         BNE   *+10                                                             
         MVC   FULL,NAPINET         NET $$$                                     
*                                                                               
         EDIT  FULL,TEMPDOL,2,ALIGN=LEFT                                        
         MVC   NWRINV1$,TEMPDOL                                                 
*                                                                               
         MVC   IO2L(2),=Y(NWRLENQ+4)                                            
         BAS   RE,WRKR                                                          
*                                                                               
IP405    OC    OUTLIMIT,OUTLIMIT   TEST ANY OUTPUT LIMIT                        
         BZ    IP410                                                            
         CLC   OUTLIMIT,SEQNUM                                                  
         BH    IPX                                                              
*                                                                               
IP410    DS    0H                                                               
         LA    R1,350                                                           
         C     R1,SEQNUM           IF AT LEAST 350 WRKR RECS                    
         BH    *+12                OPEN NEW WORKER FILE                         
         BAS   RE,WRKRCLSE                                                      
         BAS   RE,WRKROPEN                                                      
         B     IP120               SEQ                                          
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                        EXIT ROUNTINE                                *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
IPX      DS    0H                                                               
         BAS   RE,WRKRCLSE                                                      
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                             ERRORS                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
IPERROR  GOTO1 REPORT                                                           
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*   GET A0 PROFILE FOR SPOT                                           *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
GETA0S   NTR1                                                                   
         L     R6,ADBUY              R6 = A(AIO)                                
         USING APXRECD,R6            AUTOPAY RECORD DSECT                       
*                                                                               
         XC    KEY,KEY               CLEAR THE KEY                              
         MVC   KEY(4),=C'S0A0'       GET PAY PROGRAM PROFILE                    
         MVC   KEY+4(2),QAGY         AGENCY                                     
         MVC   KEY+7(3),=X'FFFFFF'   FAKE CLT OR WILL RETURN MED LEVEL          
         MVI   KEY+10,C'*'           READ OFFICE LEVEL IF THERE IS ONE          
         MVC   KEY+11(1),APXKOFC     MOVE CLIENT OFFICE INTO PROFILE            
*                                                                               
         GOTO1 GETPROF,DMCB,KEY,A0PROF,DATAMGR                                  
         CLI   8(R1),0               ANY ERRORS?                                
         BE    EXIT                  NO - EXIT                                  
         DC    H'0'                  YES - DEATH                                
*                                                                               
         DROP  R6                    DROP AUTOPAY DSECT                         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*   GET A0 PROFILE FOR NET                                            *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
GETA0N   NTR1                                                                   
         L     R6,ADBUY                                                         
         USING NAPRECD,R6                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(4),=C'S0A0'     GET PAY PROGRAM PROFILE                      
         MVC   KEY+4(2),QAGY                                                    
         MVC   KEY+6(1),NAPMED                                                  
         CLI   NAPSMED,X'40'                                                    
         BNH   *+10                                                             
         MVC   KEY+6(1),NAPSMED                                                 
         GOTO1 GETPROF,DMCB,(X'90',KEY),A0PROF,DATAMGR                          
         CLI   8(R1),0                                                          
         BE    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
         DROP  R6                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*   CHECK AGENCY TABLE FOR HEX CODE IN WORK - RETURN ENTRY IN AGYENTRY*         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
CHKAGY   NTR1                                                                   
         USING AGYTABD,R3                                                       
         XC    AGYENTRY,AGYENTRY                                                
         LA    R3,AGYTABLE         CLEAR WKFILE BUFFER                          
CA10     CLI   0(R3),X'FF'                                                      
         BE    CAXNO               NOT IN TABLE                                 
         CLC   WORK(1),ATAGYHEX    MATCH ON AGY HEX                             
         BNE   CA20                                                             
         CLC   QAGY,ATALPHA        AND MATCH ON AGY ALPHA                       
         BE    CAXYES                                                           
CA20     LA    R3,ATLENQ(R3)                                                    
         B     CA10                                                             
CAXYES   MVC   AGYENTRY,0(R3)      SAVE ENTRY                                   
         SR    RC,RC                                                            
CAXNO    LTR   RC,RC                                                            
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*   CHECK AGENCY TABLE FOR HEX CODE IN BYTE - RETURN ENTRY IN AGYENTRY*         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
CHKNAGY  NTR1                                                                   
         USING AGYTABD,R3                                                       
         XC    AGYENTRY,AGYENTRY                                                
         LA    R3,NAGYTBLE         CLEAR WKFILE BUFFER                          
CAN10    CLI   0(R3),X'FF'                                                      
         BE    CANXNO               NOT IN TABLE                                
         CLC   BYTE,ATAGYHEX       MATCH ON AGY HEX                             
         BNE   CAN20                                                            
         CLC   QAGY,ATALPHA        AND MATCH ON AGY ALPHA                       
         BE    CANXYES                                                          
CAN20    LA    R3,ATLENQ(R3)                                                    
         B     CAN10                                                            
CANXYES  MVC   AGYENTRY,0(R3)      SAVE ENTRY                                   
         SR    RC,RC                                                            
CANXNO   LTR   RC,RC                                                            
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                   OPEN WORKER FILE                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKROPEN NTR1                                                                   
         L     R0,AWKBUFF          CLEAR WKFILE BUFFER                          
         L     R1,=A(WKBUFFX-WKBUFF)                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING AGYTABD,R6                                                       
         LA    R6,AGYENTRY         AGENCY TABLE ENTRY                           
*                                                                               
         XC    SEQNUM,SEQNUM                                                    
         USING WLHDRD,R4                                                        
         LA    R4,IO2                                                           
         XC    0(255,R4),0(R4)     BUILD HEADER                                 
         MVC   WLSOFLAB,=C'*SOFSOF*'                                            
         MVC   WLUSRID,ATIDNUM     AGENCY USER ID NUMBER                        
         OC    LASTID,LASTID       HAVE OVERRIDE ID FROM X'03' ELEMENT?         
         BZ    *+10                NO                                           
         MVC   WLUSRID,LASTID      YES - USE THIS ID NUMBER                     
         MVC   WLSYSPRG(3),=C'APY'                                              
         CLI   SYSFLG,C'S'                                                      
         BE    *+10                                                             
         MVC   WLSYSPRG(3),=C'NAP'                                              
         MVC   WLSUBPRG,ATADV      SET FACPAK                                   
*                                                                               
         CLI   QOPT1,C'T'          OPTION TO GENERATE ON TST                    
         BNE   *+8                                                              
         MVI   WLSUBPRG,C'T'       SET FACTST                                   
*                                                                               
         CLI   QOPT1,C'C'          OPTION TO GENERATE ON CSC                    
         BNE   *+8                                                              
         MVI   WLSUBPRG,C'S'       SET FACCSC                                   
*                                                                               
         MVC   WLDAY,TDAY+2                                                     
         MVI   WLCLASS,C'T'        CLASS T FOR WRKF SCRIPTS                     
         MVI   WLTYPE,C'A'         TYPE A FOR IMMEDIATE EXEC                    
         MVC   WLPSWD,SPACES                                                    
         OI    WLATTB,WLATOBJ                                                   
*                                                                               
         LA    R3,IO2                                                           
         MVI   FIXED,C'Y'                                                       
         BAS   RE,WRKR                                                          
*                                                                               
         XC    SEQNUM,SEQNUM                                                    
         XC    WRKRINDX,WRKRINDX                                                
         LA    R5,WRKRINDX                                                      
         USING UKRECD,R5                                                        
         MVC   UKUSRID,ATIDNUM     AGENCY ID NUM                                
         OC    LASTID,LASTID       HAVE OVERRIDE ID FROM X'30' ELEMENT?         
         BZ    *+10                NO                                           
         MVC   UKUSRID,LASTID      YES - USE THIS ID NUMBER                     
         MVC   UKSYSPRG(3),=C'APY'                                              
         CLI   SYSFLG,C'S'                                                      
         BE    *+10                                                             
         MVC   UKSYSPRG(3),=C'NAP'                                              
         MVC   UKFILENO,WLREPRNO   WORKER FILE NUMBER                           
         DROP  R5                                                               
*                                                                               
         MVC   WRKFNO,WLREPRNO     WORKER FILE NUMBER                           
         MVI   FIXED,C'N'                                                       
*                                                                               
         MVC   P(16),=C'WORKER FILE ID ='                                       
         EDIT  (B2,WLUSRID),(4,P+20)                                            
         MVI   P+24,C','                                                        
         MVC   P+25(4),WLFILEID                                                 
         GOTO1 HEXOUT,DMCB,WLDAY,P+29,1,=C'TOG'                                 
         MVC   P+31(1),WLCLASS                                                  
         MVI   P+32,C','                                                        
         EDIT  WLREPRNO,(5,P+33),0,ALIGN=LEFT                                   
         GOTO1 REPORT                                                           
         DROP  R4                                                               
*                                                                               
         LA    R1,IO2                                                           
         XC    0(256,R1),0(R1)                                                  
         MVC   0(4,R1),=F'2101'                                                 
         MVC   4(6,R1),=C'SCRIPT'                                               
         MVC   10(8,R1),=C'SPAUTPAY'                                            
         CLI   XAUTOPAY,C'Y'         AUTOPAY CLEAR BY REP FEATURE?              
         BNE   *+10                  NO                                         
         MVC   10(8,R1),=C'SPXATPAY' YES - SCRIPT IS SPXATPAY                   
         CLI   SYSFLG,C'N'                                                      
         BNE   *+10                                                             
         MVC   10(8,R1),=C'NEAUTPAY'                                            
*                                                                               
         MVI   18(R1),C'I'           SET TYPE TO INSERT                         
         MVC   30(5,R1),=C'00006'    HEADER LENGTH                              
         MVC   35(5,R1),=C'00100'    INPUT  DATA LENGTH                         
         MVC   40(5,R1),=C'00020'    OUTPUT DATA LENGTH                         
         MVI   45(R1),C'Y'           INSERT ERRORS AT FILE END                  
         MVC   IO2L(2),=H'76'        72 + 4 BYTES FOR QSAM                      
         BAS   RE,WRKR                                                          
*                                                                               
         LA    R1,IO2                                                           
         XC    0(256,R1),0(R1)                                                  
         MVC   10(20,R1),=CL20'SIGN-ON INFORMATION '                            
         MVC   30(8,R1),ATSIGNON                                                
         OC    LASTUSR,LASTUSR     HAVE OVERRIDE USERID FROM '30' ELEM?         
         BZ    *+10                NO                                           
         MVC   30(8,R1),LASTUSR    YES - USE THAT ONE                           
         MVC   38(8,R1),ATPASSWD                                                
         MVC   IO2L(2),=H'50'        46 + 4 BYTES FOR QSAM                      
         BAS   RE,WRKR                                                          
*                                                                               
         OI    WRKRSTAT,WSTATOPN   FILE OPEN                                    
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                   CLOSE WORKER FILE                                 *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKRCLSE NTR1                                                                   
         TM    WRKRSTAT,WSTATOPN         FILE ALREADY OPEN                      
         BNO   WRKRCLX                                                          
         USING WLHDRD,R4                                                        
         LA    R4,IO2                                                           
         XC    0(255,R4),0(R4)           BUILD HEADER                           
         MVC   WLSOFLAB,=C'*EOFEOF*'                                            
         MVI   FIXED,C'Y'                                                       
         XC    SEQNUM,SEQNUM                                                    
         BAS   RE,WRKR                                                          
         NI    WRKRSTAT,X'FF'-WSTATOPN   FILE NOT OPEN                          
*                                                                               
*NOP     CLC   =C'CK',QAGY         IF COKE                                      
*NOP     BE    WRKRC10             FORCE TO HOLD FOR NOW                        
*                                                                               
         CLI   QOPT2,C'Y'                OPTION TO SET TO STATUS HOLD           
         BNE   EXIT                                                             
WRKRC10  GOTO1 DATAMGR,DMCB,=C'HOLD    ',WRKFILEN,WRKRINDX,IO2,AWKBUFF          
WRKRCLX  B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                    ADD LINE TO WORKER FILE                          *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKR     NTR1                                                                   
         OC    SEQNUM,SEQNUM                                                    
         BZ    WRKR10                                                           
         MVC   IO2(4),=F'2102'                                                  
         EDIT  SEQNUM,(6,IO2+4),0,FILL=0                                        
                                                                                
WRKR10   DS    0H                                                               
         LA    R3,IO2                                                           
         CLI   FIXED,C'Y'                                                       
         BE    *+8                                                              
         LA    R3,IO2L                                                          
                                                                                
         GOTO1 DATAMGR,DMCB,DMPRINT,WRKFILE,0,(R3),AWKBUFF                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         ST    R1,SEQNUM                                                        
         CLI   QOPT3,C'Y'                                                       
         BNE   EXIT                                                             
         MVC   P,IO2                                                            
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         LTORG                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*   AGENCY TABLE -  ALPHA CODE,SPOT AGY,USERID,PASSWD,ID NUMBER,ADV             
AGYTABLE DC    C'SJ',X'C0',CL8'SJR     ',CL8'        ',XL2'0011',C'1'           
         DC    C'*B',X'90',CL8'DDSB    ',CL8'        ',XL2'0A4B',C'T'           
         DC    C'T1',X'F0',CL8'TCH1    ',CL8'        ',XL2'002B',C'T'           
         DC    C'CK',X'B0',CL8'COKEAT  ',CL8'DDS     ',XL2'1131',C'1'           
         DC    C'YN',X'10',CL8'YNEZ    ',CL8'DDS     ',XL2'181A',C'5'           
         DC    C'H9',X'80',CL8'BROMRE  ',CL8'DDS     ',XL2'34BF',C'1'           
         DC    C'JW',X'B0',CL8'JWNYA   ',CL8'DDS     ',XL2'0016',C'2'           
         DC    C'H7',X'30',CL8'MSNYA   ',CL8'DDS     ',XL2'2192',C'2'           
         DC    C'PC',X'10',CL8'OSNYREO ',CL8'DDS     ',XL2'2FA7',C'7'           
         DC    C'GZ',X'E0',CL8'GMMRE   ',CL8'DDS     ',XL2'23C6',C'5'           
         DC    C'HD',X'40',CL8'HDTO    ',CL8'DDS     ',XL2'011B',C'2'           
         DC    C'DF',X'20',CL8'DFRES   ',CL8'DDS     ',XL2'2F0B',C'7'           
         DC    C'TH',X'90',CL8'ZEREP   ',CL8'DDS     ',XL2'319B',C'7'           
         DC    C'M2',X'F0',CL8'MENYSFS ',CL8'DDS     ',XL2'32E8',C'2'           
         DC    C'FR',X'E0',CL8'FDMJW   ',CL8'DDS     ',XL2'1974',C'2'           
         DC    C'TB',X'50',CL8'ZOTOA   ',CL8'DDS     ',XL2'3444',C'2'           
         DC    C'NT',X'30',CL8'MMCTO   ',CL8'DDS     ',XL2'2C24',C'2'           
         DC    C'H0',X'60',CL8'MSHTOA  ',CL8'DDS     ',XL2'219A',C'2'           
         DC    C'G+',X'10',CL8'GGWLNY  ',CL8'DDS     ',XL2'3368',C'2'           
         DC    C'FM',X'10',CL8'FMNN    ',CL8'DDS     ',XL2'32AC',C'5'           
         DC    C'H1',X'E0',CL8'DDSUCAN ',CL8'DDS     ',XL2'1F63',C'1'           
         DC    C'O0',X'90',CL8'SMGTOUS ',CL8'DDS     ',XL2'3FBD',C'2'           
         DC    C'TR',X'D0',CL8'TLDA    ',CL8'DDS     ',XL2'001F',C'1'           
         DC    C'UB',X'70',CL8'CARPAY  ',CL8'DDS     ',XL2'40F7',C'6'           
         DC    C'OU',X'70',CL8'OMDTOA  ',CL8'DDS     ',XL2'1DB2',C'5'           
         DC    C'HY',X'90',CL8'MCMTOA  ',CL8'DDS     ',XL2'3F41',C'2'           
         DC    C'T$',X'20',CL8'TIGDNA  ',CL8'DDS     ',XL2'30A5',C'1'           
         DC    C'DV',X'10',CL8'MHI     ',CL8'DDS     ',XL2'4573',C'1'           
         DC    C'TD',X'20',CL8'TSMED   ',CL8'DDS     ',XL2'3D42',C'6'           
         DC    C'SO',X'A0',CL8'SOTOT   ',CL8'DDS     ',XL2'0E02',C'2'           
         DC    C'AI',X'60',CL8'ACTIVE  ',CL8'DDS     ',XL2'51FE',C'1'           
         DC    X'FF'                                                            
*   AGENCY TABLE -  ALPHA CODE,SPOT AGY,USERID,PASSWD,ID NUMBER,ADV             
NAGYTBLE DC    C'SJ',X'F0',CL8'SJR     ',CL8'DDS     ',XL2'0011',C'3'           
         DC    C'FR',X'30',CL8'FDMJW   ',CL8'DDS     ',XL2'1974',C'3'           
         DC    C'H7',X'20',CL8'MSNYA   ',CL8'DDS     ',XL2'2192',C'3'           
         DC    C'*B',X'C0',CL8'DDSB    ',CL8'DDS     ',XL2'0A4B',C'T'           
         DC    C'*1',X'D0',CL8'DDS1    ',CL8'DDS     ',XL2'0A5A',C'3'           
         DC    C'YN',X'60',CL8'YNEZ    ',CL8'DDS     ',XL2'181A',C'3'           
         DC    C'M2',X'D0',CL8'MENYSFS ',CL8'DDS     ',XL2'32E8',C'3'           
         DC    C'TH',X'D0',CL8'ZEREP   ',CL8'DDS     ',XL2'319B',C'3'           
         DC    C'PC',X'20',CL8'OSNYREO ',CL8'DDS     ',XL2'2FA7',C'3'           
         DC    C'G+',X'40',CL8'GGWLNY  ',CL8'DDS     ',XL2'3368',C'3'           
         DC    C'FM',X'20',CL8'FMNN    ',CL8'DDS     ',XL2'32AC',C'5'           
         DC    C'DV',X'10',CL8'MHI     ',CL8'DDS     ',XL2'4573',C'3'           
         DC    C'UB',X'60',CL8'CARPAY  ',CL8'DDS     ',XL2'40F7',C'6'           
         DC    X'FF'                                                            
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
DMPRINT  DC    CL8'DMPRINT'                                                     
WRKFILE  DC    CL8'WRKF1  '                                                     
WRKFILEN DC    CL8'WRKFILE'                                                     
AWKBUFF  DC    A(WKBUFF)                                                        
COUNT    DS    F                   NUMBER OF RECORDS PUT OUT                    
OUTLIMIT DS    F                                                                
SEQNUM   DS    F                                                                
WRKFNO   DS    XL2                 WORKER FILE NUMBER                           
SIGNON2H DS    XL2                 2 BYTE HEX AGENCY ID                         
LASTAGY  DS    XL1                                                              
TDAY     DS    XL3                 YYMMDD PWOS                                  
CTDAY    DS    XL2                 COMPRESSED TODAY                             
DMACTN   DS    CL5                                                              
FIXED    DS    CL1                                                              
WRKRSTAT DS    XL1                                                              
WSTATOPN EQU   X'80'               A WORKER FILE IS OPEN                        
WRKRCMD  DS    CL7                                                              
WRKRINDX DS    CL42                                                             
AGYENTRY DS    XL(ATLENQ)          AGENCY TABLE ENTRY                           
*                                                                               
TEMPDOL  DS    CL12                                                             
OPTFLD   DS    CL65                                                             
XKEY     DS    CL64                                                             
XKEYSAVE DS    CL64                                                             
*                                                                               
SYSFLG   DS    CL1                 (N)ETPAK OR (S)POT?                          
*                                                                               
AINVNUM  DS    A                                                                
A0PROF   DS    CL16                                                             
INVAMT   DS    PL6                                                              
*                                                                               
LASTKEY  DS    0XL11               AGENCY/USER-ID/ID NUMBER KEY                 
LASTAGCY DS    XL1                 LAST AGENCY                                  
LASTUSR  DS    CL8                 LAST USER-ID FROM X'30' ELEMENT              
LASTID   DS    XL2                 LAST ID NUMB FROM X'30' ELEMENT              
*                                                                               
XAUTOPAY DS    CL1                 XSPFIL AUTOPAY                               
ELCODE   DS    XL1                                                              
         DS    0D                                                               
ELEM     DS    CL256                                                            
IO       DS    XL256               IO AREA                                      
IO2L     DS    F                                                                
IO2      DS    XL600               IO AREA                                      
WKBUFF   DS    14336C                                                           
WKBUFFX  EQU   *                                                                
***********************************************************************         
*        WORKER FILE DSECT                                                      
***********************************************************************         
WRECD    DSECT                                                                  
WRHDR    DS    CL30                HEADER                                       
WRPAYER  DS    CL12                PAYER                                        
WRACN    DS    CL5                 ACN FOR ID=ACN OPTION FOR COKE               
WRMEDIA  DS    CL1                 MEDIA                                        
WRCLIENT DS    CL3                 CLIENT                                       
WRPRD    DS    CL3                 PRODUCT                                      
WRPRD2   DS    CL3                 PARTNER                                      
WREST    DS    CL3                 ESTIMATE                                     
WRSTAT   DS    CL10                STATION                                      
         DS    CL1                 SPARE                                        
WRSREP   DS    CL3                 SPECIAL REP                                  
WRMONTH  DS    CL6                 MONTH (MMM/YY)                               
WRINVCE  DS    CL12                INVOICE TRACKING                             
WRLENQ   EQU   *-WRECD                                                          
***********************************************************************         
*        XSPFIL WORKER FILE DSECT                                               
***********************************************************************         
WRECDX   DSECT                                                                  
WRHDRX   DS    CL30                HEADER                                       
WRPAYERX DS    CL12                PAYER                                        
***WRMKTX   DS    CL4                 MARKET                                    
WRMEDIAX DS    CL1                 MEDIA                                        
WRCLTX   DS    CL3                 CLIENT                                       
WRPRDX   DS    CL3                 PRODUCT                                      
WRPRD2X  DS    CL3                 PARTNER                                      
WRESTX   DS    CL3                 ESTIMATE                                     
WRSTATX  DS    CL10                STATION                                      
         DS    CL1                 "S" BEFORE REP CODE                          
WRSREPX  DS    CL3                 SPECIAL REP                                  
WRMONTHX DS    CL6                 MONTH (MMM/YY)                               
WRLENQX  EQU   *-WRECDX            LENGTH WITHOUT INVOICE ENTRIES               
WRINVNUM DS    CL10                INVOICE NUMBER                               
WRINVAMT DS    CL11                INVOICE AMOUNT                               
WRLEN2Q  EQU   *-WRINVNUM          LENGTH OF INVOICE ENTRY                      
*                                                                               
***********************************************************************         
*        NET WORKER FILE DSECT                                                  
***********************************************************************         
NWRECD   DSECT                                                                  
NWRHDR   DS    CL30                HEADER                                       
NWRMSG   DS    CL60                MESSAGE ERRORS/SUCCESS                       
NWRMED   DS    CL1                 MEDIA                                        
NWRCLI   DS    CL3                 CLIENT                                       
NWRSTA   DS    CL4                 STATION                                      
NWRSREP  DS    CL4                 SPECIAL REP                                  
NWRMONTH DS    CL20                MONTH (MMM/YY)                               
NWROPT   DS    CL65                OPTIONS                                      
NWRINV1  DS    CL10                INVOICE #                                    
NWRINV1$ DS    CL12                INVOICE DOLLARS                              
NWRXKEY  DS    CL60                AUTOPAY KEY                                  
NWRLENQ  EQU   *-NWRECD                                                         
*                                                                               
***********************************************************************         
*        AGENCY TABLE DSECT                                                     
***********************************************************************         
AGYTABD  DSECT                                                                  
ATALPHA  DS    CL2                 AGENCY ALPHA ID                              
ATAGYHEX DS    XL1                 AGENCY SPOT HEX CODE                         
ATSIGNON DS    CL8                 AGENCY SIGNON ID                             
ATPASSWD DS    CL8                 AGENCY SIGNON PASSWORD                       
ATIDNUM  DS    XL2                 AGENCY USER ID NUMBER                        
ATADV    DS    CL1                 AGENCY'S ADV                                 
ATLENQ   EQU   *-AGYTABD                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPGENAPY                                                       
       ++INCLUDE SPGENXAPY                                                      
       ++INCLUDE NEGENAPY                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE DMWRKFL                                                        
       ++INCLUDE DMWRKFK                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDMASTD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'082SPREPIP02 09/30/20'                                      
         END                                                                    
