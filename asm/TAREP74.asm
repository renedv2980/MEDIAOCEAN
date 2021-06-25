*          DATA SET TAREP74    AT LEVEL 014 AS OF 06/19/15                      
*PHASE T70374E                                                                  
*INCLUDE DLFLD                                                                  
*INCLUDE SMTP                                                                   
         TITLE 'T70374 - YTDAUDIT'                                              
T70374   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70350                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          RA=A(SCREEN)                                 
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL DSECT)                            
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING RECD,R7             R7=A(LOCAL W/S)                              
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
         CLI   MODE,VALKEY         IF CALLED WITH VALIDATE KEY                  
         BNE   *+12                                                             
         BAS   RE,VK               VALIDATE THE KEY                             
         B     XIT                                                              
*                                                                               
         CLI   MODE,PRINTREP       IF CALLED WITH PRINTREP                      
         BNE   *+8                                                              
         BAS   RE,PREP             PROCESS THE REPORT                           
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              ROUTINE TO VALIDATE KEY                                          
*                                                                               
VK       NTR1                                                                   
         BAS   RE,VALOPT           MERELY VALIDATE THE OPTIONS                  
         B     XIT                                                              
*                                                                               
*              ROUTINE TO VALIDATE OPTIONS                                      
*                                                                               
VALOPT   NTR1                                                                   
         LA    R2,SPLOPTH          VALIDATE OPTIONS                             
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         SPACE 1                                                                
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            R0 = NUM OF SCAN BLOCK ENTRIES               
*                                                                               
VOPT4    CLC   =C'TRACE',SCDATA1                                                
         BNE   FLDINV                                                           
         CLI   SCDATA2,C'Y'        IF TRACING REQUESTED                         
         BNE   VOPT8                                                            
         OI    OPTION,X'80'        SAVE INDICATOR                               
*                                                                               
VOPT8    LA    R3,SCANNEXT         BUMP TO NEXT ELEMENT                         
         BCT   R0,VOPT4                                                         
*                                                                               
VOPTX    B     XIT                                                              
         DROP  R3                  DROP THE SCAND DSECT                         
         EJECT                                                                  
*                                                                               
*              ROUTINE CONTROLS REPORT GENERATION                               
*                                                                               
PREP     NTR1                                                                   
         USING DLCBD,R5                                                         
         LA    R5,DLCB                                                          
         BAS   RE,INITDOWN         INITIALIZE DOWNLOAD                          
                                                                                
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL9'CHKFIL'                                              
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         OPEN  (RECVIN,(INPUT))    OPEN THE INPUT FILE                          
*                                                                               
PR10     LA    R2,RCVREC           R2 = A(INPUT AREA)                           
         GET   RECVIN,(R2)                                                      
         LA    R2,4(R2)            R2 = A(RECOVERY HEADER)                      
         USING RCVD,R2                                                          
*                                                                               
         CLI   RFILTY,X'76'        ONLY INTERESTED IN CHKFILE                   
         BNE   PR10                                                             
*                                                                               
         CLI   RPERSON+1,0                                                      
         BNE   PR10                                                             
*                                                                               
         LA    R4,RDATA            R4 = A(ACTUAL RECORD)                        
         USING TLCKD,R4                                                         
         CLI   TLCKCD,TLCKCDQ      ONLY INTERESTED IN CHECK RECORDS             
         BNE   PR10                                                             
         TM    TLCKSTAT,X'80'      DELETED?                                     
         BO    PR10                                                             
         MVC   SVSSN,TLCKSSN       SAVE SSN                                     
*                                                                               
         USING TACDD,R4                                                         
         LA    R4,RDATA            R4 = A(ACTUAL RECORD)                        
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL                                                         
         JNE   PR10                                                             
         OC    TACDCHK,TACDCHK     ANY CHECK NUMBER?                            
         BZ    PR10                                                             
         MVC   SVCDCHK,TACDCHK     SAVE CHECK NUMBER                            
         MVC   SVCDDTE,TACDDTE     SAVE CHECK DATE                              
         MVC   SVCDSEQ,TACDSEQ     SEQUENCE                                     
         DROP  R4                                                               
*                                                                               
         USING TAPDD,R4                                                         
         LA    R4,RDATA            R4 = A(ACTUAL RECORD)                        
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         JNE   PR10                                                             
         MVC   SVPDADJS,TAPDADJS                                                
         MVC   SVPDEMP,TAPDEMP                                                  
         MVC   SVPDSTAT,TAPDSTAT                                                
         MVC   SVPDPST2,TAPDPST2                                                
         DROP  R4                                                               
*                                                                               
         USING TATID,R4                                                         
         LA    R4,RDATA            R4 = A(ACTUAL RECORD)                        
         STCM  R4,15,AIO                                                        
         MVI   ELCODE,TATIELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TATITYCO))                                     
         JNE   PR20                                                             
         L     R4,TGELEM                                                        
         MVC   SVSSN,TATIID        SET CORP SSN                                 
         DROP  R4                                                               
*                                                                               
PR20     CLI   RRECTY,X'01'        COPY?                                        
         BE    PR10                                                             
*&&DO                                                                           
         LA    R0,RDATA            SAVE COPY RECORD INTO AIO2 I/O AREA          
         LA    R1,2500                                                          
         L     RE,AIO2                                                          
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
         B     PR10                                                             
*&&                                                                             
*                                                                               
         BAS   RE,GETPREV          GET PREVIOUS CHECK                           
         BNE   PR10                                                             
         BAS   RE,COMPPRV          ADD CURRENT CHECK TO PREVIOUS                
         BE    PR10                                                             
*                                                                               
         BAS   RE,DOWNCHK                                                       
         BAS   RE,SENDMAIL                                                      
         B     PR10                                                             
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
* GET PREVIOUS CHECK                                                            
*                                                                               
GETPREV  NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING TLCKPD,R3                                                        
         MVI   TLCKPCD,TLCKECDQ                                                 
         MVC   TLCKESSN,SVSSN      SSN                                          
         MVI   TLCKECUR,C'U'                                                    
         TM    SVPDSTAT,TAPDSCAN   CANADIAN $?                                  
         BZ    *+8                                                              
         MVI   TLCKECUR,C'C'                                                    
         TM    SVPDPST2,TAPDPEUR   EURO $?                                      
         BZ    *+8                                                              
         MVI   TLCKECUR,C'E'                                                    
         MVC   TLCKEEMP,SVPDEMP    EMPLOYER                                     
         MVC   TLCKEDTE,SVCDDTE    CHECK DATE                                   
         XC    TLCKEDTE,=X'FFFFFF'                                              
         MVC   TLCKESEQ,SVCDSEQ    SEQUENCE                                     
         XC    TLCKESEQ,=X'FFFFFFFF'                                            
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(TLCKESEQ-TLCKPCD),KEYSAVE                                    
         JNE   NO                                                               
*                                                                               
         GOTO1 SEQ                                                              
         CLC   KEY(TLCKEDTE-TLCKPCD+1),KEYSAVE   COMPARE UP TO YEAR             
         JNE   NO                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,TAYEELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAYETCHK))                                     
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
* ADD CURRENT CHECK'S AMOUNTS TO PREVIOUS AND COMPARE TO CURRENT                
* CHECK'S YTD AMOUNTS                                                           
*                                                                               
COMPPRV  NTR1                                                                   
         LA    R4,RDATA                                                         
         STCM  R4,15,AIO                                                        
         XC    FULL,FULL                                                        
         MVI   ELCODE,TACYELQ                                                   
         GOTO1 GETL,DMCB,(3,=C'FD ')                                            
         BNE   CPRV05                                                           
         L     R4,TGELEM                                                        
         USING TACYD,R4                                                         
         MVC   FULL,TACYEARN                                                    
*                                                                               
         MVI   ELCODE,TAYEELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAYETCHK))                                     
         BNE   CPRV05                                                           
         L     R4,TGELEM                                                        
         USING TAYED,R4                                                         
         CLC   TAYEEARN,FULL                                                    
         BNE   NO                                                               
*                                                                               
CPRV05   LA    R4,RDATA            GO THROUGH LATEST CHECK ELEMS                
         MVI   ELCODE,TACWELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   COMPPRVX                                                         
         B     *+12                                                             
CPRV10   MVI   ELCODE,TACWELQ                                                   
         BAS   RE,NEXTEL                                                        
         BNE   CPRV20                                                           
         USING TACWD,R4                                                         
         ST    R4,ATACWEL                                                       
*                                                                               
         CLC   TACWUNIT,=C'CN '                                                 
         BNE   CPRV16                                                           
*                                                                               
         USING TACXD,R5                                                         
         MVC   AIO,AIO2            FOR CANADIANS:                               
         MVI   ELCODE,TACXELQ                                                   
         GOTO1 GETL,DMCB,0                                                      
         BNE   CPRV10                                                           
         L     R5,TGELEM                                                        
*                                                                               
         L     RF,TACWGST          CURRENT TACWGST + PREV GST YTD               
         LPR   RF,RF                                                            
         L     RE,TACXGSTY         = CURRENT GST YTD                            
*                                                                               
         TM    SVPDADJS,TAPDADVD   VOID                                         
         BO    CPRV12                                                           
         AR    RF,RE                                                            
         ST    RF,SVGSTY                                                        
         B     CPRV14                                                           
*                                                                               
CPRV12   SR    RE,RF                                                            
         ST    RE,SVGSTY                                                        
*                                                                               
CPRV14   L     RF,TACWGST          CURRENT TACWGST + PREV PST YTD               
         L     RE,TACXPSTY                                                      
         AR    RF,RE                                                            
         ST    RF,SVPSTY                                                        
*                                                                               
         LA    RF,RDATA                                                         
         ST    RF,AIO                                                           
         GOTO1 GETL,DMCB,0                                                      
         BNE   NO                                                               
         L     R5,TGELEM                                                        
*                                                                               
         L     RF,TACXPST         + CURRENT TACXPST + TACKGST                   
         L     RE,TACXGST                                                       
         AR    RF,RE                                                            
         L     RE,SVPSTY          = CURRENT PST YTD                             
         AR    RF,RE                                                            
         ST    RF,SVPSTY                                                        
*                                                                               
         CLC   TACXGSTY,SVGSTY                                                  
         BNE   NO                                                               
         CLC   TACXPSTY,SVPSTY                                                  
         BNE   NO                                                               
*                                                                               
         USING TACWD,R4                                                         
         USING TACYD,R5                                                         
CPRV16   L     R4,ATACWEL                                                       
         MVC   AIO,AIO2            GET YTD FROM PREVIOUS CHECK                  
         MVI   ELCODE,TACYELQ                                                   
         GOTO1 GETL,DMCB,(3,TACWUNIT)                                           
         BNE   CPRV10                                                           
         L     R5,TGELEM                                                        
*                                                                               
         L     RF,TACWTAX                                                       
         L     RE,TACYTAX                                                       
         AR    RF,RE                                                            
         ST    RF,SVTAX            TAX                                          
         L     RF,TACWFICA                                                      
         L     RE,TACYFICA                                                      
         AR    RF,RE                                                            
         ST    RF,SVFICA           FICA/SDI/RTX/INR                             
         L     RF,TACWMED                                                       
         L     RE,TACYMED                                                       
         AR    RF,RE                                                            
         ST    RF,SVMED            MED/SUI/GST                                  
*                                                                               
         LA    RF,RDATA            CONFIRM PREVIOUS + CURRENT =                 
         ST    RF,AIO              CURRENT YTD AMOUNTS                          
         MVI   ELCODE,TACYELQ                                                   
         GOTO1 GETL,DMCB,(3,TACWUNIT)                                           
         BE    CPRV18                                                           
         OC    SVTAX,SVTAX         NO TACY MEANS IT SHOULD BE 0                 
         BNZ   NO                                                               
         OC    SVFICA,SVFICA                                                    
         BNZ   NO                                                               
         OC    SVMED,SVMED                                                      
         BNZ   NO                                                               
         B     CPRV10                                                           
*                                                                               
CPRV18   L     R5,TGELEM                                                        
         CLC   TACYTAX,SVTAX       TAX                                          
         BNE   NO                                                               
*                                                                               
         MVC   FULL,SVFICA         FICA                                         
         CLC   TACYUNIT,=C'CN '    CANADIAN TAXES?                              
         BNE   *+10                                                             
         MVC   FULL,SVPSTY         PST                                          
         CLC   TACYFICA,FULL                                                    
         BNE   NO                                                               
*                                                                               
         CLC   TACYMED,SVMED       MED/SUI/GST                                  
         BNE   NO                                                               
         B     CPRV10                                                           
*                                                                               
CPRV20   LA    R4,RDATA            ACCUMULATE CURRENT CHECK CAN TAXES           
         MVI   ELCODE,TAATELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   CPRV40                                                           
         B     *+8                                                              
CPRV22   BAS   RE,NEXTEL                                                        
         BNE   CPRV30                                                           
         USING TAATD,R4                                                         
*                                                                               
         L     RF,TAATTAX                                                       
         L     RE,SVUTAX                                                        
         AR    RF,RE                                                            
         ST    RF,SVUTAX           FEDERAL TAXES USD                            
         L     RF,TAATPP                                                        
         L     RE,SVUPP                                                         
         AR    RF,RE                                                            
         ST    RF,SVUPP            PENSION PLAN USD                             
         L     RF,TAATEI                                                        
         L     RE,SVUEI                                                         
         AR    RF,RE                                                            
         ST    RF,SVUEI            EMPLOYER INSURANCE USD                       
         L     RF,TAATPIP                                                       
         L     RE,SVUPIP                                                        
         AR    RF,RE                                                            
         ST    RF,SVUPIP           PARENTAL INSURANCE USD                       
         L     RF,TAATCTAX                                                      
         L     RE,SVCTAX                                                        
         AR    RF,RE                                                            
         ST    RF,SVCTAX           FEDERAL TAXES CAD                            
         L     RF,TAATCPP                                                       
         L     RE,SVCPP                                                         
         AR    RF,RE                                                            
         ST    RF,SVCPP            PENSION PLAN CAD                             
         L     RF,TAATCEI                                                       
         L     RE,SVCEI                                                         
         AR    RF,RE                                                            
         ST    RF,SVCEI            EMPLOYER INSURANCE CAD                       
         L     RF,TAATCPIP                                                      
         L     RE,SVCPIP                                                        
         AR    RF,RE                                                            
         ST    RF,SVCPIP           PARENTAL INSURANCE CAD                       
         B     CPRV22                                                           
*                                                                               
         USING TAYED,R5            CURRENT CHECK + PREVIOUS CHECK               
CPRV30   MVC   AIO,AIO2            CANADIAN TAXES                               
         MVI   ELCODE,TAYEELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAYETCHK))                                     
         BNE   NO                                                               
         L     R5,TGELEM                                                        
*                                                                               
         L     RF,SVUTAX                                                        
         L     RE,TAYEUTAX                                                      
         AR    RF,RE                                                            
         ST    RF,SVUTAX           FEDERAL TAXES USD                            
         L     RF,SVUPP                                                         
         L     RE,TAYEUPP                                                       
         AR    RF,RE                                                            
         ST    RF,SVUPP            PENSION PLAN USD                             
         L     RF,SVUEI                                                         
         L     RE,TAYEUEI                                                       
         AR    RF,RE                                                            
         ST    RF,SVUEI            EMPLOYER INSURANCE USD                       
         L     RF,SVUPIP                                                        
         L     RE,TAYEUPIP                                                      
         AR    RF,RE                                                            
         ST    RF,SVUPIP           PARENTAL INSURANCE USD                       
         L     RF,SVCTAX                                                        
         L     RE,TAYECTAX                                                      
         AR    RF,RE                                                            
         ST    RF,SVCTAX           FEDERAL TAXES CAD                            
         L     RF,SVCPP                                                         
         L     RE,TAYECPP                                                       
         AR    RF,RE                                                            
         ST    RF,SVCPP            PENSION PLAN CAD                             
         L     RF,SVCEI                                                         
         L     RE,TAYECEI                                                       
         AR    RF,RE                                                            
         ST    RF,SVCEI            EMPLOYER INSURANCE CAD                       
         L     RF,SVCPIP                                                        
         L     RE,TAYECPIP                                                      
         AR    RF,RE                                                            
         ST    RF,SVCPIP           PARENTAL INSURANCE CAD                       
*                                                                               
         LA    RF,RDATA                                                         
         ST    RF,AIO                                                           
         MVI   ELCODE,TAYEELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAYETCHK))                                     
         BNE   NO                                                               
         L     R5,TGELEM                                                        
*                                                                               
         CLC   TAYEUTAX,SVUTAX     CONFIRM PREVIOUS + CURRENT =                 
         BNE   NO                  CURRENT YTD AMOUNTS                          
         CLC   TAYEUPP,SVUPP                                                    
         BNE   NO                                                               
         CLC   TAYEUEI,SVUEI                                                    
         BNE   NO                                                               
         CLC   TAYEUPIP,SVUPIP                                                  
         BNE   NO                                                               
         CLC   TAYECTAX,SVCTAX                                                  
         BNE   NO                                                               
         CLC   TAYECPP,SVCPP                                                    
         BNE   NO                                                               
         CLC   TAYECEI,SVCEI                                                    
         BNE   NO                                                               
         CLC   TAYECPIP,SVCPIP                                                  
         BNE   NO                                                               
*                                                                               
CPRV40   LA    R4,RDATA            GET CURRENT TAXABLE SUI                      
         MVI   ELCODE,TACYELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   COMPPRVX                                                         
         B     *+8                                                              
CPRV42   BAS   RE,NEXTEL                                                        
         BNE   COMPPRVX                                                         
         USING TACYD,R4                                                         
*                                                                               
         MVC   TGCTRY,=C'CA'       CANADIAN                                     
         GOTO1 TAXVAL,DMCB,(X'FF',TACYUNIT) UNIT                                
         BE    CPRV42                                                           
*                                                                               
         CLI   TACYLEN,TACYLN4Q                                                 
         BL    CPRV42                                                           
*                                                                               
         MVC   TGTHREE,TACYUNIT                                                 
         MVC   SVTSUI,TACYTSUI     TAXABLE SUI                                  
         MVC   SVYSUI,TACYYSUI     YTD SUI                                      
         DROP  R4                                                               
*                                                                               
         USING TACYD,R5                                                         
         MVC   AIO,AIO2                                                         
         MVI   ELCODE,TACYELQ                                                   
         GOTO1 GETL,DMCB,(3,TGTHREE)                                            
         BNE   NO                                                               
         L     R5,TGELEM                                                        
*                                                                               
         L     RF,SVTSUI           CURRENT TAXABLE SUI + PREV YTD SUI           
         L     RE,TACYYSUI         SHOULD EQUAL CURRENT YTD SUI                 
         AR    RF,RE                                                            
         C     RF,SVYSUI                                                        
         BNE   NO                                                               
         B     CPRV42                                                           
*                                                                               
COMPPRVX B     YES                                                              
*                                                                               
ATACWEL  DS    F                                                                
*                                                                               
SVTAX    DS    F                   TAXES                                        
SVFICA   DS    F                   FICA                                         
SVMED    DS    F                   MEDICARE                                     
SVUTAX   DS    F                   TAX USD (CANADIAN TAXES)                     
SVUPP    DS    F                   PP USD (CANADIAN TAXES)                      
SVUEI    DS    F                   EI USD (CANADIAN TAXES)                      
SVUPIP   DS    F                   PIP USD (CANADIAN TAXES)                     
SVCTAX   DS    F                   TAX CAD (CANADIAN TAXES)                     
SVCPP    DS    F                   PP CAD (CANADIAN TAXES)                      
SVCEI    DS    F                   EI CAD (CANADIAN TAXES)                      
SVCPIP   DS    F                   PIP CAD (CANADIAN TAXES)                     
SVTSUI   DS    F                   TAXABLE SUI                                  
SVYSUI   DS    F                   YTD SUI                                      
SVGSTY   DS    F                   GST                                          
SVPSTY   DS    F                   PST YTD                                      
*                                                                               
GETELEM  NTR1                                                                   
         XC    ELEM,ELEM                                                        
         MVC   AIO,0(R1)           AIO                                          
         L     RF,4(R1)            UNIT                                         
         MVC   TGTHREE,0(RF)                                                    
         MVC   ELCODE,11(R1)       ELEMENT ID                                   
*                                                                               
         GOTO1 GETL,DMCB,(3,TGTHREE)                                            
         BNE   NO                                                               
*                                                                               
         L     RF,TGELEM                                                        
         ZIC   R1,1(RF)                                                         
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(RF)                                                    
         B     YES                                                              
*                                                                               
DOWNCHK  NTR1                                                                   
         USING RCVD,R2                                                          
         USING TLCKD,R4                                                         
*                                                                               
         LA    R4,RDATA            R4 = A(ACTUAL RECORD)                        
*                                                                               
         MVC   PRNTACT,=C'ADD'                                                  
         CLI   RRECTY,X'03'        ADD?                                         
         BE    *+10                                                             
         MVC   PRNTACT,=C'CHA'                                                  
*                                                                               
*        GOTO1 TIMECON,DMCB,RTIME,RDATE,(8,PRNTTIME)                            
         ICM   RF,15,RTIME                                                      
         TM    RTIME,X'80'                                                      
         JNO   *+12                                                             
         SLL   RF,1                                                             
         SRL   RF,5                                                             
         XC    DUB,DUB                                                          
         STCM  RF,15,DUB+4                                                      
         OI    DUB+7,X'0C'                                                      
         UNPK  WORK(6),DUB+4(4)                                                 
         OI    WORK+5,X'F0'                                                     
         MVC   PRNTTIME(2),WORK                                                 
         MVI   PRNTTIME+2,C':'                                                  
         MVC   PRNTTIME+3(2),WORK+2                                             
         MVI   PRNTTIME+5,C':'                                                  
         MVC   PRNTTIME+6(2),WORK+4                                             
*                                                                               
         MVC   PRNTAGY,TLCKAGY                                                  
*                                                                               
         GOTO1 TINVCON,DMCB,TLCKINV,WORK,DATCON                                 
         MVC   PRNTINV,WORK                                                     
*                                                                               
         MVC   PRNTSSN,TLCKSSN                                                  
         MVC   PRNTNUM,SVCDCHK                                                  
         GOTO1 DATCON,DMCB,(1,SVCDDTE),(8,PRNTDATE)                             
*                                                                               
         MVC   PRNTTYPE,SPACES                                                  
*                                                                               
         USING TAPDD,R4                                                         
         LA    R4,RDATA            R4 = A(ACTUAL RECORD)                        
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         JNE   DWNCHK10                                                         
         TM    SVPDADJS,TAPDADVD   VOID                                         
         BZ    *+10                                                             
         MVC   PRNTTYPE,=CL12'VOID'                                             
         TM    SVPDADJS,TAPDADRI   REISSUE                                      
         BZ    *+10                                                             
         MVC   PRNTTYPE,=CL12'REISSUE'                                          
         TM    SVPDADJS,TAPDADIS   ISSUE                                        
         BZ    *+10                                                             
         MVC   PRNTTYPE,=CL12'ISSUE'                                            
         TM    SVPDADJS,TAPDADTR   TAX REFUND                                   
         BZ    *+10                                                             
         MVC   PRNTTYPE,=CL12'TAX REFUND'                                       
         TM    SVPDADJS,TAPDADTT   TAX TRANSFER                                 
         BZ    *+10                                                             
         MVC   PRNTTYPE,=CL12'TAX TRANSFER'                                     
         TM    SVPDADJS,TAPDADST   SSN REFUND                                   
         BZ    *+10                                                             
         MVC   PRNTTYPE,=CL12'SSN TRANSFER'                                     
*                                                                               
DWNCHK10 GOTO1 OUTPDOWN,DMCB,(C'T',PRNTACT),L'PRNTACT                           
         GOTO1 OUTPDOWN,DMCB,(C'T',PRNTTIME),L'PRNTTIME                         
         GOTO1 OUTPDOWN,DMCB,(C'T',PRNTAGY),L'PRNTAGY                           
         GOTO1 OUTPDOWN,DMCB,(C'T',PRNTINV),L'PRNTINV                           
         GOTO1 OUTPDOWN,DMCB,(C'T',PRNTSSN),L'PRNTSSN                           
         GOTO1 OUTPDOWN,DMCB,(C'T',PRNTDATE),L'PRNTDATE                         
         GOTO1 OUTPDOWN,DMCB,(C'T',PRNTNUM),L'PRNTNUM                           
         GOTO1 OUTPDOWN,DMCB,(C'T',PRNTTYPE),L'PRNTTYPE                         
         GOTO1 OUTPDOWN,DMCB,(C'T',PRNTSTAT),L'PRNTSTAT                         
         BAS   RE,EOLDOWN                                                       
         B     XIT                                                              
*                                                                               
         DROP  R4                                                               
***********************************************************************         
*        INITIALIZE DOWNLOAD SETTINGS                                 *         
*        ON ENTRY ... R5=A(DOWNLOAD BLOCK)                            *         
***********************************************************************         
                                                                                
         USING DLCBD,R5                                                         
INITDOWN NTR1                                                                   
         XC    DLCBD(DLCBXLX),DLCBD                                             
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,PRTDOWN          A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         MVC   P,SPACES                                                         
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         OI    DLCBFLG1,DLCBFXTN                                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
***********************************************************************         
*        USER SUPPLIED PRINT ROUTINE                                  *         
***********************************************************************         
                                                                                
PRTDOWN  NTR1                                                                   
         MVI   LINE,1              PREVENT PAGE BREAK                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
***********************************************************************         
*        OUTPUT DOWNLOAD ENTRY                                        *         
*        ON ENTRY ... P1, B0    = TYPE TO PASS                        *         
*                         B1-B3 = ADDRESS OF DATA                     *         
*                     P2        = LENGTH                              *         
***********************************************************************         
                                                                                
OUTPDOWN NTR1                                                                   
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBTYP,0(R1)                                                    
         OI    DLCBFLG1,DLCBFXFL                                                
                                                                                
         L     RF,0(R1)            RF=A(DOWNLOAD FIELD)                         
         L     RE,4(R1)            RE=L'DOWNLOAD FIELD                          
                                                                                
         LR    R1,RF                                                            
         AR    R1,RE                                                            
OPD10    SHI   R1,1                                                             
         CLI   0(R1),C' '                                                       
         JNE   OPD20                                                            
         BCT   RE,OPD10                                                         
         LHI   RE,1                                                             
OPD20    STC   RE,DLCBLEN                                                       
                                                                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLX(0),0(RF)                                                 
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
***********************************************************************         
*        FINISH LINE OF DOWNLOAD OUPUT                                *         
***********************************************************************         
                                                                                
EOLDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOL      END OF LINE                                 
         GOTO1 =V(DLFLD),DLCBD      (DON'T USE RF, BASR RE,RF BELOW)            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        END DOWNLOAD                                                 *         
***********************************************************************         
                                                                                
ENDDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOR      END OF REPORT                               
         GOTO1 =V(DLFLD),DLCBD      LAST FOR REPORT                             
         J     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
*              HEADLINE HOOK (HEADHOOK)                                         
         SPACE 1                                                                
HOOK     NTR1                                                                   
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT LINE OF OUTPUT                                  
         SPACE 1                                                                
PRINTIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         LTORG                                                                  
*---------------------------------------------------------------------          
* E-MAIL ERROR REPORT                                                           
*---------------------------------------------------------------------          
SENDMAIL NTR1  BASE=*,LABEL=*                                                   
         MVC   SUBJDSCS,SVCDCHK                                                 
         TM    PRGSTAT,TESTSYS                                                  
         BZ    *+10                                                             
         MVC   SUBJSYS,=C'(TST)'                                                
         TM    PRGSTAT,CSCSYS                                                   
         BZ    *+10                                                             
         MVC   SUBJSYS,=C'(CSC)'                                                
         TM    PRGSTAT,FQASYS                                                   
         BZ    *+10                                                             
         MVC   SUBJSYS,=C'(FQA)'                                                
*                                                                               
         MVC   BODYSSN,SVSSN                                                    
         GOTO1 SSNPACK,DMCB,SVSSN,BODYPID                                       
*                                                                               
         TM    SVPDADJS,TAPDADVD   VOID                                         
         BZ    *+10                                                             
         MVC   BODYTYPE,=CL12'VOID'                                             
         TM    SVPDADJS,TAPDADRI   REISSUE                                      
         BZ    *+10                                                             
         MVC   BODYTYPE,=CL12'REISSUE'                                          
         TM    SVPDADJS,TAPDADIS   ISSUE                                        
         BZ    *+10                                                             
         MVC   BODYTYPE,=CL12'ISSUE'                                            
         TM    SVPDADJS,TAPDADTR   TAX REFUND                                   
         BZ    *+10                                                             
         MVC   BODYTYPE,=CL12'TAX REFUND'                                       
         TM    SVPDADJS,TAPDADTT   TAX TRANSFER                                 
         BZ    *+10                                                             
         MVC   BODYTYPE,=CL12'TAX TRANSFER'                                     
         TM    SVPDADJS,TAPDADST   SSN REFUND                                   
         BZ    *+10                                                             
         MVC   BODYTYPE,=CL12'SSN TRANSFER'                                     
*                                                                               
         GOTOR =V(SMTP),DMCB,('SMTPAINI',JESMAIL)                               
         LA    R4,TOWHO                                                         
         LA    R3,SUBJDSC                                                       
*                                                                               
         GOTOR =V(SMTP),DMCB,('SMTPAPRS',(R4)),(L'SUBJDSC,(R3))                 
*                                                                               
         GOTOR =V(SMTP),DMCB,('SMTPAPTL',BODYLIN1)                              
         GOTOR (RF),DMCB,('SMTPASND',0)                                         
         GOTOR (RF),DMCB,('SMTPAEND',0) DETACH SMTP                             
SENDMX   XIT1                                                                   
*                                                                               
JESMAIL  DC    CL8'JESMAIL '                                                    
*                                                                               
TOWHO    DC    C'US-TALENT_PRODUCT_TEAM,US-TALENT_MF_DEV_TEAM:'                 
*                                                                               
SUBJDSC  DC    0CL80                                                            
         DC    C'** YTDAUDIT ERROR CHECK#: '                                    
SUBJDSCS DC    C'        '                                                      
         DC    C' '                                                             
SUBJSYS  DC    C'     '                                                         
         DC    CL(L'SUBJDSC-(*-SUBJDSC))' '     SPARE SPACES                    
*                                                                               
BODYLIN1 DC    0CL80                                                            
         DC    C'SSN:'                                                          
BODYSSN  DC    C'         '                                                     
         DC    C'  '                                                            
         DC    C'PID:'                                                          
BODYPID  DC    C'      '                                                        
         DC    C'  '                                                            
         DC    C'TYPE:'                                                         
BODYTYPE DC    C'            '                                                  
         DC    CL(L'BODYLIN1-(*-BODYLIN1))' '   SPARE SPACES                    
                                                                                
*              ERROR ROUTINES                                                   
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 ERREX                                                            
         SPACE 2                                                                
*              CONDITION CODE & EXIT ROUTINE                                    
         SPACE 1                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              STANDARD ROUTINES                                                
         SPACE 1                                                                
         GETEL  R4,DATADISP,ELCODE                                              
         SPACE 2                                                                
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,                                 X        
               MACRF=GM,EODAD=XIT                                               
SORTCARD DC    CL80'SORT FIELDS=(1,27,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=200'                                   
REASDATA DS    0C                                                               
         DC    X'8B',CL23'FTRACK DELETE'                                        
         DC    X'44',CL23'INVOICE CANCEL/REOPEN'                                
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECIFICATIONS                                            
         SPACE 1                                                                
MYSPECS  SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,117,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
*                                                                               
         SSPEC H1,46,C'YTD AUDIT'                                               
*                                                                               
         SSPEC H7,01,C'ACT'                                                     
         SSPEC H8,01,C'---'                                                     
         SSPEC H7,06,C'  TIME'                                                  
         SSPEC H8,06,C'--------'                                                
         SSPEC H7,16,C'AGENCY'                                                  
         SSPEC H8,16,C'------'                                                  
         SSPEC H7,24,C'INVOICE'                                                 
         SSPEC H8,24,C'-------'                                                 
         SSPEC H7,32,C'   SSN'                                                  
         SSPEC H8,32,C'---------'                                               
         SSPEC H7,43,C'  DATE'                                                  
         SSPEC H8,43,C'--------'                                                
         SSPEC H7,54,C'CHECK #'                                                 
         SSPEC H8,54,C'--------'                                                
         SSPEC H7,64,C'    TYPE'                                                
         SSPEC H8,64,C'------------'                                            
         SSPEC H7,78,C'   STATUS'                                               
         SSPEC H8,64,C'------------'                                            
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
RECD     DSECT                                                                  
COPYCNT  DS    X                                                                
TACOUNT  DS    X                                                                
*                                                                               
SVSSN    DS    CL9                 SSN                                          
SVCDCHK  DS    CL8                 CHECK NUMBER                                 
SVCDDTE  DS    XL3                 CHECK DATE                                   
SVCDSEQ  DS    XL4                 SEQUENCE                                     
SVPDADJS DS    XL1                                                              
SVPDEMP  DS    CL3                 EMPLOYER                                     
SVPDSTAT DS    XL1                 STATUS                                       
SVPDPST2 DS    XL1                 STATUS                                       
*                                                                               
PRNTACT  DS    CL3                 ACTION                                       
PRNTTIME DS    CL8                 TIME                                         
PRNTAGY  DS    CL6                 AGENCY                                       
PRNTINV  DS    CL6                 INVOICE                                      
PRNTSSN  DS    CL9                 S/S NUMBER                                   
PRNTDATE DS    CL8                 DATE                                         
PRNTNUM  DS    CL8                 CHECK NUMBER                                 
PRNTTYPE DS    CL12                TYPE                                         
PRNTSTAT DS    CL10                STATUS                                       
*                                                                               
         DS    0F                                                               
*                                                                               
DLCB     DS    CL(DLCBXLX)         DOWNLOAD BLOCK                               
*                                                                               
SORTREC  DS    XL200                                                            
RCVREC   DS    0H                                                               
RLEN     DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
RHEAD    DS    XL24                                                             
RDATA    DS    2500C                                                            
         SPACE 2                                                                
*              DSECT TO COVER REASON TABLE                                      
         SPACE 1                                                                
REASD    DSECT                                                                  
REASSCR  DS    X                                                                
REASPGM  DS    CL23                                                             
REASDLQ  EQU   *-REASD                                                          
         SPACE 2                                                                
         EJECT                                                                  
         ORG                                                                    
         EJECT                                                                  
RCVD     DSECT                                                                  
       ++INCLUDE DMRCVRHDR                                                      
         SPACE 1                                                                
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPEAD                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDPERVAL                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSMTPD                                                        
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014TAREP74   06/19/15'                                      
         END                                                                    
