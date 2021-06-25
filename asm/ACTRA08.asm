*          DATA SET ACTRA08    AT LEVEL 105 AS OF 02/15/19                      
*PHASE T62208B                                                                  
*INCLUDE SPFMTINO                                                               
*INCLUDE PPFMTINO                                                               
         SPACE                                                                  
         TITLE '(T62208)  BILLING TRANSFER - TRACE OVERLAY'                     
T62208   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MAXPSTLQ,T62208**,RR=RE,R9,R8                                    
         LR    R3,RC                                                            
*                                                                               
         USING TWAD,R5            R5=A(TWA)                                     
         USING SAVAREA,R6         R6=A(SAVE AREA)                               
         USING WORKD,R7           R7=A(GLOBAL WORKING STORAGE)                  
         USING LOCALD,RC                                                        
         L     RC,APALOCAL        RC=A(LOCAL WORKING STORAGE)                   
         ST    RB,APBASE1                                                       
         ST    RB,APNTRYA                                                       
         ST    RD,APWORKA                                                       
         ST    RE,APRELO           RELOCATION FACTOR                            
         ST    R3,APOSTING         R3=A(POSTINGS)                               
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         LA    RE,BDISYSH                                                       
         STCM  RE,15,APCURSOR                                                   
*                                                                               
         CLI   APMODE,APMVALK        VALIDATE KEY                               
         BE    VALKEY                                                           
         CLI   APMODE,APMDISR        DISPLAY RECORD                             
         BE    DISREC                                                           
         CLI   APMODE,APMDISK        DISPLAY KEY (SELECT)                       
         BE    DISKEY                                                           
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
YES      CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
NO       LTR   RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
*=====================================*                                         
* VALKEY - VALIDATES KEY              *                                         
*=====================================*                                         
*                                                                               
VALKEY   GOTO1 AVALSYS,BDISYSH    VALIDATE SYSTEM                               
         BNE   VALKX                                                            
*                                                                               
         LA    R1,BDIMEDH                                                       
         STCM  R1,15,APCURSOR                                                   
         GOTO1 AFVAL,BDIMEDH                                                    
         BNE   VALKERR2            INVALID                                      
         GOTO1 AVALMED,BDIMEDH     VALIDATE MEDIA                               
         BNE   VALKX                                                            
*                                                                               
         LA    R1,BDICLTH                                                       
         STCM  R1,15,APCURSOR                                                   
         GOTO1 AFVAL,BDICLTH                                                    
         BNE   VALKERR2           INVALID                                       
         GOTO1 AVALCLT,BDICLTH    VALIDATE CLIENT                               
         BNE   VALKX                                                            
*                                                                               
         LA    R1,BDIPRDH                                                       
         STCM  R1,15,APCURSOR                                                   
         GOTO1 AFVAL,BDIPRDH      AUTO EXIT IF MISSING                          
         BNE   VALKERR2           INVALID                                       
         GOTO1 AVALPRD,BDIPRDH    VALIDATE PRODUCT                              
         BNE   VALKX                                                            
*                                                                               
         LA    R1,BDIESTH                                                       
         STCM  R1,15,APCURSOR                                                   
         GOTO1 AFVAL,BDIESTH      AUTO EXIT IF MISSING                          
         BNE   VALKERR2           INVALID                                       
         GOTO1 AVALEST,BDIESTH    VALIDATE ESTIMATE                             
         BNE   VALKX                                                            
*                                                                               
         LA    R1,BDIRDTH                                                       
         STCM  R1,15,APCURSOR                                                   
         GOTO1 AFVAL,BDIRDTH                                                    
         BNE   VALKERR2           INVALID                                       
         GOTO1 AVALRDT,BDIRDTH    VALIDATE RUN DATE                             
         BNE   VALKX                                                            
         CLI   QSYS,C'P'                                                        
         BE    *+8                                                              
         BAS   RE,GETBMN          CALCULATE BILL MONTH OFF OF RUN DATE          
*                                                                               
         LA    R1,BDISDTH                                                       
         STCM  R1,15,APCURSOR                                                   
         GOTO1 AFVAL,BDISDTH                                                    
         BNE   VALKERR2           INVALID                                       
         GOTO1 AVALSRVM,BDISDTH   VALIDATE MONTH OF SERVICE                     
         BNE   VALKX                                                            
*                                                                               
         LA    R1,BDIINVH                                                       
         STCM  R1,15,APCURSOR                                                   
         GOTO1 AFVAL,BDIINVH                                                    
         BNE   VALKERR2                                                         
         GOTO1 AVALINV,BDIINVH    VALIDATE INVOICE NUMBER                       
         BNE   VALKX                                                            
*                                                                               
         LA    R1,BDIIDTH                                                       
         STCM  R1,15,APCURSOR                                                   
         CLI   BDIIDTH+5,0        ANY INPUT                                     
         BE    VALK30                                                           
         GOTO1 AVALIDT,BDIIDTH    VALIDATE INVOICE DATE                         
         BNE   VALKX                                                            
         CLI   BIDT+2,0           ANY DAY?                                      
         BE    VALKERRD           DATE ERROR                                    
*                                                                               
VALK30   XC    BTDT,BTDT                                                        
         LA    R2,BDITDTH         TRANSFER DATE                                 
         STCM  R2,15,APCURSOR                                                   
         CLI   5(R2),0                                                          
         BE    VALK40                                                           
         GOTO1 VDATVAL,APPARM,(0,8(R2)),APWORK                                  
         OC    APPARM,APPARM                                                    
         BZ    VALKERRD                                                         
         GOTO1 VDATCON,APPARM,(0,APWORK),(2,BTDT)                               
*                                                                               
VALK40   XC    APRECKEY,APRECKEY                                                
         BAS   RE,SETKEY          SET APPRECKEY                                 
         GOTO1 ASETFILE           SET ACC FILE & LIMIT ACCESS                   
         BE    VALK45                                                           
         LA    R1,BDISYSH                                                       
         ST    R1,APCURSOR                                                      
         B     VALKX                                                            
*                                                                               
VALK45   MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   IOKEY,APRECKEY                                                   
         GOTO1 AMYIO,APPARM,IOHI+IO1,=C'D'                                      
         CLI   MYIOERR,0                                                        
         BNE   VALKX                                                            
         LA    RE,25              LENGTH OF PRINT KEY                           
         CLI   QSYS,C'P'                                                        
         BE    *+8                                                              
         LA    RE,13              LENGTH OF SPOT/NET                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         BNE   VALKERR                                                          
         GOTO1 AMYIO,APPARM,IOGET+IO1,=C'F'                                     
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,CHKTRUE         MAKE SURE NOT TRUE AOR BILL                   
         BNE   VALKERR3                                                         
         BAS   RE,CHKRET          CHECK TYPE OF RETAIL BILL                     
         BNE   VALKERR5                                                         
         BAS   RE,CHKINV          CHECK INVOICE DATE MATCHES                    
         BNE   VALKERR                                                          
         BAS   RE,CHKTRA          CHECK TRANSFER DATE MATCHES                   
         BNE   VALKERR                                                          
         BAS   RE,CHKMED          IF NET - CHECK STATION TYPE                   
         BNE   VALKERR                                                          
         OI    APINDS,APIOKDIS    OKAY TO DISPLAY                               
VALKX    B     EXIT                                                             
         SPACE                                                                  
VALKERR  MVC   FVMSGNO,=AL2(FVFERNF) RECORD NOT FOUND                           
         LA    R1,BDISYSH                                                       
         STCM  R1,15,APCURSOR     PT TO SYSTEM                                  
         B     VALKX                                                            
VALKERR2 MVC   FVMSGNO,=AL2(FVIREQ) INPUT REQUIRED                              
         B     VALKX                                                            
VALKERRD MVC   FVMSGNO,=AL2(FVIDATE)                                            
         B     VALKX                                                            
VALKERR3 MVC   FVMSGNO,=AL2(FVITAOR) TRUE AOR NOT FOR DISPLAY                   
         LA    R1,BDISYSH                                                       
         STCM  R1,15,APCURSOR     PT TO SYSTEM                                  
         B     VALKX                                                            
VALKERR5 MVC   FVMSGNO,=AL2(FVINORET) RETAIL NOT FOR DISPLAY                    
         LA    R1,BDISYSH                                                       
         STCM  R1,15,APCURSOR     PT TO SYSTEM                                  
         B     VALKX                                                            
         EJECT                                                                  
*------------------------------------*                                          
* GETBMN - CALCULATES BILL MONTH     *                                          
*    EXIT - BILMONTH SET             *                                          
*------------------------------------*                                          
*                                                                               
GETBMN   NTR1                                                                   
         MVI   BILMONTH,0                                                       
         OC    BRDT,BRDT                                                        
         BZ    GETBMNX                                                          
         GOTO1 VDATCON,APPARM,(3,BRDT),(X'20',APWORK)                           
         PACK  APDUB,APWORK(2)      GET DECADE                                  
         SR    RE,RE                                                            
         CVB   RF,APDUB                                                         
         D     RE,=F'10'                                                        
         SLL   RE,4                                                             
         STC   RE,APBYTE                                                        
*                                                                               
         PACK  APDUB,APWORK+2(2)  GET MONTH                                     
         CVB   RF,APDUB                                                         
         STC   RF,APFULL                                                        
*                                                                               
         OC    APBYTE,APFULL                                                    
         MVC   BILMONTH,APBYTE                                                  
GETBMNX  XIT1                                                                   
         EJECT                                                                  
*----------------------------------------*                                      
* SETKEY  - SETS APRECKEY GIVEN BVALS    *                                      
*----------------------------------------*                                      
*                                                                               
SETKEY   NTR1                                                                   
         LA    R1,APRECKEY                                                      
         USING BILLRECD,R1                                                      
         CLI   QSYS,C'P'                                                        
         BE    SETKEY10                                                         
*                                                                               
         MVC   BKEYAM,BAGYMD                                                    
         MVC   BKEYCLT,BCLT                                                     
         MVC   BKEYPRD,QPRD                                                     
         MVC   BKEYEST,BEST+1                                                   
         MVC   BKEYMBIL,BILMONTH  BILL MONTH (Y/M)                              
         MVC   BKEYYSRV(2),BSRVM  SERVICE Y/M                                   
         MVC   BKEYINV,BINV       BILL NUMBER                                   
         B     SETKEYX                                                          
*                                                                               
SETKEY10 MVC   PBILKAGY,TWAAGY    AGENCY ID                                     
         OC    QALPH,QALPH                                                      
         BZ    *+10                                                             
         MVC   PBILKAGY,QALPH     AGENCY FOR MEDIA SPLIT FILES                  
         MVC   PBILKMED,QMED                                                    
         MVI   PBILKRCD,X'08'     RECORD TYPE                                   
         MVC   PBILKCLT,QCLT                                                    
         MVC   PBILKPRD,QPRD                                                    
         MVC   PBILKEST,BEST                                                    
         MVC   PBILKMOS,BSRVM     Y/M OF SERVICE DATE                           
         MVC   PBILKBMN,BRDT      Y/M OF RUN DATE                               
         MVC   PBILKBNO,BINV      BILL NUMBER                                   
*                                                                               
SETKEYX  B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
*----------------------------------------*                                      
* CHKTRUE - CHECKS IF BILL IS TRUE AOR   *                                      
*    EXIT - IF SO - SETS CC NOT EQUAL    *                                      
*----------------------------------------*                                      
*                                                                               
CHKTRUE  NTR1                                                                   
         L     R1,AIOAREA1                                                      
         USING BILLRECD,R1                                                      
         CLI   QSYS,C'P'                                                        
         BE    CHKTRU10                                                         
*                                                                               
         TM    BILSTAT,X'20'                                                    
         BO    NO                                                               
         B     YES                                                              
*                                                                               
CHKTRU10 TM    PBILCMSW,X'20'                                                   
         BO    NO                                                               
         B     YES                                                              
         DROP  R1                                                               
         EJECT                                                                  
*==============================================================*                
* CHKRET - CHECKS IF RETAIL BILL VALID FOR POSTING DISPLAY     *                
*==============================================================*                
*                                                                               
CHKRET   NTR1                                                                   
         L     R2,AIOAREA1                                                      
         USING BILLRECD,R2                                                      
         CLI   QSYS,C'P'                                                        
         BE    CHKRET30                                                         
*                                                                               
         CLI   BRETAIL,0          IF A RETAIL BILL                              
         BE    YES                                                              
         CLI   BRETAIL,X'81'      RETAIL CORP CONTROL ARE NEVER TRANS           
         BE    NO                 SKIP BILL                                     
         ZAP   BGRSP,BGRSP                                                      
         BNZ   YES                                                              
         ZAP   BNETP,BNETP                                                      
         BNZ   YES                                                              
         ZAP   BACTP,BACTP                                                      
         BNZ   YES                                                              
         OC    BVATAMT,BVATAMT                                                  
         BNZ   YES                                                              
         B     NO                                                               
*                                                                               
CHKRET30 CLI   PBRETAIL,0                                                       
         BE    YES                                                              
         CLI   PBRETAIL,X'81'     RETAIL CORP CONTROL ARE NEVER TRANS           
         BE    NO                                                               
         CP    PBILLGRS,=P'0'     SKIP ZERO BILLS                               
         BNE   YES                                                              
         CP    PBILLNET,=P'0'                                                   
         BNE   YES                                                              
         CP    PBILLBIL,=P'0'                                                   
         BNE   YES                                                              
         CP    PBILLRCV,=P'0'                                                   
         BNE   YES                                                              
         LA    RE,PBILLEL                                                       
         SR    R0,R0                                                            
CHKRET40 CLI   0(RE),0                                                          
         BE    NO                 NO AMOUNTS                                    
         CLI   0(RE),X'0A'                                                      
         BE    CHKRET50                                                         
         CLI   0(RE),X'84'                                                      
         BE    CHKRET60                                                         
CHKRET45 ICM   R0,1,1(RE)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    RE,R0                                                            
         B     CHKRET40                                                         
*                                                                               
         USING PBILVEL,RE                                                       
CHKRET50 OC    PBILLVAT,PBILLVAT                                                
         BNZ   YES                NOT ALL AMOUNTS ZERO                          
         B     CHKRET45           KEEP ON CHECKING                              
         DROP  RE                                                               
*                                                                               
         USING PBLPSTEL,RE                                                      
CHKRET60 OC    PBLPVAMT,PBLPVAMT                                                
         BNZ   YES                 VAT AMOUNT NOT ZERO                          
         OC    PBLPVBAS,PBLPVBAS   VAT BASIS AMOUNT NOT ZERO                    
         BNZ   YES                                                              
         B     CHKRET45                                                         
         DROP  R2,RE                                                            
         EJECT                                                                  
*----------------------------------------*                                      
* CHKINV - CHECKS IF INVOICE DATE MATCHES*                                      
*----------------------------------------*                                      
*                                                                               
CHKINV   NTR1                                                                   
         CLI   BDIIDTH+5,0                                                      
         BE    YES                IF NO INVOICE DATE OKAY                       
         L     R1,AIOAREA1                                                      
         USING BILLRECD,R1                                                      
         CLI   QSYS,C'P'                                                        
         BE    CHKINV10                                                         
*                                                                               
         CLC   QIDT(2),BQDATE                                                   
         BNE   NO                                                               
         CLC   QIDT+2(2),BQDATE+2                                               
         BNE   NO                                                               
         CLI   BIDT+2,0                                                         
         BE    YES                                                              
         CLC   QIDT+4(2),BQDATE+4                                               
         BE    YES                                                              
         B     NO                                                               
*                                                                               
CHKINV10 CLC   QIDT(2),PBILINVD                                                 
         BNE   NO                                                               
         CLC   QIDT+2(2),PBILINVD+2                                             
         BNE   NO                                                               
         CLI   BIDT+2,0                                                         
         BE    YES                                                              
         CLC   QIDT+4(2),PBILINVD+4                                             
         BE    YES                                                              
         B     NO                                                               
         DROP  R1                                                               
         EJECT                                                                  
*----------------------------------------*                                      
* CHKTRA - CHECKS IF TRANSFER DATE MATCH *                                      
*----------------------------------------*                                      
*                                                                               
CHKTRA   NTR1                                                                   
         CLI   BDITDTH+5,0                                                      
         BE    YES                IF NO INVOICE DATE OKAY                       
         L     R1,AIOAREA1                                                      
         USING BILLRECD,R1                                                      
         CLI   QSYS,C'P'                                                        
         BE    CHKTRA10                                                         
         CLC   BILPOST,BTDT       DATES MATCH                                   
         BNE   NO                                                               
         B     YES                                                              
*                                                                               
CHKTRA10 CLC   PBILPOST,BTDT      TRANSFER DATES MATCH                          
         BNE   NO                                                               
         B     YES                                                              
         DROP  R1                                                               
         EJECT                                                                  
*----------------------------------------*                                      
* CHKMED - CHECKS MEDIA TYPE FOR NET BILLS                                      
*----------------------------------------*                                      
*                                                                               
CHKMED   NTR1                                                                   
         CLI   QSYS,C'N'                                                        
         BNE   YES                                                              
         L     R1,AIOAREA1                                                      
         USING BILLRECD,R1                                                      
         MVC   APBYTE,BLMED                                                     
         OC    APBYTE,SPACES                                                    
         CLI   APBYTE,C' '                                                      
         BNE   *+8                                                              
         MVI   APBYTE,C'N'        NO STATION TYPE =N                            
         CLC   QMED,APBYTE                                                      
         BE    YES                                                              
         B     NO                                                               
         EJECT                                                                  
*===========================================================*                   
* DISREC - DISPLAYS RECORD                                  *                   
*          SETS VALUES AND CALLS ACPOSTER                   *                   
*  IF SEL CODE = R  READ POST MAINT REC                     *                   
*  IF SEL CODE = S & UNTRANS BILL READS POST MAINT RECORDS  *                   
*  IF SEL CODE = S & TRANS BILL READS POST DETAIL RECORDS   *                   
*===========================================================*                   
*                                                                               
DISREC   LA    RF,MAXPNUM         CLEAR OUT AREA                                
         L     RE,APSTTBL         POSTING ACCOUNTS                              
DISR2    XC    0(ACPRTNL,RE),0(RE)                                              
         LA    RE,ACPRTNL(RE)                                                   
         BCT   RF,DISR2                                                         
         L     RE,APOSTING        CLEAR POSTINGS AREA                           
         LA    RF,MAXPSTLQ                                                      
         XCEFL                                                                  
         GOTO1 =A(CLEARF),APPARM,(RC),(1,BDIL1H),BDIL14H,RR=APRELO              
         BAS   RE,DSPDAT          DISPLAY INTERFACE & INV DATE                  
         BAS   RE,CHKDISP         DISPLAYING POST DETAIL RECORD?                
         BNE   *+12                                                             
         BAS   RE,DSPPOST         DISPLAY POSTING DETAIL RECORDS                
         B     DISRX                                                            
*                                                                               
         MVI   LINENUM,1                                                        
         XC    BDIMSG,BDIMSG                                                    
         XC    BDIMSG2,BDIMSG2                                                  
         BAS   RE,CHKPROF         CALL POSTER FOR PROFILE VALUES                
*                                                                               
         TM    MAKEPOST,X'80'     DDS MAKE POSTINGS =N                          
         BNO   DISR4                                                            
         LA    R2,BDIMSG                                                        
         LA    RE,L'NOPSTMSG-1                                                  
         EXMVC RE,0(R2),NOPSTMSG                                                
         LA    RE,1(RE)           RESTORE LENGTH                                
         AR    R2,RE              POINT R1 PAST MESSAGE                         
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         LA    RE,L'NOACCESS-1                                                  
         EXMVC RE,0(R2),NOACCESS                                                
         OI    BDIMSGH+6,X'80'                                                  
         B     DISRX              ACCESS NOT AUTHORIZED - GET OUT               
*                                                                               
DISR4    MVI   TRUERR,0           TRUE AOR POSTING ERROR FLAG                   
         MVI   TRUERRC,0                                                        
         BAS   RE,SETOGSTC        SET OUTPUT GST CODE FROM CLI BILL             
         BAS   RE,SETOPSTC        SET OUTPUT PST CODE FROM CLI BILL             
         CLC   =C'AOR',TYPBIL                                                   
         BNE   DISR5                                                            
         BAS   RE,DOAOR           IF NOT MAIN CLIENT BILL -                     
         BNE   DISR5              JUST CALL ACPOSTER DIRECTLY                   
         CLI   FNDTRUE,C'Y'                                                     
         BNE   DISRBAD            CAN'T FIND TRUE AOR BILL                      
         B     DISR6                                                            
*                                                                               
DISR5    LA    R3,PSTBLK                                                        
         USING ACPOSTD,R3                                                       
         BAS   RE,SETPST                                                        
         CLC   =C'AOR',TYPBIL                                                   
         BNE   *+8                                                              
         MVI   ACPAFLG,C'M'       WANT MAIN BILL POSTINGS                       
         GOTO1 ACPOSTER,APPARM,(R3)                                             
         MVI   SCSWSE,0           CLEAR SAVED ACC SYS(ACP. MESSED UP)           
         DROP  R3                                                               
         EJECT                                                                  
DISR6    LA    R1,PSTBLK                                                        
         USING ACPOSTD,R1                                                       
         TM    ACPERR,ACPNOREC    NO POST MAINT RECORDS                         
         BO    *+12                                                             
         TM    ACPERRC,ACPNOBPR                                                 
         BNO   DISR8                                                            
         LA    R2,BDIMSG                                                        
         LA    RE,L'NOPSTMSG-1                                                  
         EXMVC RE,0(R2),NOPSTMSG                                                
         LA    RE,1(RE)           RESTORE LENGTH                                
         AR    R2,RE              POINT R1 PAST MESSAGE                         
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         MVC   0(2,R2),=CL2'NO'                                                 
         LA    R2,3(R2)                                                         
         CLC   =C'REG',TYPBIL                                                   
         BNE   DISR6A                                                           
         OC    SUBTYPE,SUBTYPE     NO POST MAINT REC                            
         BZ    DISR7                                                            
         MVC   0(2,R2),SUBTYPE     NO UCPOST OR UNPOST MAINT REC                
         LA    R2,1(R2)                                                         
         CLI   0(R2),C' '                                                       
         BE    *+8                                                              
         LA    R2,1(R2)                                                         
         B     DISR7                                                            
DISR6A   CLC   =C'AOR',TYPBIL                                                   
         BNE   DISR6F                                                           
         OC    SUBTYPE,SUBTYPE                                                  
         BZ    DISR6D                                                           
         MVC   0(L'SUBTYPE,R2),SUBTYPE                                          
         LA    R2,L'SUBTYPE(R2)    NO UACPOST OR UANPOST MAINT REC              
         B     DISR7                                                            
DISR6D   MVI   0(R2),C'A'          NO APOST MAINT REC                           
         LA    R2,1(R2)                                                         
         B     DISR7                                                            
DISR6F   CLC   =C'FIN',TYPBIL                                                   
         BNE   DISR7                                                            
         MVI   0(R2),C'S'          NO SPOST MAIN REC                            
         LA    R2,1(R2)                                                         
DISR7    TM    ACPERRC,ACPNOBPR                                                 
         BNO   DISR7A                                                           
         LA    RE,L'NOBPR-1                                                     
         EXMVC RE,0(R2),NOBPR      NO BPOST/MAINT REC                           
         OI    BDIMSGH+6,X'80'                                                  
         B     DISRX                                                            
DISR7A   LA    RE,L'NOPREC-1                                                    
         EXMVC RE,0(R2),NOPREC     NO POST/MAINT RECS                           
         OI    BDIMSGH+6,X'80'                                                  
         B     DISRX                                                            
         DROP  R1                                                               
*                                                                               
DISR8    L     R3,APOSTING         LOOK AT POSTINGS RETURNED                    
         USING ACPRTND,R3                                                       
         MVI   SIDE2,C'Y'         INDICATE SECOND SIDE TO SCREEN                
         LA    R4,14              NUMBER OF LINES PER CHUNK                     
         LA    R2,BDIL1H                                                        
         USING DISD,R2                                                          
         LA    RE,POSTNUM         MAX NUMBER OF POSTINGS                        
*                                                                               
DISR10   OC    0(ACPRTNL,R3),0(R3) ANY POSTINGS RETURNED?                       
         BZ    DISR50                                                           
         MVC   DACC,ACPACC        ACCOUNT                                       
         CLI   ACPERR2,0          ANY ERROR WITH ACOCUNT                        
         BE    DISR38                                                           
         LA    R0,L'DACC                                                        
         LA    R1,DACC                                                          
         AR    R1,R0              PT TO END OF ACCOUNT                          
DISR32   CLI   0(R1),C' '                                                       
         BNH   DISR34                                                           
         MVI   1(R1),C'*'         PUT * AFTER LAST CHAR OF ACCOUNT              
         B     DISR38                                                           
DISR34   BCTR  R1,0               BACK UP A CHARACTER                           
         BCT   R0,DISR32                                                        
         LA    R0,L'DACC          IF NO ROOM FOR *                              
         LA    R1,DACC                                                          
         AR    R1,R0              PT TO END OF ACCOUNT                          
         MVI   0(R1),C'*'         PUT INBETWEEN ACCOUNT AND AMOUNTS             
*                                                                               
DISR38   OC    ACPAMT2,ACPAMT2                                                  
         BZ    DISR35                                                           
         EDIT  ACPAMT2,(11,DAMT),2,FLOAT=-,DUB=APDUB,WRK=APWORK                 
DISR35   XC    DTYPE,DTYPE                                                      
         TM    ACPSTAT,ACPCR                                                    
         BNO   *+12                                                             
         MVI   DTYPE,C'C'       DESCRIPTION OF ELEMENT TYPE                     
         B     DISR40                                                           
         TM    ACPSTAT,ACPDEB                                                   
         BNO   DISR40                                                           
         MVI   DTYPE,C'D'                                                       
DISR40   OC    ACPMEMO2,ACPMEMO2  MEMO                                          
         BZ    DISR45                                                           
         CLI   ACPBTYP,MBTTAPR    IF AOR RECEIVABLE - DON'T SHOW MEMO           
         BNE   DISR42                                                           
         CLC   =C'SR',ACPACC                                                    
         BE    DISR45                                                           
DISR42   EDIT  ACPMEMO2,(11,DMEMO),2,FLOAT=-,DUB=APDUB,WRK=APWORK               
DISR45   CLI   ACPBIL2,2          BILL # 2                                      
         BNE   *+8                                                              
         OI    6(R2),X'08'        HIGH INTENSITY                                
         OI    6(R2),X'80'        TRANSMIT                                      
         ZIC   R1,0(R2)           BUMP TO NEXT LINE SAME POSITION               
         AR    R2,R1                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R4,DISR50          DECREMENT LINE COUNTER                        
*                                                                               
         CLI   SIDE2,C'Y'         IF ALREADY USED SECOND SIDE                   
         BE    *+12                                                             
         MVI   MOREPOST,C'Y'      SET INDICATOR SOME PSTGS NOT DSPLYD           
         B     DISR55                                                           
         MVI   SIDE2,C'N'         NO MORE SIDES TO SCREEN                       
         LA    R2,BDIL1BH         PT TO FIRST PRINT LINE                        
         LA    R4,14              RESET NUMBER OF LINES                         
DISR50   LA    R3,ACPRTNL(R3)     NEXT POSTING RETURNED                         
         BCT   RE,DISR10                                                        
DISR55   BAS   RE,PRTMSGS         PRINT OUT MSGS                                
DISRX    MVC   FVMSGNO,=AL2(FVFOK) FOOL GENERAL INTO THINKING                   
         B     YES                 EVERYTHING OK                                
         DROP  R3                                                               
DISRBAD  XC    BDIMSG,BDIMSG                                                    
         MVC   BDIMSG(L'NOTRUE),NOTRUE                                          
         OI    BDIMSGH+6,X'80'                                                  
         B     DISRX                                                            
         EJECT                                                                  
*--------------------------------------------*                                  
* DSPDAT - DISPLAY INTERFACE  & INV DATE     *                                  
*--------------------------------------------*                                  
*                                                                               
DSPDAT   NTR1                                                                   
         TM    TWAMODE,TWAMLSM    IF COMING FROM LIST                           
         BO    DSPDATX            ALREADY DISPLAYED (IN DISP KEY)               
         L     R2,AIOAREA1                                                      
         USING BILLRECD,R2                                                      
         CLI   QSYS,C'P'          IF PRINT SYSTEM                               
         BNE   DSPDAT30                                                         
*                                                                               
         OC    PBILINVD,PBILINVD                                                
         BZ    DSPDAT10                                                         
         XC    BDIIDT,BDIIDT                                                    
         GOTO1 VDATCON,APPARM,(3,PBILINVD),(5,BDIIDT)                           
         OI    BDIIDTH+6,X'80'                                                  
DSPDAT10 OC    PBILPOST,PBILPOST                                                
         BZ    DSPDATX                                                          
         XC    BDITDT,BDITDT                                                    
         GOTO1 VDATCON,APPARM,(2,PBILPOST),(5,BDITDT)                           
         OI    BDITDTH+6,X'80'                                                  
         B     DSPDATX                                                          
*                                                                               
DSPDAT30 OC    BQDATE,BQDATE                                                    
         BZ    DSPDAT40                                                         
         XC    BDIIDT,BDIIDT                                                    
         GOTO1 VDATCON,APPARM,(0,BQDATE),(5,BDIIDT)                             
         OI    BDIIDTH+6,X'80'                                                  
DSPDAT40 OC    BILPOST,BILPOST                                                  
         BZ    DSPDATX                                                          
         XC    BDITDT,BDITDT                                                    
         GOTO1 VDATCON,APPARM,(2,BILPOST),(5,BDITDT)                            
         OI    BDITDTH+6,X'80'                                                  
*                                                                               
DSPDATX  B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*----------------------------------------------------*                          
* CHKDISP - CHECKS IF DISPLAY RULES OR DETAIL RECORD *                          
*----------------------------------------------------*                          
*                                                                               
CHKDISP  NTR1                                                                   
         L     R2,AIOAREA1                                                      
         USING BILLRECD,R2                                                      
         CLI   QSYS,C'P'                                                        
         BNE   CHKD30                                                           
*                                                                               
         CLI   SCSELCOD,C'R'      RE-READ POST MAINT(RULES) RECORDS?            
         BE    CHKD10                                                           
         TM    PBILLCTL+1,X'01'   IS BILL TRANSFERED?                           
         BO    YES                YES -THEN READ POST DETAIL RECORDS            
         OC    PBILPOST,PBILPOST                                                
         BNZ   YES                YES -READ POST DETAIL                         
CHKD10   BAS   RE,SETPTYPE        SET TYPBIL FOR PRINT                          
         B     NO                                                               
*                                                                               
CHKD30   CLI   SCSELCOD,C'R'      RE-READ POST MAINT(RULES) RECORDS?            
         BE    CHKD40                                                           
         TM    BCNTRL,X'01'       IS BILL TRANSFERED?                           
         BO    YES                YES - READ POST DETAIL RECORDS                
         OC    BILPOST,BILPOST                                                  
         BNZ   YES                YES - READ POST DETAIL                        
CHKD40   BAS   RE,SETSTYPE        SET TYPBIL FOR SPOTL                          
         B     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
*----------------------------------------------------*                          
* SETOGSTC- GET OUTPUT GST CODE FROM CLIENT BILL REC *                          
*----------------------------------------------------*                          
*                                                                               
SETOGSTC NTR1                                                                   
         L     R2,AIOAREA1                                                      
         USING BILLRECD,R2                                                      
         MVI   GSTO,C' '                                                        
         CLI   QSYS,C'P'                                                        
         BNE   SETOGST5                                                         
*                                                                               
         SR    R0,R0                                                            
         LA    R1,PBILLEL                                                       
         USING PBILVEL,R1                                                       
SETOGST2 CLI   0(R1),0                                                          
         BE    SETOGSTX                                                         
         CLI   0(R1),X'0A'                                                      
         BE    SETOGST3                                                         
         ICM   R0,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     SETOGST2                                                         
*                                                                               
SETOGST3 MVC   GSTO,PBILLVCD                                                    
         B     SETOGSTX                                                         
         DROP  R1                                                               
*                                                                               
SETOGST5 CLC   BLEN,=H'90'                                                      
         BNH   SETOGSTX                                                         
         CLI   BVATCOD,0                                                        
         BE    SETOGSTX                                                         
         MVC   GSTO,BVATCOD                                                     
SETOGSTX B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*----------------------------------------------------*                          
* SETOPSTC- GET OUTPUT PST CODE FROM CLIENT BILL REC *                          
*----------------------------------------------------*                          
*                                                                               
SETOPSTC NTR1                                                                   
         LA    RE,L'PSTOVALS       CLEAR PST INFO (CODES,AMOUNTS)               
         MH    RE,=Y(ACPMCODE)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    PSTOVALS(0),PSTOVALS                                             
*                                                                               
         L     R2,AIOAREA1         R2=A(CLIENT BILL RECORD)                     
         USING BILLRECD,R2                                                      
         LA    RF,PSTOVALS         RF=A(PST TABLE INFORMATION)                  
         CLI   QSYS,C'P'                                                        
         BNE   SETOPST5                                                         
         SR    R0,R0                                                            
         LA    R1,PBILLEL                                                       
         USING PBILVEL,R1                                                       
SETOPST2 CLI   0(R1),0                                                          
         BE    SETOPSTX                                                         
         CLI   0(R1),X'84'         IF ANY PROVINCIAL VAT ELEMENTS               
         BNE   *+12                                                             
         BAS   RE,SETPVAL          SET PST INFORMATION                          
         LA    RF,L'PSTOVALS(RF)   BUMP TO NEXT ENTRY IN TABLE                  
         ICM   R0,1,1(R1)          GET NEXT PRINT ELEMENT                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     SETOPST2            AND LOOP                                     
         DROP  R1                                                               
*                                                                               
SETOPST5 CLC   BLEN,=H'130'        IF NEW BILL LENGTH                           
         BNH   SETOPSTX                                                         
         SR    R0,R0                                                            
         ICM   R0,1,BILNPVTS       IF ANY PROVINCIAL VAT ELEMENTS               
         BZ    SETOPSTX                                                         
         LA    R1,BILPVELD         R1=A(FIRST ELEMENT)                          
SETOPST8 BAS   RE,SETPVAL          SET PST INFORMATION                          
         LA    RF,L'PSTOVALS(RF)   BUMP TO NEXT TABLE ENTRY                     
         LA    R1,BILPVLEN(R1)                                                  
         BCT   R0,SETOPST8                                                      
*                                                                               
SETOPSTX B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
* SETPVAL - ROUTINE TO SAVE PROVINCIAL VAT INFORMATION                          
*                                                                               
*                                  RF=A(POSITION IN PSTOVALS)                   
         USING PSTELD,R1           R1=A(PST ELEMENT)                            
SETPVAL  NTR1                                                                   
         CLI   QSYS,C'P'           IF PRINT SYSTEM                              
         BNE   *+8                                                              
         LA    R1,2(R1)            PT TO ELEMENT DATA                           
         ZIC   RE,PSTPVPRV         PROVINCE NUMBER                              
         BCTR  RE,0                                                             
         MH    RE,=AL2(L'PRVTAB)                                                
         LA    RE,PRVTAB(RE)                                                    
         USING PRVTABD,RE                                                       
         MVC   0(2,RF),PRVTCODE    PST PROVINCE CODE                            
         MVC   2(1,RF),PRVTOUT     PST ELEMENT EQUATE                           
         MVC   3(1,RF),PSTPVCOD    PST TAX CODE                                 
         MVC   4(4,RF),PSTPVBAS    PST BASIS                                    
         MVC   8(4,RF),PSTPVAMT    PST AMOUNT                                   
         B     EXIT                                                             
         DROP  R1,RE                                                            
         EJECT                                                                  
*----------------------------------------------------*                          
* SETPST - SET UP CALL TO ACPOSTER                   *                          
*----------------------------------------------------*                          
SETPST   NTR1                                                                   
         XC    IOKEY,IOKEY        READ DUMMY ACC REC TO SWITCH                  
         MVC   IOKEY(1),COMPANY   TO NATIVE ACC SYSTEM BEFORE CALLING           
         GOTO1 AMIOACC,APPARM,IOHI+IOACCFIL+IO2,=C'SE1'                         
         LA    RE,PSTBLK          CLEAR CONTROL BLOCK                           
         LA    RF,ACPOSTL                                                       
         XCEFL                                                                  
         LA    R3,PSTBLK                                                        
         USING ACPOSTD,R3                                                       
         MVC   ACPBILL,AIOAREA1   A(BILL RECORD)                                
         MVC   ACPACOM,ACOM       A(COMFACS)                                    
         MVC   ACPSW,VSWITCH      A(SWITCH)                                     
         MVC   ACPPOST,APSTTBL    A(POSTING ACCS RETURNED)                      
         MVC   ACPPOST2,APOSTING  A(ACTUAL POSTINGS RETURNED)                   
         MVC   ACPXSORT,VXSORT    ADDRESS OF XSORT                              
         MVC   ACPCMPC,COMPANY    NATIVE COMPANY CODE                           
         MVC   ACPCMPC2,COMPANY2  OTHER COMPANY CODE                            
         MVC   ACPSPROD,SVPROD    UNIT,LEDGER                                   
         MVC   ACPMI,SVMI         Y=USE MI RECORDS                              
         MVC   ACPCOST,SVCOST     Y=USE COSTING,BILLING,REVENUE                 
         MVC   ACPALPH,QALPH      AGENCY FOR MEDIA SPLIT FILES                  
         MVC   ACPSYS,QSYS        SYSTEM                                        
         MVC   ACPMED,QMED        MEDIA CODE                                    
         MVC   ACPOFC,SVCOFF      CLIENT OFFICE CODE                            
         MVC   ACPOFG,SVOFFG      OFFICE GROUP                                  
         MVC   ACPCLT,QCLT        CLIENT CODE                                   
         MVC   ACPPRD,QPRD        PRODUCT CODE                                  
         MVC   ACPSJPRD,SJPRD     CHECK FOR SJ PRODUCT                          
         MVC   ACP1CPRD,SJ1CPRD   CHECK FOR PRODUCT LVL 1C                      
         MVC   ACPSE1,SVSE1       NATIVE SE #                                   
         MVC   ACPSE2,SVSE2       OTHER SE NUMBER                               
         CLC   =C'AOR',TYPBIL                                                   
         BNE   SETPST15                                                         
* APCTACT IS NOW A 6 BYTE FIELD TO ACCOMODATE PRINT BILLING                     
         MVC   ACPTACT,TRUEAMT    TRUE AOR AMOUNT                               
         MVC   ACPTGST,TRUEGST    TRUE AOR GST AMOUNT                           
         MVC   ACPTGSTI,TRUEGSTI  TRUE AOR GST CODE                             
         MVC   ACPPSTI(10*L'ACPPSTI),PSTIVALS TRUE AOR PST INFO                 
SETPST15 LA    RE,MAXPNUM         MAX NUMBER OF ACCOUNTS                        
         STC   RE,ACPNUMA                                                       
         LA    RE,POSTNUM         MAX NUMBER OF POSTINGS                        
         STC   RE,ACPNUM2                                                       
         MVC   ACPGSTO,GSTO       GST CODE FROM BILL RECORD                     
         MVC   ACPPSTO(10*L'ACPPSTO),PSTOVALS PST INFORMATION                   
         MVC   ACPCC,CCOVER       CC OVERRIDE                                   
         MVC   ACPIOR,IORPCT      % OF GROSS FOR INT                            
         MVC   ACPTPCT,TRDPCT     TRADE PERCENTAGE                              
         MVC   ACPTBAS,TRDBAS     TRADE BASIS                                   
         MVC   ACPTOFF,TRDOFF     TRADE OFFICE                                  
         MVC   ACPTMOFF,MTRDOFF   GROUPM MIDAS TRADE OFFICE                     
         MVC   ACPTMPCT,MTRDPCT   GROUPM MIDAS SPLIT PERCENTAGE                 
         CLC   =C'RET',TYPBIL                                                   
         BNE   SETPST23                                                         
         MVC   ACPLDG,RLDG        UNIT 3 LEDGER                                 
         MVC   ACPRCV,RRCV        RCV ACCOUNT OVERRIDE                          
         MVC   ACPCST,RCST                                                      
         MVC   ACPMGROV,RMGR                                                    
*                                                                               
SETPST23 CLI   ACPSYS,C'P'        IF PRINT BILLS                                
         BNE   SETPSTX                                                          
         MVC   ACPSUFX,SUSFX      SUSPENSE SUFFIX                               
         MVC   ACPRBFX,REBFX      REBATE SUFFIX                                 
         MVC   ACPRCLT,RCVCLT     GET RCVBL ACC FROM CLIENT                     
         MVC   ACPRVSJ,RCVSJ      POST TO RCVBL SJ                              
         MVC   ACPJBCD,JBCD       JOB CODE FOR SEP CD                           
         MVC   ACPWKSJ,WKSJ       WORK CODE FOR SJ                              
         MVC   ACPNMEM,NOMEMO     POST CD MEMO TO SJ                            
SETPSTX  B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
PRTMSGS  NTR1                                                                   
         SR    R1,R1              COUNTER OF LENGTH OF WORDS SO FAR             
         LA    R3,PSTBLK                                                        
         USING ACPOSTD,R3                                                       
         LA    R2,BDIMSG                                                        
         BAS   RE,CHKERRS          IF NO ERRORS FOUND                           
         BNE   PRTM2                                                            
         LA    RE,L'PSTMSG-1                                                    
         EXMVC RE,0(R2),PSTMSG    POSTINGS WILL BE MADE                         
         LA    RE,1(RE)           RESTORE LENGTH                                
         AR    R2,RE              POINT R1 PAST MESSAGE                         
         AR    R1,RE              ADD TO LENGTH OF WORDS SO FAR                 
         CLI   MOREPOST,C'Y'      IF MORE POSTINGS THAN SHOWN                   
         BNE   PRTM90                                                           
         MVI   0(R2),C','         INDICATE SO                                   
         LA    R2,1(R2)                                                         
         LA    R1,1(R1)                                                         
         LA    RE,L'MOREPMSG-1                                                  
         EXMVC RE,0(R2),MOREPMSG                                                
         LA    RE,1(RE)                                                         
         AR    R2,RE              PT R2 PAST MESSAGE                            
         AR    R1,RE              ADD TO LENGTH OF MSG DISPLAYED                
         B     PRTM90                                                           
*                                                                               
PRTM2    LA    RE,L'NOPSTMSG-1                                                  
         EXMVC RE,0(R2),NOPSTMSG                                                
         LA    RE,1(RE)           RESTORE LENGTH                                
         AR    R2,RE              POINT R1 PAST MESSAGE                         
         AR    R1,RE              ADD TO LENGTH OF WORDS SO FAR                 
         TM    MAKEPOST,X'40'     MAKE POSTINGS =N                              
         BNO   PRTM12                                                           
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         LA    R1,1(R1)                                                         
         LA    RE,L'NOPOST-1                                                    
         EXMVC RE,0(R2),NOPOST    POSTINGS = NO                                 
         LA    RE,1(RE)                                                         
         AR    R2,RE              PT R2 PAST MESSAGE                            
         AR    R1,RE              ADD TO LENGTH OF MSG DISPLAYED                
PRTM12   CLI   MOREPOST,C'Y'      IF MORE POSTINGS THAN SHOWN                   
         BNE   PRTM15                                                           
         MVI   0(R2),C','          INDICATE SO                                  
         LA    R2,1(R2)                                                         
         LA    RE,L'MOREPMSG-1                                                  
         EXMVC RE,0(R2),MOREPMSG                                                
         LA    RE,1(RE)                                                         
         AR    R2,RE              PT R2 PAST MESSAGE                            
         AR    R1,RE              ADD TO LENGTH OF MSG DISPLAYED                
*                                                                               
PRTM15   CLI   ACPERR,0                                                         
         BNE   PRTM20                                                           
         CLI   ACPERRC,0                                                        
         BE    PRTM40                                                           
PRTM20   MVC   APBYTE,ACPERR      PRINT OUT MAIN BILL ERROR MSGS                
         MVC   APBYTEC,ACPERRC                                                  
         BAS   RE,GENMSGS         PRINT OUT GENERAL MESSAGES                    
         BNE   PRTM90             NO MORE ROOM LEFT                             
*                                                                               
PRTM40   CLI   TRUERR,0           ANY TRUE AOR ERROR MSGS?                      
         BNE   PRTM50                                                           
         CLI   TRUERRC,0                                                        
         BE    PRTM80                                                           
PRTM50   MVC   APBYTE,TRUERR                                                    
         MVC   APBYTEC,TRUERRC                                                  
         BAS   RE,GENMSGS         PRINT OUT GENERAL MESSAGES                    
         BNE   PRTM90                                                           
*                                                                               
PRTM80   BAS   RE,PSTMSGS         PRINT OUT SPECIFIC POSTING ERRORS             
*                                                                               
PRTM90   OI    BDIMSGH+6,X'80'                                                  
         OI    BDIMSG2H+6,X'80'                                                 
         B     EXIT                                                             
         SPACE 2                                                                
*        CHECK ANY ERRORS CC<> IF ERRORS FOUND                                  
*                                                                               
CHKERRS  CLI   MAKEPOST,0         MAIN BILL ERRORS                              
         BNER  RE                                                               
         CLI   ACPERR,0                                                         
         BNER  RE                                                               
         CLI   ACPERRC,0                                                        
         BNER  RE                                                               
         CLI   TRUERR,0           TRUE AOR ERRORS IF ANY                        
         BNER  RE                                                               
         CLI   TRUERRC,0                                                        
         BR    RE                                                               
         EJECT                                                                  
GENMSGS  NTR1                                                                   
         TM    APBYTE,ACPNOMI     NO MI RECORD FOUND                            
         BNO   GENM15                                                           
         LA    R4,L'NOMIREC-1                                                   
         BAS   RE,CHKROOM         MOVES , & SETS ADDRESS OF LINE                
         BNE   GENM90             NO MORE ROOM FOR ERROR MSGS                   
         EXMVC R4,0(R2),NOMIREC   NO MI RECORD                                  
         BAS   RE,BUMP            PT R2 PAST MESSAGE                            
*                                                                               
GENM15   TM    APBYTE,ACPSJERR                                                  
         BNO   GENM20                                                           
         LA    R4,L'NOCLTREC-1                                                  
         BAS   RE,CHKROOM                                                       
         BNE   GENM90             NO MORE ROOM FOR ERROR MSGS                   
         EXMVC R4,0(R2),NOCLTREC  NO __ CLT REC                                 
         MVC   3(2,R2),SVPROD     FILLS BLANK                                   
         BAS   RE,BUMP            PT R2 PAST MESSAGE                            
*                                                                               
GENM20   TM    APBYTE,ACPNOPRD                                                  
         BNO   GENM25                                                           
         LA    R4,L'NOPRDREC-1                                                  
         BAS   RE,CHKROOM                                                       
         BNE   GENM90             NO MORE ROOM FOR ERROR MSGS                   
         EXMVC R4,0(R2),NOPRDREC  NO __ PRD REC                                 
         MVC   3(2,R2),SVPROD     FILLS BLANK                                   
         BAS   RE,BUMP            PT R2 PAST MESSAGE                            
*                                                                               
GENM25   TM    APBYTE,ACPNOP1C                                                  
         BNO   GENM35                                                           
         LA    R4,L'NOPRD1C-1                                                   
         BAS   RE,CHKROOM                                                       
         BNE   GENM90             NO MORE ROOM FOR ERROR MSGS                   
         EXMVC R4,0(R2),NOPRD1C   NO PRD LVL 1C                                 
         BAS   RE,BUMP            PT R2 PAST MESSAGE                            
*                                                                               
GENM35   TM    APBYTE,ACPMISSA                                                  
         BNO   GENM40                                                           
         LA    R4,L'NOAMTS+3-1    JUST INCASE HAVE TO ADD LETTERS               
         BAS   RE,CHKROOM                                                       
         BNE   GENM90             NO MORE ROOM FOR ERROR MSGS                   
         SH    R4,=H'3'           RESET LENGTH                                  
         CLC   =C'REG',TYPBIL                                                   
         BNE   GENM35A                                                          
         OC    SUBTYPE,SUBTYPE                                                  
         BZ    GENM38                                                           
         MVC   0(2,R2),SUBTYPE                                                  
         LA    R2,1(R2)                                                         
         CLI   0(R2),C' '                                                       
         BE    *+8                                                              
         LA    R2,1(R2)                                                         
         B     GENM38                                                           
*                                                                               
GENM35A  CLC   =C'AOR',TYPBIL                                                   
         BNE   GENM36                                                           
         OC    SUBTYPE,SUBTYPE                                                  
         BZ    GENM35D                                                          
         MVC   0(L'SUBTYPE,R2),SUBTYPE  UACPOST OR UANPOST MAINT                
         LA    R2,L'SUBTYPE(R2)                                                 
         B     GENM38                                                           
GENM35D  MVI   0(R2),C'A'          APOST MAINT                                  
         LA    R2,1(R2)                                                         
         B     GENM38                                                           
*                                                                               
GENM36   CLC   =C'FIN',TYPBIL                                                   
         BNE   GENM38                                                           
         MVI   0(R2),C'S'                                                       
         LA    R2,1(R2)                                                         
GENM38   EXMVC R4,0(R2),NOAMTS    NO POST/MAINT AMOUNTS                         
         BAS   RE,BUMP            PT R2 PAST MESSAGE                            
*                                                                               
GENM40   TM    APBYTE,ACPNOTEQ                                                  
         BNO   GENM55                                                           
         LA    R4,L'NOMATCH-1                                                   
         BAS   RE,CHKROOM                                                       
         BNE   GENM90             NO MORE ROOM FOR ERROR MSGS                   
         EXMVC R4,0(R2),NOMATCH   DEBITS DO NOT EQUAL CREDITS                   
         BAS   RE,BUMP            PT R2 PAST MESSAGE                            
*                                                                               
GENM55   TM    APBYTEC,ACPNOWK                                                  
         BNO   GENM58                                                           
         LA    R4,L'NOWKCD-1                                                    
         BAS   RE,CHKROOM                                                       
         BNE   GENM90             NO MORE ROOM FOR ERROR MSGS                   
         EXMVC R4,0(R2),NOWKCD    NO WORK CODE                                  
         BAS   RE,BUMP            PT R2 PAST MESSAGE                            
*                                                                               
GENM58   TM    APBYTEC,ACPIRET                                                  
         BNO   GENM62                                                           
         LA    R4,L'INVRET-1                                                    
         BAS   RE,CHKROOM                                                       
         BNE   GENM90             NO MORE ROOM FOR ERROR MSGS                   
         EXMVC R4,0(R2),INVRET    NO RPROF/OR NOT SET UP CORRECTLY              
         BAS   RE,BUMP            PT R2 PAST MESSAGE                            
*                                                                               
GENM62   TM    APBYTEC,ACPINT                                                   
         BNO   GENM64                                                           
         LA    R4,L'INTERR-1                                                    
         BAS   RE,CHKROOM                                                       
         BNE   GENM90             NO MORE ROOM FOR ERROR MSGS                   
         EXMVC R4,0(R2),INTERR    INTERNAL POSTINGS NOT SET UP                  
         BAS   RE,BUMP            PT R2 PAST MESSAGE                            
*                                                                               
GENM64   DS    0H                                                               
         CR    RB,RB                                                            
         B     GENMX                                                            
*                                                                               
GENM90   LTR   RB,RB                                                            
GENMX    XIT1  REGS=(R1,R2)                                                     
         DROP  R3                                                               
         EJECT                                                                  
PSTMSGS  NTR1                                                                   
         USING ACPRTND,R3                                                       
         LA    R0,POSTNUM                                                       
         L     R3,APOSTING        LOOK AT POSTINGS RETURNED                     
*                                                                               
PSTM30   OC    0(ACPRTNL,R3),0(R3)                                              
         BZ    PSTM88                                                           
         TM    ACPERR2,ACPEINV                                                  
         BNO   PSTM65                                                           
         TM    PRNTD,ACPEINV      CHECK IF MESSAGE ALREADY PRINTED              
         BO    PSTM65             PRINTED - SKIP TO NEXT                        
         OI    PRNTD,ACPEINV                                                    
         LA    R4,L'NOREC-1                                                     
         BAS   RE,CHKROOM                                                       
         BNE   PSTM90             NO MORE ROOM FOR ERROR MSGS                   
         EXMVC R4,0(R2),NOREC     RECORD NOT FOUND                              
         BAS   RE,BUMP            PT R2 PAST MESSAGE                            
*                                                                               
PSTM65   TM    ACPERR2,ACPEINP                                                  
         BNO   PSTM70                                                           
         TM    PRNTD,ACPEINP      CHECK IF MESSAGE ALREADY PRINTED              
         BO    PSTM70             PRINTED - SKIP TO NEXT                        
         OI    PRNTD,ACPEINP                                                    
         LA    R4,L'INVPOST-1                                                   
         BAS   RE,CHKROOM                                                       
         BNE   PSTM90             NO MORE ROOM FOR ERROR MSGS                   
         EXMVC R4,0(R2),INVPOST   INVALID ACCOUNT FOR POSTING                   
         BAS   RE,BUMP            PT R2 PAST MESSAGE                            
*                                                                               
PSTM70   TM    ACPERR2,ACPEUNT3                                                 
         BNO   PSTM73                                                           
         TM    PRNTD,ACPEUNT3     CHECK IF MESSAGE ALREADY PRINTED              
         BO    PSTM73             PRINTED - SKIP TO NEXT                        
         OI    PRNTD,ACPEUNT3                                                   
         LA    R4,L'NOUNT3-1                                                    
         BAS   RE,CHKROOM                                                       
         BNE   PSTM90             NO MORE ROOM FOR ERROR MSGS                   
         EXMVC R4,0(R2),NOUNT3    ERROR WITH UNIT 3 ACCOUNT                     
         BAS   RE,BUMP            PT R2 PAST MESSAGE                            
*                                                                               
PSTM73   TM    ACPERR2,ACPE3ROC                                                 
         BNO   PSTM75                                                           
         TM    PRNTD,ACPE3ROC     CHECK IF MESSAGE ALREADY PRINTED              
         BO    PSTM75             PRINTED - SKIP TO NEXT                        
         OI    PRNTD,ACPE3ROC                                                   
         LA    R4,L'NOUNT3RC-1                                                  
         BAS   RE,CHKROOM                                                       
         BNE   PSTM90             NO MORE ROOM FOR ERROR MSGS                   
         EXMVC R4,0(R2),NOUNT3RC  POINTER ON UNIT 3 PARTIC MISSING              
         BAS   RE,BUMP            PT R2 PAST MESSAGE                            
*                                                                               
PSTM75   TM    ACPERR2,ACPEPAR                                                  
         BNO   PSTM80                                                           
         TM    PRNTD,ACPEPAR      CHECK IF MESSAGE ALREADY PRINTED              
         BO    PSTM80             PRINTED - SKIP TO NEXT                        
         OI    PRNTD,ACPEPAR                                                    
         LA    R4,L'NOPART-1                                                    
         BAS   RE,CHKROOM                                                       
         BNE   PSTM90             NO MORE ROOM FOR ERROR MSGS                   
         EXMVC R4,0(R2),NOPART    ERROR WITH PARTICIPANT                        
         BAS   RE,BUMP            PT R2 PAST MESSAGE                            
*                                                                               
PSTM80   TM    ACPERR2,ACPESFX                                                  
         BNO   PSTM85                                                           
         TM    PRNTD,ACPESFX      CHECK IF MESSAGE ALREADY PRINTED              
         BO    PSTM85             PRINTED - SKIP TO NEXT                        
         OI    PRNTD,ACPESFX                                                    
         LA    R4,L'NOSUFFX-1                                                   
         BAS   RE,CHKROOM                                                       
         BNE   PSTM90             NO MORE ROOM FOR ERROR MSGS                   
         EXMVC R4,0(R2),NOSUFFX   ERROR WITH SUFFIX                             
         BAS   RE,BUMP            PT R2 PAST MESSAGE                            
*                                                                               
PSTM85   TM    ACPERR2,ACPEADCD                                                 
         BNO   PSTM88                                                           
         TM    PRNTD,ACPEADCD     CHECK IF MESSAGE ALREADY PRINTED              
         BO    PSTM88             PRINTED - SKIP TO NEXT                        
         OI    PRNTD,ACPEADCD                                                   
         LA    R4,L'NOADCD-1                                                    
         BAS   RE,CHKROOM                                                       
         BNE   PSTM90             NO MORE ROOM FOR ERROR MSGS                   
         EXMVC R4,0(R2),NOADCD    ERROR WITH ADCODE                             
         BAS   RE,BUMP            PT R2 PAST MESSAGE                            
*                                                                               
PSTM88   LA    R3,ACPRTNL(R3)                                                   
         BCT   R0,PSTM30                                                        
         CR    RB,RB                                                            
         B     PSTMX                                                            
*                                                                               
PSTM90   LTR   RB,RB              NO MORE ROOM                                  
PSTMX    XIT1  REGS=(R1,R2)                                                     
         DROP  R3                                                               
         EJECT                                                                  
*----------------------------------------------------*                          
* CHKMSG - CHECK IF ENOUGH ROOM FOR MESSAGES         *                          
*----------------------------------------------------*                          
*                                                                               
CHKROOM  NTR1                                                                   
         LA    R4,1(R4)           INCREMENT TO REAL LENGTH                      
         LA    RE,L'BDIMSG        LENGTH OF LINE ONE                            
         CLI   LINENUM,1                                                        
         BE    *+8                                                              
         LA    RE,L'BDIMSG2                                                     
         AR    R1,R4              ADD NEW MSG TO TOTAL LENGTH SO FAR            
         CR    R1,RE                                                            
         BL    CHKROOM5           OKAY TO ADD THIS LINE                         
         CLI   LINENUM,2                                                        
         BE    CHKROOMN           NO MORE ROOM                                  
         LA    R2,BDIMSG2         PT TO SECOND LINE                             
         MVI   LINENUM,2                                                        
         LR    R1,R4              SET LGTH SO FAR                               
         B     CHKROOMY                                                         
*                                                                               
CHKROOM5 MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         LA    R1,1(R1)           PLUS ONE FOR COMMA                            
CHKROOMY CR    RB,RB                                                            
         B     CHKROOMX                                                         
CHKROOMN LTR   RB,RB              NO ROOM FOR ANY MESSAGES                      
CHKROOMX XIT1  REGS=(R1,R2)                                                     
         SPACE                                                                  
*----------------------------------------------------*                          
* BUMP - PT R2 PAST MESSAGE JUST PRINTED             *                          
*----------------------------------------------------*                          
*                                                                               
BUMP     NTR1                                                                   
         LA    R4,1(R4)           RESTORE LENGTH OF MESSAGE                     
         AR    R2,R4                                                            
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*--------------------------------------------*                                  
* DISPLAYING TRANSFERRED BILL                *                                  
* READ POST DETAIL RECORDS                   *                                  
*--------------------------------------------*                                  
*                                                                               
DSPPOST  NTR1                     READS POST DETAIL RECORDS                     
         XC    BDIMSG,BDIMSG                                                    
         OI    BDIMSGH+6,X'80'                                                  
         L     RE,AIOAREA1                                                      
         LA    RF,2000                                                          
         XCEF                                                                   
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING MPDRECD,R4                                                       
         MVI   MPDKTYP,MPDKTYPQ    X'2F'                                        
         MVI   MPDKSUB,MPDKSUBQ    X'00'                                        
         MVC   MPDKCPY,COMPANY    NATIVE COMPANY CODE                           
         MVC   MPDKALPH,QALPH     AGENCY FOR MEDIA SPLIT FILES                  
         MVC   MPDKSYS,QSYS       SYSTEM                                        
         MVC   MPDKMED,QMED       MEDIA                                         
         MVC   MPDKCLI,QCLT       CLIENT                                        
         MVC   MPDKPRD,QPRD       PRODUCT                                       
         MVC   MPDKEST,QEST       ESTIMATE #                                    
*                                                                               
         XC    APWORK,APWORK                                                    
         MVC   APWORK(3),BRDT                                                   
         GOTO1 VDATCON,APPARM,(3,APWORK),(0,APWORK+3)                           
         MVC   MPDKPER,APWORK+3   YYMM                                          
*                                                                               
         CLI   BSRVM+1,X'0D'      SPECIAL 13TH MONTH                            
         BNE   DSPPST8                                                          
         MVC   APWORK(2),BSRVM                                                  
         MVI   APWORK+2,X'01'                                                   
         GOTO1 VDATCON,APPARM,(3,APWORK),(0,APWORK+3)                           
         MVC   MPDKMOS(2),APWORK+3                                              
         MVC   MPDKMOS+2(2),=C'13'                                              
         B     DSPPST10                                                         
DSPPST8  XC    APWORK,APWORK                                                    
         MVC   APWORK(2),BSRVM                                                  
         MVI   APWORK+2,X'01'                                                   
         GOTO1 VDATCON,APPARM,(3,APWORK),(0,APWORK+3)                           
         MVC   MPDKMOS,APWORK+3   YYMM                                          
*                                                                               
*                                                                               
DSPPST10 XC    APWORK,APWORK                                                    
         GOTO1 VDATCON,APPARM,(5,BDIRDT),(0,APWORK)                             
         GOTO1 =V(SPFMTINO),APPARM,APWORK,(2,BINV),(QMED,B1PROF),      X        
               B1XPROF,RR=Y                                                     
         L     R1,APPARM+4                                                      
         MVI   MPDKINV,C'0'                                                     
         MVC   MPDKINV+1(4),3(R1)                                               
*                                                                               
*SPPST10 EDIT  BINV,(5,MPDKINV),FILL=0,DUB=APDUB,WRK=APWORK                     
         GOTO1 AMIOACC,APPARM,IOACCFIL+IORD+IO1,=C'SE1'                         
         CLI   MYIOERR,0                                                        
         BNE   DSPNOREC           NO POSTING RECORD FOUND                       
*                                                                               
         MVC   SKEY,IOKEY         SAVE KEY                                      
         MVI   SECREC,C'N'                                                      
         L     R4,AIOAREA1                                                      
         TM    MPDRSTA,X'01'      SECOND RECORD TO FOLLOW                       
         BNO   *+8                                                              
         MVI   SECREC,C'Y'                                                      
         DROP  R4                                                               
*                                                                               
         MVI   SIDE2,C'Y'         HAVEN'T USED SIDE TWO OF SCREEN               
         LA    R3,14              # OF LINES PER SIDE                           
         LA    R2,BDIL1H          PT TO FIRST PRINT LINE                        
         USING DISD,R2                                                          
DSPPST20 L     R4,AIOAREA1                                                      
         USING MBTELD,R4                                                        
         AH    R4,DATADISP                                                      
         CLI   0(R4),GDAELQ       ONLY DATE ELEMENT - MEANS RE-TRANSFER         
         BE    DSPNOREC           NO POSTING RECORD FOUND                       
*                                                                               
DSPPST30 CLI   0(R4),0            END OF RECORD?                                
         BE    DSPPST70           YES - CHECK IF SECOND RECORD EXISTS           
         CLI   0(R4),MBTELQ       POSTING ELEMENT?                              
         BNE   DSPPST50           NO - GET NEXT ELEMENT                         
*                                 YES - MOVE OUT ALL INFO TO SCREEN             
         MVC   DACC,MBTULA        ACCOUNT                                       
         OC    MBTPOST,MBTPOST    ANY POSTING AMOUNT?                           
         BZ    DSPPST35                                                         
         EDIT  MBTPOST,(11,DAMT),2,FLOAT=-,DUB=APDUB,WRK=APWORK                 
DSPPST35 MVI   DTYPE,C'D'                                                       
         TM    MBTSTAT,MBTSDR   DEBIT POSTING                                   
         BO    *+8                                                              
         MVI   DTYPE,C'C'                                                       
         OC    MBTMEMO,MBTMEMO    ANY MEMO AMOUNT?                              
         BZ    DSPPST40                                                         
         CLI   MBTTYP,MBTTAPR     IF AOR RECEIVABLE - DON'T SHOW MEMO           
         BNE   DSPPST38                                                         
         CLC   =C'SR',MBTULA                                                    
         BE    DSPPST40                                                         
DSPPST38 EDIT  MBTMEMO,(11,DMEMO),2,FLOAT=-,DUB=APDUB,WRK=APWORK                
DSPPST40 TM    MBTSTAT,MBTSAOR    TRUE AOR POSTING                              
         BNO   DSPPST50                                                         
         OI    6(R2),X'08'                                                      
*                                                                               
DSPPST50 SR    R0,R0              GET NEXT POSTING ELEMENT                      
         ICM   R0,1,1(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         ZIC   R1,0(R2)           PT TO NEXTLINE SAME POSITION                  
         AR    R2,R1                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R3,DSPPST30                                                      
*                                 NO MORE LINES LEFT                            
         CLI   SIDE2,C'Y'         STILL HAVE SIDE 2 TO USE?                     
         BNE   DSPPSTO            NO - GIVE MESSAGE INDICATING SO               
         MVI   SIDE2,C'N'         SET TO SIDE TWO USED                          
         LA    R2,BDIL1BH         PT TO SECOND SIDE                             
         LA    R3,14                                                            
         B     DSPPST30           CONTINUE                                      
         DROP  R4                                                               
DSPPST70 CLI   SECREC,C'Y'                                                      
         BNE   DSPPSTX                                                          
         MVI   SECREC,C'N'        STOP LOOP                                     
         MVC   IOKEY,SKEY         RESORE KEY                                    
         GOTO1 AMIOACC,APPARM,IOACCFIL+IORD+IO1,=C'SE1'                         
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,AIOAREA1                                                      
         LA    RF,2000                                                          
         XCEF                                                                   
         GOTO1 AMIOACC,APPARM,IOACCFIL+IOSEQ+IO1,=C'SE1'                        
         L     RE,AIOAREA1                                                      
         USING MPDRECD,RE                                                       
         LA    R1,MPDKSEQ-MPDKTYP-1    DON'T INCLUDE SEQ #                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),SKEY                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   MPDKSEQ,0                                                        
         BNE   DSPPST20                                                         
         DC    H'0'                                                             
DSPPSTX  B     EXIT                                                             
         DROP  RE                                                               
         SPACE                                                                  
DSPNOREC XC    BDIMSG,BDIMSG                                                    
         MVC   BDIMSG(24),=CL24'NO POSTING RECORDS FOUND'                       
         OI    BDIMSGH+6,X'80'                                                  
         B     DSPPSTX                                                          
         SPACE                                                                  
DSPPSTO  XC    BDIMSG,BDIMSG       GIVE OUT OF ROOM MESSAGE                     
         MVC   BDIMSG(24),=CL24'MORE POSTINGS THAN SHOWN'                       
         OI    BDIMSGH+6,X'80'                                                  
         B     DSPPSTX                                                          
         EJECT                                                                  
*---------------------------------------*                                       
* SETSTYPE - SETS TYPBIL FOR SPOT/NET   *                                       
*       NOTE - ORDER IMPORTANT          *                                       
*---------------------------------------*                                       
*                                                                               
SETSTYPE NTR1                                                                   
         XC    SUBTYPE,SUBTYPE                                                  
         L     R1,AIOAREA1                                                      
         USING BILLRECD,R1                                                      
         MVC   TYPBIL,=C'RET'     RETAIL                                        
         CLI   BRETAIL,0                                                        
         BNE   SETSX                                                            
         MVC   TYPBIL,=C'AOR'     AOR                                           
         TM    BILSTAT,X'20'                                                    
         BNO   *+6                                                              
         DC    H'0'               *** SHOULDN'T GET TRUE AOR BILL               
         TM    BILSTAT,X'10'                                                    
         BNO   SETS10                                                           
         TM    BILSTAT,BSTSCOMQ                                                 
         BNO   *+14                                                             
         MVC   SUBTYPE,=C'UAC'                                                  
         B     SETSX                                                            
         TM    BILSTAT,BSTSNETQ                                                 
         BNO   *+14                                                             
         MVC   SUBTYPE,=C'UAN'                                                  
         B     SETSX                                                            
         TM    BILSTAT3,BSTTRCNQ      TRADE WITH AOR                            
         BNO   *+10                                                             
         MVC   SUBTYPE,=C'XA '        XA(POST)                                  
         B     SETSX                                                            
*                                                                               
SETS10   MVC   TYPBIL,=C'REG'     LEAVE AS REGULAR                              
         TM    BILSTAT,BSTSCOMQ                                                 
         BNO   *+14                                                             
         MVC   SUBTYPE,=C'UC '                                                  
         B     SETSX                                                            
         TM    BILSTAT,BSTSNETQ                                                 
         BNO   *+14                                                             
         MVC   SUBTYPE,=C'UN '                                                  
         B     SETSX                                                            
         TM    BILSTAT2,BSTAMAQ                                                 
         BNO   *+14                                                             
         MVC   SUBTYPE,=C'D  '                                                  
         B     SETSX                                                            
         TM    BILSTAT3,BSTTRCNQ        TRADE (NO AOR)                          
         BNO   *+10                                                             
         MVC   SUBTYPE,=C'X  '          X(POST)                                 
         TM    BILSTAT3,BSTMBARQ        GROUPM MIDAS TRADE                      
         BNO   *+10                                                             
         MVC   SUBTYPE,=C'M  '          M(POST)                                 
SETSX    B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
*------------------------------------*                                          
* SETPTYPE - SETS TYPBIL FOR PRINT   *                                          
*       NOTE ORDER IMPORTANT         *                                          
*------------------------------------*                                          
*                                                                               
SETPTYPE NTR1                                                                   
         L     R1,AIOAREA1                                                      
         USING BILLRECD,R1                                                      
         MVC   TYPBIL,=C'FIN'     FINANCIAL BILL                                
         LA    RE,PBILLEL                                                       
         USING PBILOTH,RE                                                       
         SR    R0,R0                                                            
SETP5    CLI   0(RE),0                                                          
         BE    SETP10                                                           
         CLI   0(RE),X'09'                                                      
         BE    SETP8                                                            
         ICM   R0,1,1(RE)                                                       
         AR    RE,R0                                                            
         B     SETP5                                                            
SETP8    TM    PBILLIND,X'01'     FINANCIAL                                     
         BO    SETPX                                                            
*                                                                               
SETP10   MVC   TYPBIL,=C'RET'     RETAIL                                        
         CLI   PBRETAIL,0                                                       
         BNE   SETPX                                                            
         TM    PBILCMSW,X'10'                                                   
         BNO   SETP20                                                           
         MVC   TYPBIL,=C'AOR'     AOR                                           
         TM    PBILCMSW,X'01'                                                   
         BNO   *+14                                                             
         MVC   SUBTYPE,=C'UAC'                                                  
         B     SETPX                                                            
         TM    PBILCMSW,X'08'                                                   
         BNO   SETPX                                                            
         MVC   SUBTYPE,=C'UAN'                                                  
         B     SETPX                                                            
*                                                                               
SETP20   MVC   TYPBIL,=C'REG'     REGULAR (REG/FIN/PROD)                        
         TM    PBILCMSW,X'01'                                                   
         BNO   *+14                                                             
         MVC   SUBTYPE,=C'UC '                                                  
         B     SETPX                                                            
         TM    PBILCMSW,X'08'                                                   
         BNO   *+14                                                             
         MVC   SUBTYPE,=C'UN '                                                  
         B     SETPX                                                            
         TM    PBILSTAT,X'20'           GROUPM MIDAS TRADE                      
         BNO   SETPX                                                            
         MVC   SUBTYPE,=C'M  '          M(POST)                                 
*                                                                               
SETPX    B     EXIT                                                             
         DROP  R1,RE                                                            
         EJECT                                                                  
*---------------------------*                                                   
* CHKPROF - CALL POSTER FOR *                                                   
* PROFILE RECORD READ       *                                                   
*---------------------------*                                                   
*                                                                               
CHKPROF  NTR1                                                                   
         XC    IOKEY,IOKEY        READ DUMMY ACC REC TO SWITCH                  
         MVC   IOKEY(1),COMPANY   TO NATIVE ACC SYSTEM BEFORE CALLING           
         GOTO1 AMIOACC,APPARM,IOHI+IOACCFIL+IO2,=C'SE1'                         
         L     RE,AIOAREA3        CLEAR OUT AREA FOR PROF RECORDS               
         LA    RF,L'IOAREA3                                                     
         XCEFL                                                                  
         LA    RE,PSTBLK          CLEAR CONTROL BLOCK                           
         LA    RF,ACPOSTL                                                       
         XCEFL                                                                  
         LA    R3,PSTBLK                                                        
         USING ACPOSTD,R3                                                       
         MVC   ACPACOM,ACOM       A(COMFACS)                                    
         MVC   ACPSW,VSWITCH      A(SWITCH)                                     
         MVC   ACPPOST,AIOAREA3   A(LATEST PROFILE RECORD RETN'D)               
         MVC   ACPCMPC,COMPANY    NATIVE COMPANY CODE                           
         MVC   ACPCMPC2,COMPANY2  OTHER COMPANY CODE                            
         MVC   ACPSPROD,SVPROD    UNIT,LEDGER                                   
         MVC   ACPMI,SVMI         Y=USE MI RECORDS                              
         MVC   ACPCOST,SVCOST     Y=USE COSTING,BILLING,REVENUE                 
         MVC   ACPSYS,QSYS        SYSTEM                                        
         MVC   ACPALPH,QALPH      AGENCY FOR MEDIA SPLIT FILES                  
         MVC   ACPMED,QMED        MEDIA CODE                                    
         MVC   ACPOFC,SVCOFF      CLIENT OFFICE CODE                            
         MVC   ACPOFG,SVOFFG      OFFICE GROUP                                  
         MVC   ACPCLT,QCLT        CLIENT CODE                                   
         MVC   ACPPRD,QPRD        PRODUCT CODE                                  
         MVC   ACPSE1,SVSE1       NATIVE SYSTEM SE NUMBER                       
         MVC   ACPSE2,SVSE2       OTHER SE NUMBER                               
         MVI   ACPTYPE,1          READ PROF RECORDS                             
         MVI   ACPTYPE2,MPRKTRAD  TRADE BILLING PROF/POST                       
         CLI   SUBTYPE,C'X'       X MEANS TRADE BILL (REG,ADJ OR AOR)           
         BE    CHKPRF30                                                         
*        CLC   =C'REGX',TYPBIL                                                  
*        BE    CHKPRF30                                                         
*        CLC   =C'ADJX',TYPBIL                                                  
*        BE    CHKPRF30                                                         
*        CLC   =C'AORX',TYPBIL    AORXM/AORXA/AORXJ                             
*        BE    CHKPRF30                                                         
         MVI   ACPTYPE2,MPRKRTL   RETAIL TYPE RECORDS                           
         CLC   =C'RET',TYPBIL                                                   
         BE    CHKPRF30                                                         
         MVI   ACPTYPE2,MPRKMTRA  GROUPM MIDAS TRADE                            
         CLC   =C'REG',TYPBIL                                                   
         BNE   *+12                                                             
         CLI   SUBTYPE,C'M'       M MEANS GROUPM TRADE                          
         BE    CHKPRF30                                                         
         MVI   ACPTYPE2,MPRKPPB   PRINT (REG/PROD/FIN)                          
         CLI   ACPSYS,C'P'                                                      
         BE    CHKPRF30                                                         
         MVI   ACPTYPE2,MPRKREG   REG/AOR                                       
*                                                                               
CHKPRF30 GOTO1 ACPOSTER,APPARM,(R3)                                             
         MVI   SCSWSE,0           CLEAR SAVED ACC SYS (ACPOSTER READS           
*                                 MESS IT UP)                                   
         BAS   RE,MAKEPST         CHECK MAKE POSTINGS                           
         MVI   APBYTE,MTPFCC      CC OVERRIDE                                   
         BAS   RE,GETPROF                                                       
         MVC   CCOVER,PROFWORK                                                  
         CLI   CCOVER,0                                                         
         BNE   *+8                                                              
         MVI   CCOVER,C'N'                                                      
         MVI   APBYTE,MTPFIPCT     % OF GROSS FOR INT                           
         BAS   RE,GETPROF                                                       
         MVC   IORPCT,PROFWORK                                                  
         MVI   APBYTE,MTPFPCT      TRADE BILLING PERCENTAGE                     
         BAS   RE,GETPROF                                                       
         MVC   TRDPCT,PROFWORK                                                  
         MVI   APBYTE,MTPFBAS      TRADE BILLING BASIS (GROSS OR INC)           
         BAS   RE,GETPROF                                                       
         MVC   TRDBAS,PROFWORK                                                  
         MVI   APBYTE,MTPFOFF      TRADE BILLING OFFICE                         
         BAS   RE,GETPROF                                                       
         MVC   TRDOFF,PROFWORK                                                  
         MVI   APBYTE,MTPFMOFF     GROUPM MIDAS TRADE BILLING OFFICE            
         BAS   RE,GETPROF                                                       
         MVC   MTRDOFF,PROFWORK                                                 
         MVI   APBYTE,MTPFSPCT     GROUPM MIDAS TRADE SPLIT PERCENTAGE          
         BAS   RE,GETPROF                                                       
         MVC   MTRDPCT,PROFWORK                                                 
*                                                                               
         CLC   =C'AOR',TYPBIL      IF AOR BILL                                  
         BNE   CHKPRF40                                                         
         MVI   APBYTE,MTPFIPT2     AOR % OF GROSS FOR INT                       
         BAS   RE,GETPROF                                                       
         MVC   IORPCT,PROFWORK     SET % OF GROSS FOR INT (AOR BILLS)           
*                                                                               
CHKPRF40 MVI   APBYTE,MTPFSJ      CHECK FOR SJ PRODUCT RECORD                   
         BAS   RE,GETPROF                                                       
         MVC   SJPRD,PROFWORK                                                   
         MVI   APBYTE,MTPFSJ1C    CHECK FOR PRODUCT LVL 1C REC                  
         BAS   RE,GETPROF                                                       
         MVC   SJ1CPRD,PROFWORK                                                 
*                                                                               
         CLC   =C'RET',TYPBIL                                                   
         BNE   CHKPRF50                                                         
         MVI   APBYTE,MTPF3LDG                                                  
         BAS   RE,GETPROF                                                       
         MVC   RLDG,PROFWORK                                                    
         MVI   APBYTE,MTPFRCV                                                   
         BAS   RE,GETPROF                                                       
         MVC   RRCV,PROFWORK                                                    
         MVI   APBYTE,MTPFCST                                                   
         BAS   RE,GETPROF                                                       
         MVC   RCST,PROFWORK                                                    
         MVI   APBYTE,MTPFMGOV                                                  
         BAS   RE,GETPROF                                                       
         MVC   RMGR,PROFWORK                                                    
*                                                                               
CHKPRF50 CLI   ACPSYS,C'P'                                                      
         BNE   CHKPROFX                                                         
         MVI   APBYTE,MTPFRCSJ    POST TO RCVBL SJ                              
         BAS   RE,GETPROF                                                       
         MVC   RCVSJ,PROFWORK                                                   
         MVI   APBYTE,MTPFCLRC    POST TO CLIENT RCVBL                          
         BAS   RE,GETPROF                                                       
         MVC   RCVCLT,PROFWORK                                                  
         MVI   APBYTE,MTPFJBCD    JOB CODE FOR SEP CD                           
         BAS   RE,GETPROF                                                       
         MVC   JBCD,PROFWORK                                                    
         MVI   APBYTE,MTPFSUFX    SUSPENSE SUFFIX                               
         BAS   RE,GETPROF                                                       
         MVC   SUSFX,PROFWORK                                                   
         MVI   APBYTE,MTPFRBFX    REBATE SUFFIX                                 
         BAS   RE,GETPROF                                                       
         MVC   REBFX,PROFWORK                                                   
         MVI   APBYTE,MTPFWKSJ    WORK CODE FOR SJ                              
         BAS   RE,GETPROF                                                       
         MVC   WKSJ,PROFWORK                                                    
         MVI   APBYTE,MTPFNMEM    POST CD MEMO TO SJ                            
         BAS   RE,GETPROF                                                       
         MVC   NOMEMO,PROFWORK                                                  
CHKPROFX B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*--------------------------------------*                                        
* MAKEPST - CHECK MAKE POSTINGS VALUES *                                        
*--------------------------------------*                                        
*                                                                               
MAKEPST  NTR1                                                                   
         MVI   MAKEPOST,X'80'     DDS MAKE POSTINGS =NO                         
         MVI   APBYTE,MTPFDPST                                                  
         BAS   RE,GETPROF                                                       
         OC    PROFWORK,PROFWORK  DDS ALLOWS THEM TO POST?                      
         BZ    MAKEPSTX           NO PROFILE SET UP                             
         CLI   PROFWORK,C'Y'                                                    
         BNE   MAKEPSTX           DDS=N = NO POSTINGS                           
         MVI   MAKEPOST,0         YES DDS ALLOWS POSTING                        
*                                                                               
         MVI   APBYTE,MTPFPST                                                   
         BAS   RE,GETPROF                                                       
         OC    PROFWORK,PROFWORK  THEY THEMSELVES ALLOW POSTING?                
         BNZ   MAKEPST5                                                         
         OI    MAKEPOST,X'40'     NO DDS=Y & AGY=N --> N                        
         B     MAKEPSTX                                                         
*                                                                               
MAKEPST5 CLI   PROFWORK,C'Y'                                                    
         BE    MAKEPSTX           DDS=Y & AGY=Y                                 
         OI    MAKEPOST,X'40'     DDS=Y & AGY=NO                                
MAKEPSTX B     EXIT                                                             
         SPACE 2                                                                
*-----------------------------------*                                           
* GETPROF - GIVEN ROW -GETS PROFILE *                                           
*-----------------------------------*                                           
*                                                                               
GETPROF  NTR1                                                                   
         XC    PROFWORK,PROFWORK                                                
         ZIC   R1,APBYTE                                                        
         BCTR  R1,0                                                             
         SR    RE,RE                                                            
         LA    RF,ACPRTNL                                                       
         MR    RE,R1                                                            
         L     R2,AIOAREA3                                                      
         AR    R2,RF                                                            
         USING ACPRTND,R2                                                       
         OC    0(ACPRTNL,R2),0(R2)                                              
         BZ    *+10                                                             
         MVC   PROFWORK,ACPFVAL                                                 
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*================================================================*              
* DOAOR  - SET DEFAULT AOR ACCOUNTS AND CHECK IF TRUE INFO NEEDED*              
*================================================================*              
*                                                                               
DOAOR    NTR1                     GET TRUE AOR INFO                             
         BAS   RE,GETAOR          NEED TO SET DEFAULTS FIRST                    
         MVI   FNDTRUE,C'N'                                                     
         L     R1,AIOAREA1                                                      
         USING BILLRECD,R1                                                      
         CLI   QSYS,C'P'                                                        
         BNE   DOAOR15                                                          
*                                                                               
         TM    PBILCMSW,X'08'     DON'T FIND TRUE AOR BILL EXCEPT               
         BO    NO                                                               
         CLI   PBILSEP,C'R'                                                     
         BNE   NO                 FOR MAIN CLIENT BILL                          
         BAS   RE,GETPT           SAVE TRUE AOR POSTINGS IF NECESSARY           
         B     DOAOR30                                                          
*                                                                               
DOAOR15  DS    0H                                                               
*        TM    BILSTAT,BSTSADJQ   DON'T FIND TRUE AOR BILL                      
*        BO    NO                 EXCEPT FOR MAIN BILL                          
         TM    BILSTAT,BSTSNETQ                                                 
         BO    NO                                                               
         TM    BILSTAT,BSTSADJQ   FOR AORJ BILLS THERE ARE NO TRUE              
         BO    NO                 PSTNGS SO JUST CALL ACPOSTER DIRECTLY         
         BAS   RE,GETST           SAVE SPOT/NET TRUE PSTGS IF NECESSARY         
*                                                                               
DOAOR30  CLI   FNDTRUE,C'Y'                                                     
         BNE   YES                                                              
         LA    RF,MAXPNUM         RESET FOR MAIN BILL POSTINGS                  
         L     RE,APSTTBL         POSTING ACCOUNTS                              
DOAOR35  XC    0(ACPRTNL,RE),0(RE)                                              
         LA    RE,ACPRTNL(RE)                                                   
         BCT   RF,DOAOR35                                                       
         L     RE,APOSTING        CLEAR POSTINGS AREA                           
         LA    RF,MAXPSTLQ                                                      
         XCEFL                                                                  
         BAS   RE,GETAOR          BUILD SOME ACCOUNTS                           
*                                                                               
         LA    R3,PSTBLK                                                        
         USING ACPOSTD,R3                                                       
         BAS   RE,SETPST                                                        
         MVC   ACPPOST2,AIOAREA2  A(ACTUAL POSTINGS RETURNED)                   
         L     RE,AIOAREA2        CLEAR POSTINGS AREA                           
         LA    RF,L'IOAREA2                                                     
         XCEF                                                                   
         LA    RE,AIOPNUM                                                       
         STC   RE,ACPNUM2                                                       
         MVI   ACPAFLG,C'M'       WANT MAIN BILL POSTINGS                       
         GOTO1 ACPOSTER,APPARM,(R3)                                             
         MVI   SCSWSE,0           CLEAR SAVED ACC SYS(ACP. MESSED UP)           
         DROP  R3                                                               
         L     R3,AIOAREA2                                                      
         USING ACPRTND,R3                                                       
         LA    R0,AIOPNUM                                                       
         L     R2,APOSTING                                                      
DOAOR40  OC    0(ACPRTNL,R3),0(R3)                                              
         BZ    DOAOR45                                                          
         MVC   0(ACPRTNL,R2),0(R3)                                              
         LA    R2,ACPRTNL(R2)                                                   
DOAOR45  LA    R3,ACPRTNL(R3)                                                   
         BCT   R0,DOAOR40                                                       
*                                                                               
         L     R3,AIOAREA3        MOVE IN TRUE POSTINGS RIGHT AFTER             
DOAOR50  OC    0(ACPRTNL,R3),0(R3)                                              
         BZ    YES                                                              
         MVC   0(ACPRTNL,R2),0(R3)                                              
         LA    R2,ACPRTNL(R2)                                                   
         LA    R3,ACPRTNL(R3)                                                   
         B     DOAOR50                                                          
         DROP  R3                                                               
         EJECT                                                                  
*  GET PRINT TRUE AOR BILL, IF NECESSARY                                        
*                                                                               
GETPT    NTR1                                                                   
         L     R1,AIOAREA1                                                      
         USING BILLRECD,R1                                                      
         BAS   RE,CPYBILL         COPY BILL FOR FUDGING                         
         MVC   RUNDT,PBILLDAT     SAVE RUN DATE                                 
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'PBILLKEY),0(R1)                                          
         DROP  R1                                                               
         GOTO1 AMYIO,APPARM,IOHI+IO2,=C'D'                                      
         CLC   IOKEY(L'PBILLKEY),IOKEYSAV                                       
         BE    *+6                                                              
         DC    H'0'                                                             
GETPT05  GOTO1 AMYIO,APPARM,IOSEQ+IO2,=C'D'                                     
         LA    R3,PBILKBNO-PBILLKEY                                             
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         BNE   GETPT15            END OF SET - CHECK HAVE EVERYTHING            
         GOTO1 AMYIO,APPARM,IOGET+IO2,=C'F'                                     
         L     R2,AIOAREA2                                                      
         USING BILLRECD,R2                                                      
         CLC   PBILLDAT,RUNDT     IF SET - RUN DATES MUST MATCH                 
         BNE   GETPT15                                                          
         TM    PBILCMSW,X'20'     TRUE AOR BILL?                                
         BNO   GETPT10                                                          
         BAS   RE,SVPTRUE         SAVE TRUE AOR INFO                            
         MVI   FNDTRUE,C'Y'                                                     
         B     GETPT05                                                          
*                                                                               
GETPT10  TM    PBILCMSW,X'10'     AOR BILL, AT LEAST?                           
         BNO   GETPT15             NO - END OF SET                              
         CLI   PBILSEP,C'R'        YES - BUT BEGINNING OF NEXT SET              
         BE    GETPT15                                                          
         BAS   RE,ADPBIL          ADD BILL TO CREATE FUDGED BILL                
         B     GETPT05                                                          
*                                                                               
GETPT15  CLI   FNDTRUE,C'Y'       IF END OF SET - MUST HAVE FOUND               
         BNE   *+8                                                              
         BAS   RE,DOTRUE          GET TRUE AOR POSTINGS                         
GETPTX   B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO SAVE INFORMATION FROM TRUE AOR PRINT BILL             
*                                                                               
SVPTRUE  NTR1                                                                   
         L     R2,AIOAREA2         R2=A(PRINT BILL)                             
         USING BILLRECD,R2                                                      
         ZAP   TRUEAMT,PBILLRCV                                                 
         SR    RE,RE                                                            
         ICM   RE,3,PBILKBNO       TRUE INVOICE NUMBER                          
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  TRUEINV(6),APDUB                                                 
         MVC   TRUEINV(2),PBILLDAT+2                                            
*                                                                               
         XC    TRUEGST,TRUEGST     CLEAR INPUT GST AMOUNT                       
         MVI   TRUEGSTI,C' '       CLEAR INPUT GST TAX CODE                     
         LA    RE,L'PSTIVALS       CLEAR INPUT PST INFO (CODES,AMOUNTS)         
         MH    RE,=H'10'                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    PSTIVALS(0),PSTIVALS                                             
         LA    RF,PSTIVALS         RF=A(INPUT PST TABLE)                        
*                                                                               
         SR    R0,R0                                                            
         LA    R1,PBILLEL          R1=A(FIRST ELEMENT)                          
SVPTRU20 CLI   0(R1),0                                                          
         BE    SVPTRUX                                                          
         CLI   0(R1),X'0A'                                                      
         BNE   SVPTRU25                                                         
         USING PBILVEL,R1                                                       
         MVC   TRUEGST,PBILLVAT   TRUE AOR GST AMOUNT                           
         MVC   TRUEGSTI,PBILLVCD  TRUE AOR GST CODE                             
         B     SVPTRU30                                                         
*                                                                               
SVPTRU25 CLI   0(R1),X'84'                                                      
         BNE   SVPTRU30                                                         
         USING PBLPSTEL,R1                                                      
         ZIC   RE,PBLPVPRV         PROVINCE NUMBER                              
         BCTR  RE,0                                                             
         MH    RE,=AL2(L'PRVTAB)                                                
         LA    RE,PRVTAB(RE)                                                    
         USING PRVTABD,RE                                                       
         MVC   0(2,RF),PRVTCODE    PST PROVINCE CODE                            
         MVC   2(1,RF),PRVTIN      PST ELEMENT EQUATE                           
         MVC   3(1,RF),PBLPVCOD    PST TAX CODE                                 
         MVC   4(4,RF),PBLPVBAS    PST BASIS                                    
         MVC   8(4,RF),PBLPVAMT    PST AMOUNT                                   
         LA    RF,L'PSTIVALS(RF)   BUMP TO NEXT TABLE ENTRY                     
*                                                                               
SVPTRU30 ICM   R0,1,1(R1)          GET NEXT PRINT ELEMENT                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     SVPTRU20            AND LOOP                                     
*                                                                               
SVPTRUX  B     EXIT                                                             
         DROP  R1,R2,RE                                                         
         EJECT                                                                  
* ADPBIL  - ADDS BILL AMOUNTS IN AIOREA1 TO FUDGED BILL IN XTRAREA              
*                                                                               
ADPBIL   NTR1                                                                   
         L     R2,AIOAREA2        R2=A(PRINT BILL RECORD)                       
         USING BILLRECD,R2                                                      
         ZAP   BILGRS,PBILLGRS                                                  
         ZAP   BILBIL,PBILLBIL                                                  
         ZAP   BILNET,PBILLNET                                                  
         ZAP   BILRCV,PBILLRCV                                                  
         XC    BILGST,BILGST                                                    
         XC    BILPST(10*L'BILPST),BILPST                                       
         LA    RF,BILPST                                                        
*                                                                               
         SR    R0,R0                                                            
         LA    R1,PBILLEL         R1=A(FIRST ELEMENT)                           
ADPBIL5  CLI   0(R1),0                                                          
         BE    ADPBIL15                                                         
         CLI   0(R1),X'0A'        IF GST ELEMENT                                
         BNE   *+14                                                             
         USING PBILVEL,R1                                                       
         MVC   BILGST,PBILLVAT    SET GST AMOUNT                                
         B     ADPBIL8                                                          
         CLI   0(R1),X'84'        IF PST ELEMENT                                
         BNE   ADPBIL8                                                          
         USING PBLPSTEL,R1                                                      
         MVC   0(1,RF),PBLPVPRV   SET PST INFORMATION                           
         MVC   1(4,RF),PBLPVBAS                                                 
         MVC   5(4,RF),PBLPVAMT                                                 
         LA    RF,L'BILPST(RF)                                                  
*                                                                               
ADPBIL8  ICM   R0,1,1(R1)         BUMP TO NEXT ELEMENT IN RECORD                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     ADPBIL5                                                          
         DROP  R1,R2                                                            
*                                                                               
ADPBIL15 LA    R2,XTRAREA         R2=A(FUDGED BILL RECORD)                      
         USING BILLRECD,R2                                                      
         AP    PBILLGRS,BILGRS                                                  
         AP    PBILLBIL,BILBIL                                                  
         AP    PBILLNET,BILNET                                                  
         AP    PBILLRCV,BILRCV                                                  
         LA    R1,PBILLEL         R1=A(FIRST ELEMENT)                           
ADPBIL20 CLI   0(R1),0                                                          
         BE    ADPBILX                                                          
         CLI   0(R1),X'0A'        IF GST ELEMENT                                
         BNE   ADPBIL25                                                         
         USING PBILVEL,R1                                                       
         ICM   RF,15,PBILLVAT     ADD TO GST AMOUNT                             
         ICM   RE,15,BILGST                                                     
         AR    RE,RF                                                            
         STCM  RE,15,PBILLVAT                                                   
         B     ADPBIL30                                                         
*                                                                               
ADPBIL25 CLI   0(R1),X'84'        IF PST ELEMENT                                
         BNE   ADPBIL30                                                         
         USING PBLPSTEL,R1                                                      
         LA    RF,BILPST                                                        
         LA    R0,ACPMCODE                                                      
ADPBIL27 CLC   0(1,RF),PBLPVPRV                                                 
         BE    ADPBIL28                                                         
         LA    RF,L'BILPST(RF)                                                  
         BCT   R0,ADPBIL27                                                      
         DC    H'0'                                                             
ADPBIL28 ICM   RE,15,PBLPVBAS                                                   
         ICM   R3,15,1(RF)                                                      
         AR    RE,R3                                                            
         STCM  RE,15,PBLPVBAS                                                   
         ICM   RE,15,PBLPVAMT                                                   
         ICM   R3,15,5(RF)                                                      
         AR    RE,R3                                                            
         STCM  RE,15,PBLPVAMT                                                   
*                                                                               
ADPBIL30 SR    R0,R0                                                            
         ICM   R0,1,1(R1)         BUMP TO NEXT ELEMENT IN RECORD                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     ADPBIL20                                                         
*                                                                               
ADPBILX  B     EXIT                                                             
         EJECT                                                                  
         DROP  R1,R2                                                            
*  GET SPOT TRUE AOR BILL                                                       
*                                                                               
GETST    NTR1                                                                   
         L     R1,AIOAREA1                                                      
         USING BILLRECD,R1                                                      
         BAS   RE,CPYBILL         COPY BILL FOR FUDGING                         
         MVC   RUNDT,BQDATE                                                     
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'BKEY),0(R1)                                              
         DROP  R1                                                               
         GOTO1 AMYIO,APPARM,IOHI+IO2,=C'D'                                      
         CLC   IOKEY(L'BKEY),IOKEYSAV                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETST05  GOTO1 AMYIO,APPARM,IOSEQ+IO2,=C'D'                                     
         LA    R3,BKEYINV-BKEY                                                  
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         BNE   GETST15            END OF SET - CHECK HAVE EVERYTHING            
         GOTO1 AMYIO,APPARM,IOGET+IO2,=C'F'                                     
         L     R2,AIOAREA2        SPOT/NET TRUE AOR BILL                        
         USING BILLRECD,R2                                                      
         CLC   BQDATE,RUNDT       IF SET - RUN DATES MUST MATCH                 
         BNE   GETST15                                                          
         TM    BILSTAT,X'20'      TRUE AOR BILL?                                
         BNO   GETST10                                                          
         BAS   RE,SVSTRUE         SAVE TRUE AOR INFO                            
         MVI   FNDTRUE,C'Y'                                                     
         B     GETST05                                                          
*                                                                               
GETST10  TM    BILSTAT,X'10'      AOR BILL, AT LEAST?                           
         BNO   GETST15                                                          
         TM    BILSTAT,X'80'                                                    
         BNO   GETST15                                                          
         BAS   RE,ADSBIL          ADD BILL TO CREATE FUDGED BILL                
         B     GETST05                                                          
*                                                                               
GETST15  CLI   FNDTRUE,C'Y'       IF END OF SET - MUST HAVE FOUND               
         BNE   *+8                                                              
         BAS   RE,DOTRUE          GET TRUE AOR POSTINGS                         
GETSTX   B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*              SAVE TRUE SPOT AOR BILL INFO                                     
*                                                                               
SVSTRUE  NTR1                                                                   
         L     R2,AIOAREA2                                                      
         USING BILLRECD,R2                                                      
         MVC   TRUEINV,BINVNO                                                   
         ZAP   TRUEAMT,BACTP        SAVE APPROPRIATE VALUES                     
         XC    TRUEGST,TRUEGST                                                  
         MVI   TRUEGSTI,C' '                                                    
         CLC   BLEN,=H'90'                                                      
         BNH   SVSTRU5                                                          
         CLI   BVATCOD,0                                                        
         BE    SVSTRU5                                                          
         MVC   TRUEGST,BVATAMT                                                  
         MVC   TRUEGSTI,BVATCOD                                                 
*                                                                               
SVSTRU5  LA    RE,L'PSTIVALS       CLEAR INPUT PST INFO (CODES,AMOUNTS)         
         MH    RE,=H'10'                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    PSTIVALS(0),PSTIVALS                                             
         CLC   BLEN,=H'130'        IF NEW BILL LENGTH                           
         BNH   SVSTRUX                                                          
         CLI   BILNPVTS,0          IF ANY PROVINCIAL VAT ELEMENTS               
         BE    SVSTRUX                                                          
*                                                                               
         ZIC   R0,BILNPVTS         R0=(N' OF ELEMENTS)                          
         LA    RF,PSTIVALS         RF=A(INPUT PST CODES,BASIS,AMT)              
         LA    R1,BILPVELD         R1=A(FIRST ELEMENT)                          
         USING BILPVELD,R1                                                      
*                                                                               
SVSTRU8  ZIC   RE,BILPVPRV         PROVINCE NUMBER                              
         BCTR  RE,0                                                             
         MH    RE,=AL2(L'PRVTAB)                                                
         LA    RE,PRVTAB(RE)                                                    
         USING PRVTABD,RE                                                       
         MVC   0(2,RF),PRVTCODE    SAVE PROVINCE CODE                           
         MVC   2(1,RF),PRVTIN      SAVE EQUATED ELEMENT FOR ACCOUNT             
         MVC   3(1,RF),BILPVCOD    SAVE TAX TYPE                                
         MVC   4(4,RF),BILPVBAS    SAVE BASIS AMOUNT                            
         MVC   8(4,RF),BILPVAMT    SAVE TAX AMOUNT                              
         LA    RF,L'PSTIVALS(RF)   BUMP TO NEXT TABLE ENTRY                     
         LA    R1,BILPVLEN(R1)     BUMP TO NEXT ELEMENT IN RECORD               
         BCT   R0,SVSTRU8                                                       
*                                                                               
SVSTRUX  B     EXIT                                                             
         DROP  R2,R1,RE                                                         
         EJECT                                                                  
* ADSBIL  - ADDS BILL AMOUNTS IN AIOREA1 TO FUDGED BILL IN XTRAREA              
*                                                                               
ADSBIL   NTR1                                                                   
         L     R2,AIOAREA2                                                      
         USING BILLRECD,R2                                                      
         ZAP   SBILNET,BNETP       SAVE NET AMOUNT                              
         ZAP   SBILACT,BACTP       SAVE ACTUAL AMOUNT                           
         ZAP   SBILAMT,BGRSP       SAVE BILL AMOUNT                             
                                                                                
         XC    SBILGST,SBILGST     CLEAR GST AND PST INFO                       
         XC    SBILPST(10*L'SBILPST),SBILPST                                    
         CLC   BLEN,=H'90'         IF GST ON BILL                               
         BNH   ADSBIL10                                                         
         CLI   BVATCOD,0                                                        
         BE    ADSBIL10                                                         
         MVC   SBILGST,BVATAMT     SAVE GST AMOUNT                              
*                                                                               
ADSBIL10 CLC   BLEN,=H'130'        IF NEW BILL LENGTH                           
         BNH   ADSBIL15                                                         
         CLI   BILNPVTS,0          IF ANY PROVINCIAL VAT ELES                   
         BE    ADSBIL15                                                         
         ZIC   RE,BILNPVTS                                                      
         LA    R1,BILPVELD         R1=A(FIRST PST ELEMENT)                      
         USING BILPVELD,R1                                                      
         LA    RF,SBILPST          RF=A(PST INFO)                               
ADSBIL14 MVC   0(1,RF),BILPVPRV                                                 
         MVC   1(4,RF),BILPVBAS                                                 
         MVC   5(4,RF),BILPVAMT                                                 
         LA    RF,L'SBILPST(RF)                                                 
         LA    R1,BILPVLEN(R1)                                                  
         BCT   RE,ADSBIL14                                                      
         DROP  R2,R1                                                            
*                                                                               
ADSBIL15 LA    R2,XTRAREA          ADD AMOUNTS TO FUDGED BILL RECORD            
         USING BILLRECD,R2                                                      
         ZAP   APDUB,BNETP         ADD NET                                      
         AP    APDUB,SBILNET                                                    
         ZAP   BNETP,APDUB                                                      
                                                                                
         ZAP   APDUB,BACTP         ADD ACTUAL BILL AMOUNT                       
         AP    APDUB,SBILACT                                                    
         ZAP   BACTP,APDUB                                                      
                                                                                
         ZAP   APDUB,BGRSP         ADD BILL AMOUNT                              
         AP    APDUB,SBILAMT                                                    
         ZAP   BGRSP,APDUB                                                      
                                                                                
         ICM   R1,15,BVATAMT       ADD GST AMOUNT                               
         A     R1,SBILGST                                                       
         STCM  R1,15,BVATAMT                                                    
*                                                                               
         CLC   BLEN,=H'130'        IF NEW BILL LENGTH                           
         BNH   ADSBILX                                                          
         CLI   BILNPVTS,0                                                       
         BE    ADSBILX                                                          
         ZIC   R0,BILNPVTS                                                      
         LA    R1,BILPVELD                                                      
         USING BILPVELD,R1                                                      
*                                                                               
ADSBIL50 LA    RE,ACPMCODE                                                      
         LA    RF,SBILPST                                                       
ADSBIL55 CLC   0(1,RF),BILPVPRV    MATCH ON PROVIDENCE NUMBER                   
         BE    *+14                                                             
         LA    RF,L'SBILPST(RF)                                                 
         BCT   RE,ADSBIL55                                                      
         DC    H'0'                                                             
*                                                                               
         ICM   RE,15,BILPVBAS      ADD BASIS AMOUNT                             
         ICM   R3,15,1(RF)                                                      
         AR    RE,R3                                                            
         STCM  RE,15,BILPVBAS                                                   
         ICM   RE,15,BILPVAMT      ADD PST AMOUNT                               
         ICM   R3,15,5(RF)                                                      
         AR    RE,R3                                                            
         STCM  RE,15,BILPVAMT                                                   
         LA    R1,BILPVLEN(R1)     BUMP TO NEXT ELEMENT IN RECORD               
         BCT   R0,ADSBIL50         LOOP                                         
*                                                                               
ADSBILX  B     EXIT                                                             
         DROP  R2,R1                                                            
         EJECT                                                                  
* COPIES MAIN BILL TO XTRAREA                                                   
*                                                                               
CPYBILL  NTR1                                                                   
         LA    R0,XTRAREA                                                       
         L     RE,AIOAREA1                                                      
         LA    R1,1000           MAX BILL RECORD LENGTH                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     EXIT                                                             
         EJECT                                                                  
         SPACE                                                                  
* CALLS ACPOSTER FOR TRUE AOR POSTINGS, THEN RESETS ACCOUNTS,IOAREAS            
* FOR MAIN BILL CALL TO ACPOSTER                                                
*                                                                               
DOTRUE   NTR1                                                                   
         LA    R3,PSTBLK                                                        
         USING ACPOSTD,R3                                                       
         BAS   RE,SETPST                                                        
         LA    RE,XTRAREA         RESET ADDRESS OF BILL                         
         ST    RE,ACPBILL                                                       
         LA    RE,AIOPNUM                                                       
         STC   RE,ACPNUM2         MAX NUMBER OF POSTINGS                        
         MVI   ACPAFLG,C'T'       LOOKING FOR TRUE POSTINGS                     
         GOTO1 ACPOSTER,APPARM,(R3)                                             
         MVI   SCSWSE,0           CLEAR SAVED ACC SYS(ACP MESSED IT UP)         
         MVC   TRUERR,ACPERR      SAVE POSTING ERRORS IF ANY                    
         MVC   TRUERRC,ACPERRC                                                  
         DROP  R3                                                               
         L     RE,AIOAREA3        CLEAR OUT AREA FOR TRUE AOR POSTINGS          
         LA    RF,L'IOAREA3                                                     
         XCEF                                                                   
         L     R3,APOSTING                                                      
         USING ACPRTND,R3                                                       
         LA    R0,POSTNUM                                                       
         L     R2,AIOAREA3                                                      
DOTRUE20 OC    0(ACPRTNL,R3),0(R3)                                              
         BZ    DOTRUE30                                                         
         MVC   0(ACPRTNL,R2),0(R3)                                              
         LA    R2,ACPRTNL(R2)                                                   
DOTRUE30 LA    R3,ACPRTNL(R3)                                                   
         BCT   R0,DOTRUE20                                                      
         B     EXIT                                                             
         EJECT                                                                  
*=========================================*                                     
* GETAOR - READS AOR SFM RECORD FOR       *                                     
*          DEFAULT ACCOUNT NAMES          *                                     
*=========================================*                                     
*                                                                               
GETAOR   NTR1                                                                   
         CLI   QSYS,C'P'          IF PRINT SYSTEM                               
         BNE   GETAOR2                                                          
         BAS   RE,GETPAOR         GET PRINT AOR RECORD                          
         BNE   GETAORX            NO AOR RECORD                                 
         B     GETAOR20                                                         
*                                                                               
GETAOR2  XC    IOKEY,IOKEY                                                      
         L     R2,AIOAREA1        SPOT/NET TRUE AOR BILL                        
         USING BILLRECD,R2                                                      
         LA    R4,IOKEY                                                         
         USING AORREC,R4                                                        
         MVC   AORKEY(2),=X'0D45'                                               
         MVC   AORKAGMD,BAGYMD                                                  
         MVC   AORKCLT,BCLT                                                     
         MVC   AORKPRD,QPRD                                                     
         MVC   AORKEST,BEST                                                     
*                                                                               
         MVI   AORKDPT,X'FF'       DEFAULT DAYPART                              
         CLI   BLDPT,C' '          TEST BILL FOR ONE DAYPART                    
         BNH   *+10                                                             
         MVC   AORKDPT(1),BLDPT                                                 
*                                                                               
         MVI   AORKSTYP,X'FF'      DEFAULT STATION TYPE                         
         CLI   BLMED,C' '          TEST BILL FOR ONE STATION TYPE               
         BNH   *+10                                                             
         MVC   AORKSTYP(1),BLMED                                                
         MVI   AORKEXT+2,X'FF'     3RD POSITION NOT USED                        
*                                                                               
         GOTO1 AMYIO,APPARM,IOHI+IO2,=C'D'                                      
         CLC   IOKEY(8),IOKEYSAV   TEST THRU PRD                                
         BNE   GETAORX             NOTHING FOR PRODUCT                          
         CLC   IOKEY(10),IOKEYSAV  TEST THRU EST                                
         BE    GETAOR8                                                          
         CLC   IOKEY+8(2),=X'FFFF' DID WE FIND DEFAULT                          
         BE    GETAOR8             YES, USE IT                                  
*                                                                               
GETAOR7  MVC   IOKEYSAV+8(2),=X'FFFF'  TRY DEFAULT EST                          
         MVC   IOKEY,IOKEYSAV                                                   
         GOTO1 AMYIO,APPARM,IOHI+IO2,=C'D'                                      
         CLC   IOKEY(10),IOKEYSAV                                               
         BNE   GETAORX             NOTHING THERE EITHER                         
*                                                                               
GETAOR8  CLC   IOKEY+10(1),IOKEYSAV+10   DAYPART                                
         BE    GETAOR10                                                         
         CLI   IOKEY+10,X'FF'         DID WE FIND DEFAULT                       
         BE    GETAOR10                                                         
         CLI   IOKEYSAV+10,X'FF'      DID WE TRY FOR IT                         
         BE    GETAOR19                                                         
*                                                                               
GETAOR9  MVI   IOKEYSAV+10,X'FF'      TRY DEFAULT                               
         MVC   IOKEY,IOKEYSAV                                                   
         GOTO1 AMYIO,APPARM,IOHI+IO2,=C'D'                                      
         CLC   IOKEY(11),IOKEYSAV                                               
         BNE   GETAOR19              NOTHING                                    
*                                                                               
GETAOR10 CLC   IOKEY+11(1),IOKEYSAV+11 STATION TYPE                             
         BE    GETAOR20                                                         
         CLI   IOKEY+11,X'FF'          DID WE FIND DEFAULLT                     
         BE    GETAOR20                                                         
         CLI   IOKEYSAV+11,X'FF'       DID WE TRY FOR IT                        
         BE    GETAOR18                                                         
         MVI   IOKEYSAV+11,X'FF'       TRY DEFAULT                              
         MVC   IOKEY,IOKEYSAV                                                   
         GOTO1 AMYIO,APPARM,IOHI+IO2,=C'D'                                      
         CLC   IOKEY(12),IOKEYSAV                                               
         BE    GETAOR20                                                         
*                                  NOTHING FOUND                                
GETAOR18 CLI   IOKEYSAV+10,X'FF'   WERE WE LOOKING UNDER ALL DPTS               
         BE    GETAOR19            YES                                          
*                                  NO, DO IT NOW                                
         MVI   IOKEYSAV+11,X'FF'   RESET STATION TYPE                           
         CLI   BLMED,C' '                                                       
         BNH   *+10                                                             
         MVC   IOKEYSAV+11(1),BLMED                                             
         B     GETAOR9                                                          
*                                                                               
GETAOR19 CLC   IOKEYSAV+8(2),=X'FFFF' WERE WE LOOKING UNDER ALL ESTS            
         BE    GETAORX                YES, NOTHING MORE TO DO                   
*                                     NO, DO IT NOW                             
         MVI   IOKEYSAV+10,X'FF'    RESET DAYPART                               
         CLI   BLDPT,C' '                                                       
         BNH   *+10                                                             
         MVC   IOKEYSAV+10(1),BLDPT                                             
         MVI   IOKEYSAV+11,X'FF'    RESET STATION TYPE                          
         CLI   BLMED,C' '                                                       
         BNH   *+10                                                             
         MVC   IOKEYSAV+11(1),BLMED                                             
         B     GETAOR7                                                          
*                                                                               
GETAOR20 GOTO1 =A(AORACCS),APPARM,(RC),RR=APRELO  ACCS FROM AOR REC             
*                                                                               
GETAORX  B     EXIT                                                             
         DROP  R4,R2                                                            
         EJECT                                                                  
*----------------------------------------*                                      
* GETPAOR - READS FOR PRINT AOR RECORD   *                                      
*    EXIT - SETS CC CODE                 *                                      
*----------------------------------------*                                      
*                                                                               
GETPAOR  NTR1                                                                   
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(2),TWAAGY    AGENCY CODE                                   
         OC    QALPH,QALPH                                                      
         BZ    *+10                                                             
         MVC   IOKEY(2),QALPH     AGENCY FOR MEDIA SPLIT FILES                  
         MVC   IOKEY+2(1),QMED    MEDIA CODE                                    
         MVI   IOKEY+3,X'14'      RECORD TYPE                                   
         MVC   IOKEY+4(3),QCLT    CLIENT CODE                                   
         MVC   IOKEY+7(3),QPRD    PRODUCT CODE                                  
         MVC   IOKEY+10(2),BEST   ESTIMATE #                                    
         GOTO1 AMYIO,APPARM,IOHI+IO2,=C'D'                                      
         CLC   IOKEY(25),IOKEYSAV                                               
         BE    YES                                                              
         MVC   IOKEY+10(2),=X'FFFF' READ DEFAULT RECORD                         
         GOTO1 AMYIO,APPARM,IOHI+IO2,=C'D'                                      
         CLC   IOKEY(25),IOKEYSAV                                               
         BE    YES                                                              
         B     NO                                                               
         EJECT                                                                  
*====================================*                                          
* DISKEY - DISPLAY KEY SELECTED      *                                          
*   ONLY EVER CALLED FROM LIST       *                                          
*====================================*                                          
*                                                                               
DISKEY   LH    R1,=Y(IODA1-WORKD) SET D/A FOR READ INTO IOAREA                  
         LA    R1,WORKD(R1)                                                     
         MVC   0(L'IODA1,R1),APRECDA                                            
         L     R2,AIOAREA1                                                      
         USING BILLRECD,R2                                                      
*                                                                               
         CLI   QSYS,C'P'          IF PRINT SYSTEM                               
         BNE   DISK2                                                            
         BAS   RE,DISPRT          DISPLAY KEY PRINT WAY                         
         B     DISKX                                                            
*                                                                               
DISK2    GOTO1 AMYIO,APPARM,IOGET+IO1,=C'F'                                     
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BDISYS,QSYS        MOVE IN SYSTEM                                
         MVC   BDISYS+1(L'QALPH),QALPH                                          
         OI    BDISYSH+6,X'80'                                                  
*                                                                               
         CLI   QSYS,C'N'                                                        
         BNE   DISK10                                                           
         MVI   APBYTE,C'N'        DEFAULT MEDIA FOR NETWORK                     
         CLI   BLMED,C' '                                                       
         BE    *+10                                                             
         MVC   APBYTE,BLMED       MEDIA FILTER                                  
         B     DISK15                                                           
DISK10   MVC   APBYTE,BKEYAM      PASS IT BINARY AGY/MED                        
         BAS   RE,GETMED          GET MEDIA CODE INTO APBYTE                    
DISK15   MVC   BDIMED,APBYTE                                                    
         OI    BDIMEDH+6,X'80'    MEDIA                                         
*                                                                               
         BAS   RE,DISSCLT         DISPLAY CLIENT CODE                           
*                                                                               
         MVC   BDIPRD,BKEYPRD                                                   
         OI    BDIPRDH+6,X'80'    PRODUCT                                       
*                                                                               
         ZIC   RE,BKEYEST                                                       
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  BDIEST,APDUB                                                     
         OI    BDIESTH+6,X'80'    ESTIMATE                                      
*                                                                               
         GOTO1 VDATCON,APPARM,(0,BDATE),(5,BDIRDT)                              
         OI    BDIRDTH+6,X'80'    RUN DATE                                      
*                                                                               
         CLI   BKEYYSRV+1,X'0D'   IF SPECIAL 13TH MONTH                         
         BNE   DISK17                                                           
         MVC   BDISDT(3),=C'13/'                                                
         ZIC   RE,BKEYYSRV                                                      
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  BDISDT+3(2),APDUB                                                
         B     DISK19                                                           
DISK17   MVC   APFULL(2),BKEYYSRV                                               
         MVI   APFULL+2,X'01'                                                   
         GOTO1 VDATCON,APPARM,(3,APFULL),(6,BDISDT)                             
DISK19   OI    BDISDTH+6,X'80'    MONTH OF SERVICE                              
*                                                                               
         GOTO1 =V(SPFMTINO),APPARM,(C'B',0),(6,BINVNO),(QMED,B1PROF),  +        
               B1XPROF,BKEY,RR=Y                                                
         CLC   BINVMED,SPACES     BILL CONTAINS LONG INV # RULES?               
         BNH   DISK20                                                           
         BRAS  RE,FRMINV                                                        
         B     DISK30                                                           
*                                                                               
DISK20   L     R1,APPARM+4        ELSE - USE SHORT INV #                        
         MVC   BDIINV(2),0(R1)                                                  
         MVC   BDIINV+2(4),3(R1)                                                
*                                                                               
DISK30   GOTO1 VDATCON,APPARM,(0,BQDATE),(5,BDIIDT)                             
         OI    BDIIDTH+6,X'80'    INVOICE DATE                                  
*                                                                               
         GOTO1 VDATCON,APPARM,(2,BILPOST),(5,BDITDT)                            
         OI    BDITDTH+6,X'80'    TRANSFER DATE                                 
DISKX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*==========================================*                                    
* GETMED - READS AGENCY RECORD LOOKING FOR *                                    
*          MATCH ON BINARY A/M             *                                    
*          NTRY- BINARY AGY/MED IN APBYTE  *                                    
*          EXIT- MEDIA CODE BACK IN APBYTE *                                    
*==========================================*                                    
*                                                                               
GETMED   NTR1                                                                   
         LA    R2,IOKEY                                                         
         USING AGYRECD,R2                                                       
         XC    AGYKEY,AGYKEY                                                    
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,TWAAGY     AGENCY ID                                     
         OC    QALPH,QALPH                                                      
         BZ    *+10                                                             
         MVC   AGYKAGY,QALPH      AGENCY FOR MEDIA SPLIT FILES                  
         GOTO1 AMYIO,APPARM,IOHI+IO2,=C'D'                                      
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'               SOMETHING WRONG-- MUST FIND RECORD            
         GOTO1 AMYIO,APPARM,IOGET+IO2,=C'F'                                     
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOAREA2                                                      
         LA    R1,AGYEL                                                         
         SR    R0,R0                                                            
GETMED5  CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'               VALUE IN RECORD MUST BE CORRECT               
         CLI   0(R1),2                                                          
         BNE   GETMED10                                                         
         CLC   3(1,R1),APBYTE     BINARY A/M                                    
         BNE   GETMED10                                                         
         MVC   APBYTE,2(R1)                                                     
         B     EXIT                                                             
*                                                                               
GETMED10 ICM   R0,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     GETMED5                                                          
         EJECT                                                                  
*              ROUTINE READS CLIENT RECORD FOR CORRECT CLIENT CODE              
*                                                                               
         USING BILLRECD,R2                                                      
DISSCLT  NTR1                                                                   
**NO-OP**GOTO1 VCLUNPK,APPARM,(SVCPRF7,BKEYCLT),BDICLT                          
*                                                                               
         L     RE,ALSM                                                          
         USING LSMD,RE                                                          
         LA    R1,LSMTWSV                                                       
         AH    R1,LSMTWDSP                                                      
         USING LDISD,R1                                                         
         MVC   BDICLT,LCLT                                                      
         OI    BDICLTH+6,X'80'    CLIENT                                        
         B     EXIT                                                             
         DROP  R2,R1,RE                                                         
         EJECT                                                                  
*==========================================*                                    
* DISPRT - DISPLAY PRINT KEY               *                                    
*==========================================*                                    
*                                                                               
         USING BILLRECD,R2                                                      
DISPRT   NTR1                                                                   
         GOTO1 AMYIO,APPARM,IOGET+IO1,=C'F'                                     
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BDISYS(1),QSYS     SYSTEM AND MEDIA FILE                         
         MVC   BDISYS+1(L'QALPH),QALPH                                          
         OI    BDISYSH+6,X'80'                                                  
*                                                                               
         MVC   BDIMED,PBILKMED    MOVE IN MEDIA                                 
         OI    BDIMEDH+6,X'80'                                                  
*                                                                               
         MVC   BDICLT,PBILKCLT    MOVE CLIENT                                   
         OI    BDICLTH+6,X'80'                                                  
*                                                                               
         MVC   BDIPRD,PBILKPRD    MOVE PRODUCT                                  
         OI    BDIPRDH+6,X'80'                                                  
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,PBILKEST                                                    
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  BDIEST,APDUB       MOVE ESTIMATE                                 
         OI    BDIESTH+6,X'80'                                                  
*                                                                               
         GOTO1 VDATCON,APPARM,(0,PBILLDAT),(5,BDIRDT)                           
         OI    BDIRDTH+6,X'80'    RUN DATE                                      
*                                                                               
         CLI   PBILKMOS+1,X'0D'   IF SPECIAL 13TH MONTH                         
         BNE   DISPRT8                                                          
         MVC   BDISDT(3),=C'13/'                                                
         ZIC   RE,PBILKMOS                                                      
         CVD   RE,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  BDISDT+3(2),APDUB                                                
         B     DISPRT10                                                         
DISPRT8  MVC   APFULL(2),PBILKMOS                                               
         MVI   APFULL+2,1                                                       
         GOTO1 VDATCON,APPARM,(3,APFULL),(6,BDISDT)                             
DISPRT10 OI    BDISDTH+6,X'80'    MONTH OF SERVICE                              
         BAS   RE,SETPINV         SET PRINT INVOICE NUMBER                      
*        MVC   BDIINV(6),APWORK                                                 
         OI    BDIINVH+6,X'80'    INVOICE NUMBER (MONTH-BILL #)                 
         GOTO1 VDATCON,APPARM,(3,PBILINVD),(5,BDIIDT)                           
         OI    BDIIDTH+6,X'80'    INVOICE DATE                                  
         GOTO1 VDATCON,APPARM,(2,PBILPOST),(5,BDITDT)                           
         OI    BDITDTH+6,X'80'    TRANSFER DATE                                 
         B     EXIT                                                             
         EJECT                                                                  
* SETPINV - SETS APWORK(6) TO BILL INVOICE NUMBER                               
SETPINV  NTR1                                                                   
         GOTO1 =V(PPFMTINO),APPARM,(C'B',PBILLDAT),(2,PBILKBNO),       +        
               (QMED,B1PROF),B1XPROF,PBILLKEY,RR=Y                              
*        GOTO1 =V(PPFMTINO),APPARM,(C'B',0),(2,PBILKBNO),(QMED,B1PROF),+        
               B1XPROF,PBILLKEY,RR=Y                                            
         CLC   PBILIMED,SPACES    BILL CONTAINS LONG INV # RULES?               
         BNH   SETPNV20                                                         
         BRAS  RE,FRMINV                                                        
         B     SETPINVX                                                         
SETPNV20 L     R1,APPARM+4        ELSE - USE SHORT INV #                        
         MVC   BDIINV(2),0(R1)                                                  
         MVC   BDIINV+2(4),3(R1)                                                
*                                                                               
*&&DO                                                                           
*        MVC   APWORK(2),PBILLDAT+2                                             
         OC    PBILPOST,PBILPOST   IF BILL TRANSFERRED                          
         BZ    SETPINV6                                                         
         CLC   PBILPOST,=X'BF01'   BEFORE 8/1/95                                
         BNL   SETPINV6                                                         
         CLI   B1XPROF+4,0         CHECK B1X PROFILE                            
         BE    SETPINV5                                                         
         PACK  APDUB,PBILLDAT(2)  YEAR OF BILL                                  
         CVB   R0,APDUB                                                         
         ZIC   RF,B1XPROF+4       INVOICE BASE YEAR                             
         SR    R0,RF                                                            
         BNP   SETPINV5                                                         
         MH    R0,=H'12'                                                        
         PACK  APDUB,PBILLDAT+2(2)                                              
         CVB   RF,APDUB                                                         
         AR    R0,RF                                                            
*                                                                               
SETPINV2 CVD   R0,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  APWORK(2),APDUB    SET NEW INVOICE MONTH                         
         B     SETPINV8                                                         
*                                                                               
SETPINV5 CLI   B1XPROF+5,0        SEE IF BUMPING INV MONTH                      
         BE    SETPINV8                                                         
         PACK  APDUB,PBILLDAT+2(2)                                              
         CVB   RF,APDUB                                                         
         ZIC   R0,B1XPROF+5                                                     
         AR    R0,RF                                                            
         CH    R0,=H'12'                                                        
         BNH   SETPINV2                                                         
         SH    R0,=H'12'                                                        
         B     SETPINV2                                                         
*                                                                               
SETPINV6 OC    PBILIMO,PBILIMO     CHECK NEW FIELD                              
         BZ    *+10                                                             
         MVC   APWORK(2),PBILIMO                                                
*                                                                               
SETPINV8 MVC   APHALF,PBILKBNO                                                  
         LH    R0,APHALF                                                        
         CVD   R0,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  APWORK+2(4),APDUB                                                
*&&                                                                             
SETPINVX B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*=============*                                                                 
* LITERAL POOL*                                                                 
*=============*                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
         DROP  RB,R9,R8                                                         
         EJECT                                                                  
DMREAD   DC    CL7'DMREAD'                                                      
DMWRITE  DC    CL7'DMWRT '                                                      
TEMPSTR  DC    CL7'TEMPSTR'                                                     
PSTMSG   DC    CL21'POSTINGS WILL BE MADE'                                      
NOPSTMSG DC    CL24'NO POSTINGS WILL BE MADE'                                   
NOPSTBPR DC    CL24'NO POSTINGS WILL BE MADE'                                   
MOREPMSG DC    CL24'MORE POSTINGS THAN SHOWN'                                   
NOACCESS DC    CL38'TRANSFER PROGRAM ACCESS NOT AUTHORIZED'                     
NOPREC   DC    CL14'POST/MAINT REC'                                             
NOBPR    DC    CL15'BPOST/MAINT REC'                                            
NOMIREC  DC    CL9'NO MI REC'                                                   
NOCLTREC DC    CL13'NO    CLT REC'                                              
NOPRDREC DC    CL13'NO    PRD REC'                                              
NOPRD1C  DC    CL13'NO PRD LVL 1C'                                              
NOPOST   DC    CL11'POSTINGS=NO'                                                
NOREC    DC    CL13'REC NOT FOUND'                                              
INVPOST  DC    CL23'INVALID ACC FOR POSTING'                                    
NOMATCH  DC    CL20'DEBS DO NOT EQ CREDS'                                       
NOAMTS   DC    CL23'POST/MAINT AMTS MISSING'                                    
NOUNT3   DC    CL12'UNIT 3 ERROR'                                               
NOUNT3RC DC    CL32'POINTER ON UNIT 3 PARTIC MISSING'                           
NOPART   DC    CL12'PARTIC ERROR'                                               
NOSUFFX  DC    CL12'SUFFIX ERROR'                                               
NOADCD   DC    CL9'NO ADCODE'                                                   
NOWKCD   DC    CL12'NO WORK CODE'                                               
INVRET   DC    CL25'INCORRECT RETAIL PROFILES'                                  
INTERR   DC    CL22'INCORRECT INTERNAL PCT'                                     
*                                                                               
NOTRUE   DC    CL29'NO TRUE AOR BILL RECORD FOUND'                              
*                                                                               
* -- ACTRAPRV                                                                   
       ++INCLUDE ACTRAPRV                                                       
         EJECT                                                                  
*----------------------------------------*                                      
* AORACCS - GETS ACCOUNTS FROM AOR RECORD*                                      
*           FOR PRINT/NET/SPOT           *                                      
*----------------------------------------*                                      
*                                                                               
         DS    0D                                                               
AORACCS  NMOD1 0,**AORAC*                                                       
         L     RC,0(R1)                                                         
         USING LOCALD,RC                                                        
*                                                                               
         GOTO1 AMYIO,APPARM,IOGET+IO2,=C'F'                                     
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIOAREA2                                                      
         CLI   QSYS,C'P'                                                        
         BE    AORACC40                                                         
         LR    R2,R4                                                            
         LA    R2,24(R2)          DISP TO FIRST ELEMENT FOR SPOT/NET            
AORACC10 CLI   0(R2),0                                                          
         BE    AORACCX                                                          
         CLI   0(R2),X'03'        AOR INFO ELEM                                 
         BE    AORACC20                                                         
         SR    R0,R0                                                            
         ICM   R0,1,1(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
         B     AORACC10                                                         
*                                                                               
         USING SAORELEM,R2                                                      
AORACC20 MVC   RCVAOR,SAORRCV                                                   
         MVC   COMAOR,SAORCOM                                                   
         B     AORACC70                                                         
         DROP  R2                                                               
*                                                                               
AORACC40 LR    R2,R4                                                            
         LA    R2,33(R2)          DISP TO FIRST ELEMENT FOR PRINT               
AORACC50 CLI   0(R2),0                                                          
         BE    AORACCX                                                          
         CLI   0(R2),X'03'        AOR INFO ELEM                                 
         BE    AORACC60                                                         
         SR    R0,R0                                                            
         ICM   R0,1,1(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
         B     AORACC50                                                         
*                                                                               
         USING PAORELEM,R2                                                      
AORACC60 MVC   RCVAOR,PAORRCV                                                   
         MVC   COMAOR,PAORCOM                                                   
         DROP  R2                                                               
*                                                                               
         USING ACPRTND,R3                                                       
AORACC70 CLC   RCVAOR,SPACES    ACCT MUST BE THERE                              
         BNH   AORACCX                                                          
         MVI   TBYTE,MBTTARI        AOR INCOME ROW                              
         BAS   RE,GETROW          PTS R3 TO CORRECT ROW                         
         MVC   ACPBTYP,TBYTE      SET TYPE                                      
         MVC   ACPLVL,=C'AOR'     LEVEL                                         
         OI    ACPSTAT,ACPCR      CREDIT                                        
         MVC   ACPACC,COMAOR      MOVE IN SFM INCOME ACCOUNT                    
         XC    APWORK,APWORK                                                    
         MVC   APWORK(L'COMAOR),COMAOR                                          
*                                                                               
         MVI   TBYTE,MBTTAPR        AOR PAY/RECEIVABLE ROW                      
         BAS   RE,GETROW          PTS R3 TO CORRECT ROW                         
         MVC   ACPBTYP,TBYTE      SET TYPE                                      
         MVC   ACPLVL,=C'AOR'     LEVEL                                         
         MVC   ACPACC,RCVAOR      MOVE IN SFM RECEIVABLE ROW                    
         CLC   ACPACC(2),=C'SR'                                                 
         BNE   AORACC72                                                         
         OI    ACPSTAT,ACPDEB     RCVBL IS DEBIT                                
         B     AORACC75                                                         
AORACC72 OI    ACPSTAT,ACPCR      CREDIT                                        
*                                                                               
AORACC75 OC    APWORK,APWORK                                                    
         BZ    AORACCX                                                          
         LA    R4,IOKEY                                                         
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY2   OTHER COMPANY CODE (IF APPL)                  
         MVC   ACTKUNT(14),APWORK                                               
         GOTO1 AMIOACC,APPARM,IOACCFIL+IORD+IO2,=C'SE2'                         
         CLI   MYIOERR,0                                                        
         BNE   AORACCX                                                          
         DROP  R4                                                               
         L     R4,AIOAREA2                                                      
         AH    R4,DATADISP                                                      
         USING ABLELD,R4                                                        
AORACC80 CLI   ABLEL,0                                                          
         BE    AORACCX                                                          
         CLI   ABLEL,ABLELQ       BALANCE ELEMENT                               
         BE    AORACC90                                                         
         SR    R0,R0                                                            
         ICM   R0,1,ABLLN                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         B     AORACC80                                                         
*                                                                               
AORACC90 BAS   RE,GETFILT         GET ANALYSIS OR UNIT FILTER                   
*                                                                               
AORACCX  XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*==================================================================*            
* GETFILT  - LOOKS UP 12 CHAR FROM UNIT1= FROM SPECIAL POSTING ELE *            
*            IF NOT FOUND - USES ANALYSIS = AS DEFAULT             *            
*==================================================================*            
*                                                                               
GETFILT  NTR1                                                                   
         L     R4,AIOAREA2                                                      
         AH    R4,DATADISP                                                      
         USING SPAELD,R4                                                        
GETUF3   CLI   0(R4),0            END OF RECORD                                 
         BE    GETAF0                                                           
         CLI   0(R4),SPAELQ       SPECIAL POSTING A/C ELEMENT                   
         BE    GETUF5                                                           
         SR    R0,R0                                                            
         ICM   R0,1,1(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         B     GETUF3                                                           
*                                                                               
GETUF5   CLI   SPATYPE,SPATANAL   ANALYSIS ACCOUNT?                             
         BNE   GETAF0                                                           
*                                                                               
         MVI   TBYTE,MBTTARR        AOR REVENUE ROW                             
         BAS   RE,GETROW          PT R3 TO CORRECT ROW                          
         MVC   ACPBTYP,TBYTE                                                    
         MVC   ACPACC,SPACES                                                    
         MVC   ACPACC(2),=C'12'                                                 
         MVC   ACPACC+2(L'SPAAANAL),SPAAANAL                                    
         MVC   ACPLVL,=C'AOR'     LEVEL                                         
         OI    ACPSTAT,ACPCR      CREDIT                                        
*                                                                               
         MVI   TBYTE,MBTTABL        AOR BILLING ROW                             
         BAS   RE,GETROW          PT R3 TO CORRECT ROW                          
         MVC   ACPBTYP,TBYTE                                                    
         MVC   ACPACC,SPACES                                                    
         MVC   ACPACC(2),=C'11'                                                 
         MVC   ACPACC+2(L'SPAAANAL),SPAAANAL                                    
         MVC   ACPLVL,=C'AOR'     LEVEL                                         
         OI    ACPSTAT,ACPCR      CREDIT                                        
         B     GETFX                                                            
         DROP  R4                                                               
*                                                                               
GETAF0   L     R4,AIOAREA2                                                      
         AH    R4,DATADISP                                                      
         USING RSTELD,R4                                                        
GETAF3   CLI   0(R4),0            END OF RECORD                                 
         BE    GETFX                                                            
*                                                                               
GETAF4   CLI   0(R4),RSTELQ       RECORD STATUS ELE?                            
         BE    GETAF5                                                           
         SR    R0,R0                                                            
         ICM   R0,1,1(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         B     GETAF3                                                           
*                                                                               
GETAF5   CLI   RSTCOSTG,0                                                       
         BE    GETFX                                                            
*                                                                               
         MVI   TBYTE,MBTTARR        AOR REVENUE ROW                             
         BAS   RE,GETROW          PT R3 TO CORRECT ROW                          
         MVC   ACPBTYP,TBYTE                                                    
         MVC   ACPACC,SPACES                                                    
         MVC   ACPACC(2),=C'12'                                                 
         MVC   ACPACC+2(1),RSTCOSTG                                             
         MVC   ACPLVL,=C'AOR'     LEVEL                                         
         OI    ACPSTAT,ACPCR      CREDIT                                        
*                                                                               
         MVI   TBYTE,MBTTABL        AOR BILLING ROW                             
         BAS   RE,GETROW          PT R3 TO CORRECT ROW                          
         MVC   ACPBTYP,TBYTE                                                    
         MVC   ACPACC,SPACES                                                    
         MVC   ACPACC(2),=C'11'                                                 
         MVC   ACPACC+2(1),RSTCOSTG                                             
         MVC   ACPLVL,=C'AOR'     LEVEL                                         
         OI    ACPSTAT,ACPCR      CREDIT                                        
GETFX    B     AORACCX                                                          
         DROP  R4                                                               
         SPACE 2                                                                
GETROW   NTR1                                                                   
         ZIC   R1,TBYTE           ROWNUMBER                                     
         BCTR  R1,0                                                             
         SR    RE,RE                                                            
         LA    RF,ACPRTNL         LENGTH OF TABLE                               
         MR    RE,R1              GET TOTAL DISP FROM START                     
         L     R3,APSTTBL                                                       
         AR    R3,RF              R3 = CORRECT ROW                              
         XIT1  REGS=(R3)                                                        
         EJECT                                                                  
*=======================================================*                       
*  CLEARF - CLEAR AND FOUT FIELDS                       *                       
*                                                       *                       
* ON ENTRY                                              *                       
*        P1    BYTE 0    = STORAGE                      *                       
*        P2    BYTE 0    = 0 UNPROTECTED FIELDS         *                       
*                        = 1 PROTECTED FIELDS           *                       
*              BYTES 1-3 = A(START FIELD HEADER)        *                       
*        P3    BYTES 1-3 = A(END FIELD HEADER)          *                       
*                                                       *                       
*=======================================================*                       
*                                                                               
         DS    0D                                                               
CLEARF   NMOD1 0,**CLR***                                                       
         L     RC,0(R1)                                                         
         USING LOCALD,RC                                                        
*                                                                               
         LM    R2,R3,4(R1)                                                      
         SR    RE,RE                                                            
         LA    R4,X'10'            BRANCH CONDITION                             
         LA    RF,MOVESPA          CLEAR FIELD INSTRUCTION                      
         CLI   4(R1),0             TEST FOR UNPROTECTED FIELDS                  
         BE    *+12                YES                                          
         LA    R4,X'80'            SET BRANCH CONDITION AND CLEAR               
         LA    RF,ZEROFLD          INSTRUCTION FOR PROTECTED FIELDS             
         SPACE 1                                                                
CLEARF2  IC    RE,0(R2)            LENGTH OF FIELD PLUS HEADER                  
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         EX    R4,TSTBRAN          BRANCH ACCORDINGLY                           
         LR    R1,RE                                                            
         SH    R1,=H'9'            SET EXECUTE LENGTH FOR FIELD DATA            
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         SH    R1,=H'8'            LESS 8 MORE FOR EXTENDED FIELD               
         EX    R1,0(RF)            CLEAR FIELD                                  
         OI    4(R2),X'20'         SET VALIDITY BIT                             
         OI    6(R2),X'80'         SET TO TRANSMIT                              
         SPACE 1                                                                
CLEARF4  LA    R2,0(RE,R2)                                                      
         CR    R2,R3               TEST IF END FIELD REACHED                    
         BNH   CLEARF2             NO-CONTINUE                                  
         XIT1                       YES-ALL DONE                                
         SPACE 1                                                                
MOVESPA  MVC   8(0,R2),SPACES                                                   
ZEROFLD  XC    8(0,R2),8(R2)                                                    
TSTBRAN  BC    0,CLEARF4                                                        
         EJECT                                                                  
************************************************************                    
* STRIP OUT DASHES WHEN DISPLAYING THE LONGER INVOICE #                         
* APPARM CONTAINS THE LONG INV #                                                
* CAN BE EITHER 9 OR 10 CHARACTERS LONG                                         
* N-01-1234  OR  01-N-1234   OR   TM-C1-1234   OR C1-TM-1234                    
************************************************************                    
         SPACE 1                                                                
FRMINV   NTR1  BASE=*,LABEL=*                                                   
         MVC   APWORK,SPACES                                                    
         LA    RF,APWORK                                                        
         L     R1,APPARM                                                        
         LHI   RE,10                                                            
         CLI   5(R1),C'-'         IS THE 5TH CHAR A - ?                         
         BE    *+8                IF SO THEN INV # IS 10 BYTES                  
         LHI   RE,9               ELSE IT'S 9 BYTES.                            
         STC   RE,APBYTE           STORE THE LENGTH                             
FRMINV10 CLI   0(R1),C'-'                                                       
         BE    *+14               SKIP                                          
         MVC   0(1,RF),0(R1)                                                    
         AHI   RF,1                                                             
         AHI   R1,1                                                             
         BCT   RE,FRMINV10                                                      
*                                                                               
FRMINV20 ZIC   RF,APBYTE                                                        
         AHI   RF,-3               ONE FOR EX PLUS 2 FOR THE DASHES             
         EXMVC RF,BDIINV,APWORK                                                 
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
* -- ACTRAWRK                                                                   
*                                                                               
       ++INCLUDE ACTRAWRK                                                       
         EJECT                                                                  
LOCALD   DSECT                                                                  
         DS    0A                                                               
APOSTING DS    A                                                                
*                                                                               
MAXPSTLQ EQU   4000                                                             
POSTNUM  EQU   (MAXPSTLQ)/ACPRTNL                                               
AIOPNUM  EQU   (L'IOAREA3)/ACPRTNL                                              
*                                                                               
TBYTE    DS    XL1                                                              
MOREPOST DS    XL1                                                              
SIDE2    DS    CL1                                                              
PASS     DS    CL2                                                              
TYPBIL   DS    CL3                                                              
SUBTYPE  DS    CL3                                                              
FNDTRUE  DS    CL1                Y= TRUE AOR BILL READ                         
PRNTD    DS    CL1                                                              
RLDG     DS    CL1                UNIT 3 LEDGER                                 
RRCV     DS    CL1                Y= RECEIVABLE ACCOUNT OVERRIDE                
RCST     DS    CL1                Y = COST ACCOUNT OVERRIDE                     
RMGR     DS    CL4                MARKET GROUP OVERRIDE                         
SECREC   DS    CL1                Y = 2ND POST DETAIL RECORD                    
CCOVER   DS    CL1                N,1,2,3,4 CC= ON A INCOME ACCOUNT             
SUSFX    DS    CL5                SUFFIX FOR SUSPENSE                           
REBFX    DS    CL5                SUFFIX FOR REBATE                             
RUNDT    DS    CL6                RUN DATE OF BILL                              
SJPRD    DS    CL1                CHECK FOR SJ PRODUCT                          
SJ1CPRD  DS    CL1                CHECK FOR PRODUCT LVL 1C                      
BILMONTH DS    CL1                BILL MONTH(SPOT/NET) Y/M IN 1 BYTE            
RCVCLT   DS    CL1                USE RCVBL ACC FROM CLT TO POST                
RCVSJ    DS    CL1                POST TO SJ RCVBL ACC                          
JBCD     DS    CL6                JOB CODE FOR SEP CD                           
WKSJ     DS    CL2                WORK CODE FOR SJ                              
NOMEMO   DS    CL1                POST CD MEMO TO SJ                            
GSTO     DS    CL1                OUTPUT GST CODE                               
PSTOVALS DS    10CL12             OUTPUT PST INFORMATION                        
MAKEPOST DS    CL1                DDS MAKE POSTINGS                             
LINENUM  DS    CL1                LINE NUMBER COUNT                             
APBYTEC  DS    CL1                                                              
TRUERR   DS    CL1                ERROR MSGS FOR TRUE AOR POSTINGS              
TRUERRC  DS    CL1                                                              
SKEY     DS    CL(L'IOKEY)        SAVED KEY                                     
IORPCT   DS    XL4                % OF GROSS FOR IOR (REG OR AOR)               
TRDPCT   DS    XL4                TRADE PERCENTAGE (XPROF)                      
TRDBAS   DS    CL1                TRADE BASIS-GROSS OR NET (XPROF)              
TRDOFF   DS    CL2                TRADE OFFICE (XPROF)                          
MTRDOFF  DS    CL2                GROUPM MIDA TRADE OFFICE (MPROF)              
MTRDPCT  DS    XL4                GROUPM MIDA TRADE SPLIT PERCENTAGE            
TRUEAMT  DS    PL6                TRUE AOR BILL ACTUAL AMOUNT                   
TRUEGST  DS    XL4                TRUE AOR INPUT GST AMOUNT                     
TRUEGSTI DS    CL1                INPUT GST CODE                                
TRUEINV  DS    CL6                TRUE AOR BILL INVOICE AMOUNT                  
PSTIVALS DS    10CL12             INPUT PST INFORMATION                         
SVACC    DS    CL14                                                             
RCVAOR   DS    CL14                                                             
COMAOR   DS    CL14                                                             
PROFWORK DS    CL40                                                             
*                                 PRINT BILL TEMPORARY AMTS                     
BILGRS   DS    PL6                                                              
BILBIL   DS    PL6                                                              
BILNET   DS    PL6                                                              
BILRCV   DS    PL6                                                              
BILGST   DS    XL4                                                              
BILPST   DS    10XL9                                                            
*                                                                               
         DS    0D                 SPOT BILL TEMPORARY AMTS                      
SBILGST  DS    XL4                                                              
SBILPST  DS    10XL9                                                            
SBILAMT  DS    PL6                                                              
SBILNET  DS    PL6                                                              
SBILACT  DS    PL6                                                              
*                                                                               
DEBS     DS    PL6                                                              
CREDS    DS    PL6                                                              
XTRAREA  DS    CL1000                                                           
         EJECT                                                                  
*                                                                               
* DISD - DSECT TO COVER TRACE SCREEN                                            
*                                                                               
DISD     DSECT                                                                  
         DS    CL8                                                              
DACC     DS    CL14               ACCOUNT                                       
         DS    CL1                                                              
DAMT     DS    CL11               AMOUNT                                        
DTYPE    DS    CL1                TYPE                                          
         DS    CL1                                                              
DMEMO    DS    CL11               MEMO                                          
         EJECT                                                                  
*                                                                               
* PSTELD - DSECT TO COVER SPOT/PRINT PST ELEMENT IN BILL RECORD                 
*                                                                               
PSTELD   DSECT                                                                  
PSTPVPRV DS    XL1                PROVINCE NUMBER                               
PSTPVCTL DS    XL1                                                              
PSTPVCOD DS    CL1                TAX CODE                                      
PSTPVAMT DS    CL4                AMOUNT                                        
PSTPVBAS DS    CL4                BASIS AMOUNT                                  
         DS    XL2                                                              
         SPACE                                                                  
*                                                                               
* SAORELEM - DSECT TO COVER SPOT AOR SFM ELEMENT INTERESTED IN X'03'            
*                                                                               
SAORELEM DSECT                                                                  
SAORELE  DS    XL1'03'                                                          
SAORLN   DS    XL1                                                              
         DS    CL12               STUFF                                         
SAORRCV  DS    CL14               DISPL TO RCVBL ACCOUNT                        
SAORCOM  DS    CL14               DISPL TO COMMISSION ACCOUNT                   
         SPACE                                                                  
*                                                                               
* PAORELEM - DSECT TO COVER PRINT AOR SFM ELEMENT INTERESTED IN X'03'           
*                                                                               
PAORELEM DSECT                                                                  
PAORELE  DS    XL1'03'                                                          
PAORLN   DS    XL1                                                              
         DS    CL12               STUFF                                         
PAORRCV  DS    CL14               DISPL TO RCVBL ACCOUNT                        
PAORCOM  DS    CL14               DISPL TO COMMISSION ACCOUNT                   
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   TRAPTGH                                                          
       ++INCLUDE ACTRAEBD                                                       
         SPACE 2                                                                
         ORG                                                                    
*========================*                                                      
* OTHER VARIOUS DSECTS   *                                                      
*========================*                                                      
* -- SPGENBILL                                                                  
* -- PBILLREC                                                                   
* -- SPGENAGY                                                                   
* -- SPGENAOR                                                                   
* -- SPGENAOR                                                                   
*                                                                               
         PRINT OFF                                                              
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         ORG BILLRECD                                                           
       ++INCLUDE PBILLREC                                                       
*                                                                               
AGYRECD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
*                                                                               
       ++INCLUDE ACTRA07D                                                       
*                                                                               
       ++INCLUDE SPGENAOR                                                       
       ++INCLUDE PBLPSTEL                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'105ACTRA08   02/15/19'                                      
         END                                                                    
