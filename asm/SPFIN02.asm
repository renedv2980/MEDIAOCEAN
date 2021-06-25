*          DATA SET SPFIN02    AT LEVEL 052 AS OF 11/14/06                      
*PHASE T20902A                                                                  
         SPACE 2                                                                
**************************************************************                  
*                                                            *                  
* 01DEC99 MZEI GLOBBER CALL TO GET %PAID AND RETURN TO       *                  
*         SUPERDESK                                          *                  
**************************************************************                  
         TITLE 'FIN02 T20902  FINANCIAL INFORMATION - FIN OVERLAY'              
T20902   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20902**,RR=RE                                                 
*                                                                               
         USING TWAD,R5                                                          
         USING SAVAREA,R6                                                       
         LA    R8,2048(R7)                                                      
         LA    R8,2048(R8)                                                      
         USING WORKD,R7,R8                                                      
         L     RC,APALOCAL                                                      
         USING LOCALD,RC                                                        
         ST    RB,APBASE1                                                       
         ST    RB,APNTRYA                                                       
         ST    RD,APWORKA                                                       
         ST    RE,APRELO                                                        
*                                                                               
         CLI   APMODE,APMVALK        VALIDATE KEY (ACTION DIS)                  
         BE    VALKEY                                                           
         CLI   APMODE,APMDISK        DISPLAY KEY (ACTION SELECT)                
         BE    DISKEY                                                           
         CLI   APMODE,APMFRP         FIRST FOR REPORT (ACTION DIS)              
         BE    FSTRP                                                            
         CLI   APMODE,APMDISR        DISPLAY RECORD (ACTION DIS/SELECT)         
         BE    DISREC                                                           
         B     EXIT                                                             
*                                                                               
YES      CR    RB,RB                                                            
         B     EXIT                                                             
NO       LTR   RB,RB                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
*===============*                                                               
* VALIDATE KEY  *                                                               
*===============*                                                               
*                                                                               
VALKEY   LA    R2,APRECKEY        DISPLAY KEY                                   
         USING APRECD,R2                                                        
*                                                                               
         OI    FDTHDLNH+1,X'20'   PROTECT AND                                   
         OI    FDTHDLNH+6,X'80'   TRANSMIT IT                                   
         OI    FDTHDL2H+6,X'80'   TRANSMIT IT                                   
         MVC   FDTHDLN(7),=CL7'MONTH'                                           
         TM    INOIND,INOIWK                                                    
         BNO   *+10                                                             
         MVC   FDTHDLN(7),=CL7'WEEK'                                            
*                                                                               
         MVC   FDTHDL2+13(19),SPACES                                            
         TM    INOIND2,INOIBIL                                                  
         BNO   *+10                                                             
         MVC   FDTHDL2+13(19),=CL19'BILLED     BILLABLE'                        
*                                                                               
         GOTO1 AVALKEYD           VALIDATE KEY                                  
         BNE   VALKX                                                            
*                                 SET SWLIST TO Y IF REALLY LIST                
         OI    APINDS,APIOKDIS                                                  
         BAS   RE,SETSW           SET SWLIST                                    
         CLI   SWLIST,C'Y'        IF REQUEST IS REALLY A LIST                   
         BE    VALKX              EXIT                                          
         CLC   SKEY,APRECKEY      IF KEY OR                                     
         BNE   VALK15                                                           
         CLC   INOSPL(L'INOLEN),SVOPTS   IF ANY OF THE OPTIONS CHANGE           
         BE    VALKX                                                            
         OC    LISTFLD,LISTFLD       AND WE ARE IN SELECT MODE                  
         BZ    VALK15                                                           
         MVI   SWLIST,C'D'           CHANGE IN OPTIONS=GO TO DISPLAY            
         B     VALKX                                                            
*                                                                               
VALK15   MVC   SKEY,APRECKEY       SAVE NEW VALUES                              
         MVC   SVOPTS(L'INOLEN),INOSPL                                          
         XC    DSPLIST,DSPLIST     AND START FROM TOP AGAIN                     
*                                                                               
VALKX    TM    TWAMODE,TWAMLSM                                                  
         BNO   EXIT                                                             
         OC    SAVRECK,SAVRECK    ANY THING IN SAVE KEY?                        
         BZ    EXIT               NO -1ST TIME IN -SEL REC NOT YET DISP         
         CLC   APRECKEY,SAVRECK   SEL REC DISP-KEY CHANGE NOW?                  
         BE    EXIT               NO  - WANT TO GO BACK TO LIST                 
         MVC   SAVRECK,APRECKEY   YES - WANT NEW KEY DISPLAYED                  
         CLI   SWLIST,C'Y'        SWITCHING TO A DIFF LIST                      
         BE    *+12                                                             
         MVI   SWLIST,C'D'        SWITCH FROM SELECT TO DISPLAY                 
         B     EXIT                                                             
*                                                                               
         MVI   LISTYPE,0          GET RID OF ALL SIGNS OF CURRENT               
         XC    LISTFLD,LISTFLD    LIST                                          
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO SET SWLIST                                            
SETSW    NTR1                                                                   
         MVI   SWLIST,C'Y'        REQUEST - SO GENERAL WILL KNOW                
         OC    APPRD,APPRD                                                      
         BZ    SETSWX                                                           
         CLI   APMOSFLG+1,C'L'                                                  
         BE    SETSWX                                                           
         CLC   =C'LST',APEST                                                    
         BE    SETSWX                                                           
         MVI   SWLIST,C'N'                                                      
SETSWX   B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*===============*                                                               
* DISPLAY KEY   *                                                               
*===============*                                                               
*                                                                               
DISKEY   LA    R2,APRECKEY         DISPLAY KEY                                  
         USING APRECD,R2                                                        
*                                                                               
         XC    LISTFLD,LISTFLD                                                  
         TM    TWAMODE,TWAMLSM     IF LIST MODE                                 
         BNO   DISKX                                                            
         BAS   RE,SETFLD           SET LISTFLD                                  
*                                                                               
         BAS   RE,DSPMED           DISPLAY MEDIA                                
         BAS   RE,DSPCLT           DISPLAY CLIENT                               
         BAS   RE,DSPPRD           DISPLAY PRODUCT                              
         BAS   RE,DSPEST           DISPLAY ESTIMATE                             
         BAS   RE,DSPDT            DISPLAY DATES                                
         BAS   RE,DSPMOS           DISPLAY MARKET/STATION                       
*                                                                               
DISKX    B     EXIT                                                             
         SPACE                                                                  
*              ROUTINE TO SET LISTFLD                                           
SETFLD   NTR1                                                                   
         CLI   LISTYPE,C'P'       IF PRODUCT LIST                               
         BNE   *+14                                                             
         MVC   LISTFLD,FINPRD      SAVE PRODUCT                                 
         B     SETFLDX                                                          
*                                                                               
         CLI   LISTYPE,C'E'       IF ESTIMATE LIST                              
         BNE   *+14                                                             
         MVC   LISTFLD,FINEST     SAVE ESTIMATE FIELD                           
         B     SETFLDX                                                          
*                                                                               
         MVC   LISTFLD,FINMOS     SAVE MKT/STA LIST FIELD                       
SETFLDX  B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO DISPLAY MEDIA                                         
DSPMED   NTR1                                                                   
         GOTO1 AGETMED,APPARM,APMED                                             
         BNE   *+10                                                             
         MVC   FINMED(L'QMED),QMED                                              
         OI    FINMEDH+6,X'80'    XMIT MEDIA                                    
         MVC   FINMEDN(L'MEDNM),MEDNM                                           
         OI    FINMEDNH+6,X'80'   AND MEDIA NAME                                
         B     EXIT                                                             
         SPACE                                                                  
*              ROUTINE TO DISPLAY CLIENT                                        
DSPCLT   NTR1                                                                   
         GOTO1 AGETCLT,APPARM,APCLT                                             
         BNE   *+10                                                             
         MVC   FINCLT(L'QCLT),QCLT                                              
         OI    FINCLTH+6,X'80'    XMIT CLIENT                                   
         MVC   FINCLTN(19),CLTNM                                                
         OI    FINCLTNH+6,X'80'   AND CLIENT NAME                               
         B     EXIT                                                             
         SPACE                                                                  
*              ROUTINE TO DISPLAY PRODUCT                                       
DSPPRD   NTR1                                                                   
         MVC   FINPRD,SPACES                                                    
         CLC   =C'ALL',APPRD                                                    
         BE    *+14                                                             
         CLC   =C'UNA',APPRD                                                    
         BNE   DSPPRD1                                                          
         MVC   FINPRD(L'APPRD),APPRD                                            
         MVC   PRDNM,SPACES                                                     
         B     DSPPRD2                                                          
*                                                                               
DSPPRD1  GOTO1 AGETPRD,APPARM,APPRDB1,APPRDB2                                   
         BNE   *+10                                                             
         MVC   FINPRD(L'QPRD),QPRD                                              
DSPPRD2  OI    FINPRDH+6,X'80'    XMIT PRODUCT NUMBER 1                         
         OC    QPRD2,QPRD2                                                      
         BZ    DSPPRD5                                                          
         MVI   FINPRD+3,C'-'            AND PRODUCT 2 IF ANY                    
         MVC   FINPRD+4(3),QPRD2                                                
DSPPRD5  MVC   FINPRDN(L'PRDNM),PRDNM   PRODUCT NAME FOR PROD # 1               
         OI    FINPRDNH+6,X'80'                                                 
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO DISPLAY ESTIMATE                                      
DSPEST   NTR1                                                                   
         MVC   FINEST,SPACES                                                    
         MVC   FINEST(L'APEST),APEST       GET ESTIMATE DETAILS                 
         CLC   =C'ALL',APEST                                                    
         BNE   *+14                                                             
         MVC   ESTNM,SPACES       NO ESTIMATE NAME FOR ALL ESTIMATES            
         B     DSPEST10                                                         
*                                                                               
         CLC   =C'NO',APEST                                                     
         BNE   DSPEST5                                                          
         MVC   ESTNM,SPACES       NO EST NAME FOR NO ESTIMATES                  
         OC    QSEPFLT,QSEPFLT                                                  
         BZ    DSPEST10                                                         
         LA    R0,L'QSEPFLT                                                     
         LA    R1,QSEPFLT                                                       
         LA    RE,FINEST+2                                                      
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
DSPEST1  CLI   0(R1),C'*'                                                       
         BE    DSPEST2                                                          
         CLI   0(R1),C' '                                                       
         BE    DSPEST2                                                          
         TM    0(R1),X'40'        NEGATIVE FILTER                               
         BO    *+12                                                             
         MVI   0(RE),C'-'                                                       
         LA    RE,1(RE)                                                         
         MVC   0(1,RE),0(R1)                                                    
         OI    0(RE),X'40'        INSURE UPPER CASE                             
*                                                                               
DSPEST2  LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,DSPEST1                                                       
         B     DSPEST10                                                         
*                                                                               
DSPEST5  GOTO1 AGETEST,APPARM,APESTB1,APESTB2                                   
         BNE   *+10                                                             
         MVC   FINEST(L'QEST),QEST                                              
         OC    QEST2,QEST2        SECOND ESTIMATE NUMBER?                       
         BZ    DSPEST10                                                         
         MVI   FINEST+3,C','      YES - MOVE IT OUT                             
         MVC   FINEST+4(3),QEST2                                                
         MVC   ESTNM,SPACES       NO EST NAME FOR RANGE OF ESTIMATES            
DSPEST10 OI    FINESTH+6,X'80'                                                  
         MVC   FINESTN(19),ESTNM                                                
         OI    FINESTH+6,X'80'                                                  
         OI    FINESTNH+6,X'80'                                                 
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO DISPLAY START AND END DATES                           
DSPDT    NTR1                                                                   
         MVC   BSTDT,APSTDT                                                     
         CLI   BSTDT+2,0                                                        
         BNE   *+8                                                              
         MVI   BSTDT+2,1                                                        
         GOTO1 VDATCON,APPARM,(3,BSTDT),(0,QSTDT)                               
         GOTO1 VDATCON,APPARM,(0,QSTDT),(6,APWORK)                              
         MVC   BNDDT,APENDT                                                     
         CLI   BNDDT+2,0                                                        
         BNE   *+8                                                              
         MVI   BNDDT+2,1                                                        
         GOTO1 VDATCON,APPARM,(3,BNDDT),(0,QNDDT)                               
         GOTO1 VDATCON,APPARM,(0,QNDDT),(6,APWORK+6)                            
         CLI   APESTDT,C'N'       USING ESTIMATE DATES?                         
         BE    DSPDT15                                                          
         MVC   FINSDT(2),=C'ES'       YES                                       
         OI    FINSDTH+6,X'80'                                                  
         MVC   FINSDTN(L'QSTDT),APWORK                                          
         OI    FINSDTNH+6,X'80'                                                 
         MVC   FINEDTN(L'QNDDT),APWORK+6                                        
         OI    FINEDTNH+6,X'80'                                                 
         B     DSPDTX                                                           
*                                                                               
DSPDT15  MVC   FINSDT(L'QSTDT),APWORK  PUT OUT REQUEST DATES                    
         OI    FINSDTH+6,X'80'                                                  
         MVC   FINEDT(L'QNDDT),APWORK+6                                         
         OI    FINEDTH+6,X'80'                                                  
*                                                                               
DSPDTX   B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO DISPLAY MARKET/STATION                                
DSPMOS   NTR1                                                                   
         XC    FINMOS,FINMOS                                                    
         MVC   FINMOSN,SPACES                                                   
         MVC   FINMOS(3),=C'ALL'                                                
         CLC   =C'AL',APMKT                                                     
         BE    DSPMOSX                                                          
         SR    R1,R1                                                            
         ICM   R1,3,APMKT                                                       
         BNZ   DSPMOS3                                                          
         CLI   APMOSFLG+1,C'L'    MKT/STA LIST CAN HAVE                         
         BNE   DSPMOS5                                                          
         MVC   FINMOS(4),=X'F0F0F0F0'                                           
*                                                                               
DSPMOS3  CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  FINMOS(4),APDUB      MOVE IN MARKET                              
DSPMOS5  CLC   =C'ALL',APSTA                                                    
         BE    DSPMOSX                                                          
         GOTO1 AGETMOS,APPARM,APMKT,APSTA,APMOSFLG                              
         BNE   DSPMOSX                                                          
         CLI   APMOSFLG+1,C'L'                                                  
         BE    DSPMOS10                                                         
         CLI   APMOSFLG,C'S'                                                    
         BE    DSPMOS10                                                         
         MVC   FINMOS(L'QMKT),QMKT                                              
         B     DSPMOSX                                                          
*                                                                               
DSPMOS10 MVC   FINMOS(L'QSTA),QSTA                                              
         CLI   FINMOS,C'0'          IF CABLE STATION                            
         BL    *+8                                                              
         MVI   FINMOS+4,C'/'        INDICATE SO                                 
         CLI   AGYPRF7,C'C'         IF CANADIAN                                 
         BNE   DSPMOS15                                                         
         CLI   QSTACNET,C' '        CABLE IS DIFFERENT                          
         BNH   DSPMOS12                                                         
         CLI   QMED,C'N'                                                        
         BE    *+12                                                             
         CLI   QMED,C'C'                                                        
         BNE   DSPMOS12                                                         
         LA    RF,FINMOS+3                                                      
         CLI   0(RF),C' '                                                       
         BE    *+8                                                              
         LA    RF,1(RF)                                                         
         MVI   0(RF),C'/'           APPEND CANADIAN CABLE SUFFIX                
         MVC   1(2,RF),QSTACNET                                                 
         B     DSPMOSX                                                          
DSPMOS12 CLI   FINMOS+3,C' '        IF 3 LETTER STATION                         
         BNE   *+12                                                             
         MVI   FINMOS+4,C' '        JUST DISPLAY 3 LETTERS                      
         B     DSPMOSX                                                          
         CLI   FINMOS+4,C'N'        IF NETWORK                                  
         BE    DSPMOSX              DISPLAY ALL FIVE                            
         CLI   QMED,C'R'                                                        
         BE    DSPMOSX                                                          
         MVI   FINMOS+4,C' '        OTHERWISE DISPLAY 4                         
         B     DSPMOSX                                                          
*                                                                               
DSPMOS15 CLI   FINMOS+3,C' '                                                    
         BNE   DSPMOSX                                                          
         MVI   FINMOS+3,C'-'                                                    
*                                                                               
DSPMOSX  OI    FINMOSH+6,X'80'    XMIT MOS FIELDS                               
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE FIRST FOR REPORT                                         
FSTRP    CLI   SWLIST,C'Y'        IF LIST REQUEST                               
         BE    FSTRX              -EXIT                                         
         CLI   SWLIST,C'D'        SWITCH TO DISPLAY (FROM SELECT)               
         BE    FSTRX                                                            
         TM    INOIND,INOIWK                                                    
         BNO   FSTR5                                                            
         BAS   RE,CLRWBLK                                                       
         B     FSTR8                                                            
FSTR5    GOTO1 ACLRBLK,BLKLBL     CLEAR BLOCK                                   
*                                                                               
FSTR8    TM    TWAMODE,TWAMLSM                                                  
         BO    FSTRX                                                            
         TM    INOIND,INOIWK           IF WEEKLY                                
         BNO   FSTRX                                                            
         BAS   RE,CALLSP               CALL SPOTIO                              
FSTRX    B     EXIT                                                             
         SPACE 3                                                                
*              ROUTINE TO CALL SPOTIO                                           
CALLSP   NTR1                                                                   
         MVI   LISTYPE,0                                                        
         GOTO1 AINTSPBK                INITIALIZE SPOTBLOCK                     
         LA    R1,IOHOOK                                                        
         ST    R1,SBIOHOOK             SPOTIO MUST HOOK HERE                    
         GOTO1 VSPOTIO,APPARM,SBLOCK   *** CALL SPOTIO **                       
         B     EXIT                                                             
         EJECT                                                                  
*================*                                                              
* DISPLAY RECORD *                                                              
*================*                                                              
*                                                                               
DISREC   CLI   SWLIST,C'Y'         IF LIST REQUEST                              
         BNE   *+12                                                             
         BAS   RE,SETLST           SET INDICATORS FOR LIST                      
         B     DISRX                                                            
         CLI   SWLIST,C'D'         SWITCH TO DISPLAY                            
         BNE   *+12                                                             
         BAS   RE,SETDSP           SET INDICATORS FOR DISPLAY                   
         B     DISRX                                                            
*                                                                               
         LA    R9,NLINES                                                        
         TM    TWAMODE,TWAMLSM                                                  
         BO    DISR10                                                           
*                                                                               
         MVI   PREVDATA,0         FLAG IND 1ST TIME - DO PREV DATA              
         TM    INOIND,INOIWK                                                    
         BO    DISR50                                                           
         GOTO1 ACLRBLK,BLKLBL     FOR REGULAR MONTHLY DISPLAY                   
         BAS   RE,CALLSP          CALL SPOTIO EVERY TIME                        
         XC    DSPLIST,DSPLIST                                                  
         B     DISR50                                                           
*                                                                               
DISR10   OC    DSPLIST,DSPLIST                                                  
         BZ    DISR11             FIRST TIME FOR LIST                           
         XC    DSPLIST,DSPLIST                                                  
         MVI   APMODE,APMLRP      LAST FOR REQUEST                              
         B     DISRX                                                            
*                                                                               
DISR11   LA    R2,APRECKEY                                                      
         USING APRECD,R2           LIST                                         
*                                                                               
         BAS   RE,SETBLKL          SET BLOCK LABEL                              
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         LA    RE,BLKLBL                                                        
         ST    RE,TSAREC          GET THE APPROPRIATE RECORD                    
         MVI   TSACTN,TSARDH      FOR DISPLAY                                   
         GOTO1 VTSAR              AND IT MUST                                   
         BE    DISR50             BE THERE                                      
         DC    H'0'                                                             
DISR50   BAS   RE,DSPMNTH         DISPLAY MONTH/WEEK AND AMOUNTS                
DISRX    B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
*              ROUTINE TO SET LIST INDICATORS                                   
SETLST   NTR1                                                                   
         LA    R1,APMSWP          TELL GENERAL TO LOAD                          
         STC   R1,APMODE          LIST OVERLAY AND SCREEN                       
         LA    R1,RECFIN                                                        
         STC   R1,APPARM                                                        
         LA    R1,ACTLIS                                                        
         STC   R1,APPARM+1                                                      
         B     EXIT                                                             
         SPACE                                                                  
*              ROUTINE TO SET LIST INDICATORS                                   
SETDSP   NTR1                                                                   
         LA    R1,RECFIN                                                        
         STC   R1,APPARM                                                        
         LA    R1,ACTDRP                                                        
         STC   R1,APPARM+1                                                      
         MVI   APMODE,APMSWP                                                    
         NI    TWAMODE,X'FF'-TWAMLSM                                            
         NI    TWAMODE,X'FF'-TWAMSEL                                            
         B     EXIT                                                             
         SPACE                                                                  
*              ROUTINE TO SET BLKLBL                                            
SETBLKL  NTR1                                                                   
         XC    BLKLBL,BLKLBL                                                    
         CLI   LISTYPE,C'P'                                                     
         BNE   *+12                                                             
         BAS   RE,SETBPRD         SET FOR PRODUCT                               
         B     SETBLKLX                                                         
*                                                                               
         CLI   LISTYPE,C'E'                                                     
         BNE   *+12                                                             
         BAS   RE,SETBEST          SET FOR ESTIMATE                             
         B     SETBLKLX                                                         
*                                                                               
         BAS   RE,SETBMOS          SET FOR MARKET/STATION                       
SETBLKLX B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO SET PRODUCT IN BLKLBL                                 
SETBPRD  NTR1                                                                   
         LA    RE,BLKLBL                                                        
         CLC   =C'ALL',APPRD      TOTAL LINE?                                   
         BNE   *+14                                                             
         MVC   0(4,RE),=C'FZZZ'                                                 
         B     SETBPRDX                                                         
*                                                                               
         MVI   0(RE),C'A'                                                       
         CLC   =C'UNA',APPRD                                                    
         BNE   *+8                                                              
         MVI   0(RE),C'C'                                                       
         MVC   1(L'APPRD,RE),APPRD                                              
         MVC   4(L'APPRD2,RE),APPRD2                                            
SETBPRDX B     EXIT                                                             
         SPACE                                                                  
*              ROUTINE TO SET ESTIMATE IN BLKLBL                                
SETBEST  NTR1                                                                   
         LA    RE,BLKLBL                                                        
         MVI   0(RE),C'A'                                                       
         MVC   1(1,RE),APESTB1                                                  
         CLC   =C'ALL',APEST                                                    
         BE    SETBEST5                                                         
         CLC   =C'NO',APEST                                                     
         BNE   SETBESTX                                                         
SETBEST5 MVI   0(RE),C'F'                                                       
         MVI   1(RE),X'FF'                                                      
SETBESTX B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO SET MARKET/STATION IN BLKLBL                          
SETBMOS  NTR1                                                                   
         LA    RE,BLKLBL                                                        
         CLI   LISTYPE,C'S'        STATION TOTAL                                
         BNE   SETBMOS5                                                         
         MVC   0(L'APSTA,RE),APSTA                                              
         CLC   =C'ALL',APSTA                                                    
         BNE   *+10                                                             
         MVC   0(L'BLKLBL,RE),=9C'9'                                            
         B     SETBMOSX                                                         
*                                                                               
SETBMOS5 CLC   =C'ALL',QMKT                                                     
         BNE   SETBMOS8                 ALL MKT/STA TOTAL                       
         MVC   0(L'QMKT,RE),=4X'FF'                                             
         MVC   4(L'APSTA,RE),=5C'9'                                             
         B     SETBMOSX                                                         
*                                                                               
SETBMOS8 MVC   0(L'QMKT,RE),QMKT        MOVE IN MARKET                          
         OC    0(L'QMKT,RE),0(RE)                                               
         BNZ   *+10                                                             
         MVC   0(L'QMKT,RE),=X'F0F0F0F0'                                        
         MVC   4(L'APSTA,RE),APSTA                                              
         CLC   =C'ALL',APSTA                                                    
         BNE   *+10                                                             
         MVC   4(L'APSTA,RE),=5C'9'                                             
SETBMOSX B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY MONTH/WEEK AND AMOUNTS                        
DSPMNTH  NTR1                                                                   
         LA    R3,DTLIST          DATE LIST                                     
         ICM   R1,15,DSPLIST                                                    
         AR    R3,R1                                                            
         LA    R2,FDTL1H          FIRST PRINT LINE                              
         USING DISD,R2                                                          
*                                                                               
         USING BLKD,R4                                                          
DSPMN10  CLI   0(R3),0            END OF DATE LIST                              
         BE    DSPMN60                                                          
         OI    6(R2),X'80'        TRANSMIT                                      
         LA    R2,8(R2)                                                         
*                                                                               
         TM    INOIND,INOIPRE     PREVIOUS DATA REQUESTED?                      
         BNO   DSPMN20                                                          
         CLI   PREVDATA,0                                                       
         BNE   DSPMN20                                                          
         MVI   PREVDATA,1         ALREADY DID PREV                              
         MVC   DMONTH,SPACES                                                    
         MVC   DMONTH(4),=C'PREV'                                               
         LA    R4,BLK                                                           
         AH    R4,=Y(BLKBEF-BLKD) POINT TO PREVIOUS VALUES                      
         BAS   RE,PRTLIN                                                        
         BCTR  R9,0               DECREMENT LINES TO PRINT                      
         LA    R2,77(R2)          POINT TO NEXT LINE                            
         OI    6(R2),X'80'        XMIT                                          
         LA    R2,8(R2)                                                         
*                                                                               
DSPMN20  TM    INOIND,INOIWK      IF WEEKLY OR                                  
         BO    DSPMN30                                                          
         TM    INOIND,INOIBRD     IGNORE PROFILE                                
         BO    DSPMN40            YES - DISPLAY MONTH/YEAR                      
         CLI   SVPROF+2,0         NO   -IF PROFILE NOT ZERO                     
         BE    DSPMN40                                                          
DSPMN30  XC    DMONTH,DMONTH      THEN DISPLAY MONTH AND DAY                    
         GOTO1 VDATCON,APPARM,(2,0(R3)),(4,DMONTH)                              
         B     DSPMN50                                                          
DSPMN40  GOTO1 VDATCON,APPARM,(2,0(R3)),(6,DMONTH)                              
*                                                                               
DSPMN50  LA    R4,BLK                                                           
         AH    R4,=Y(BLKCUM-BLKD)   PT TO CUMS **NEW                            
         LA    R1,DTLIST                                                        
         LR    RE,R3              DON'T TOUCH R3 HERE                           
         SR    RE,R1              GET DIFFERENCE                                
         MH    RE,=H'3'            MULTIPLY BY 3                                
         AR    R4,RE              PT TO CORRECT MONTH IN BLKCUM                 
         BAS   RE,PRTLIN                                                        
         LA    R2,77(R2)          POINT TO NEXT LINE                            
         LA    R3,2(R3)           NEXT DATE                                     
         BAS   RE,CHKEND                                                        
         BE    DSPMN80            END OF SCREEN                                 
         B     DSPMN10                                                          
*                                                                               
DSPMN60  TM    INOIND,INOIPOS     POST DATA REQUESTED?                          
         BNO   DSPMN70                                                          
         OI    6(R2),X'80'        TRANSMIT                                      
         LA    R2,8(R2)                                                         
         MVC   DMONTH,SPACES                                                    
         MVC   DMONTH(4),=C'POST'                                               
         LA    R4,BLK                                                           
         AH    R4,=Y(BLKAFT-BLKD) POINT TO POST VALUES                          
         BAS   RE,PRTLIN                                                        
         LA    R2,77(R2)          POINT TO NEXT LINE                            
         BAS   RE,CHKEND                                                        
         BE    DSPMN80            END OF SCREEN                                 
*                                                                               
DSPMN70  OI    6(R2),X'80'        XMIT                                          
         LA    R2,8(R2)                                                         
         MVC   DMONTH,SPACES                                                    
         MVC   DMONTH(5),=C'TOTAL'                                              
         LA    R4,BLK                                                           
         AH    R4,=Y(BLKTOT-BLKD) POINT TO TOTAL VALUES                         
         BAS   RE,PRTLIN          PRINT TOTAL                                   
         LA    R2,77(R2)          CLEAR THE REST OF THE SCREEN                  
         GOTO1 VCLEARF,APPARM,(1,(R2)),FDTL16H                                  
         XC    DSPLIST,DSPLIST    END OF LIST - 0 DISPLACEMENT                  
         TM    TWAMODE,TWAMLSM                                                  
         BNO   *+14                                                             
         MVC   DSPLIST,=F'1'                                                    
         B     *+8                                                              
         MVI   APMODE,APMLRP      LAST FOR REQUEST                              
*                                                                               
DSPMN80  CLC   SVXFRCTL,=C'SPOSDE'  IF NOT SUPERDESK, EXIT                      
         BNE   DSPMNTHX                                                         
*                                                                               
* OTHERWISE, SEND SUPERDESK THE % PAID                                          
         L     RF,ACPARMA                                                       
         L     RF,16(RF)                                                        
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),APPARM,=C'PUTD',PERPDAMT,6,GLVBUY1      % PAID              
*                                                                               
DSPMNTHX B     EXIT                                                             
         SPACE 2                                                                
*================================================*                              
* IF SCREEN FULL DISPLAYED - SAVES DISPLACEMENT  *                              
* INTO DATE LIST FOR NEXT TIME AROUND            *                              
*================================================*                              
*                                                                               
CHKEND   BCTR  R9,0                                                             
         LTR   R9,R9                                                            
         BNZ   CHKNO                                                            
         LA    R1,DTLIST                                                        
         SR    R3,R1              SAVE DISPLACEMENT INTO DTLIST                 
         STCM  R3,15,DSPLIST                                                    
         CR    RB,RB              SET END OF SCREEN CC                          
         B     CHKENDX                                                          
*                                                                               
CHKNO    LTR   RB,RB              SET CC NOT END OF SCREEN                      
CHKENDX  BR    RE                                                               
         EJECT                                                                  
*===========================================*                                   
* PRTLIN - PRINTS ONE LINE ONTO THE SCREEN  *                                   
*===========================================*                                   
*                                                                               
PRTLIN   NTR1                                                                   
         LA    R0,3               NUMBER OF ROWS                                
         LA    R1,AMOUNTS         GET ORD,PAID,BILLED                           
PRT2     ZAP   0(L'AMOUNTS,R1),0(L'AMOUNTS,R4)                                  
         LA    R1,L'AMOUNTS(R1)    NEXT AMOUNT                                  
         ZIC   RF,NUMDTS                                                        
         MH    RF,=H'6'                                                         
         AH    RF,=H'18'                                                        
         AR    R4,RF                                                            
         BCT   R0,PRT2                                                          
*                                 UNPAID=RND(ORD)-RND(PAID)                     
         LA    RF,APWORK                                                        
         ZAP   0(L'ORDAMT,RF),ORDAMT                                            
         SRP   0(L'ORDAMT,RF),64-2,5                                            
         ZAP   UNPDAMT,0(L'ORDAMT,RF)       ROUNDED ORDERED AMT                 
         CP    PAIDAMT,=P'0'                                                    
         BE    PRT3                                                             
         ZAP   0(L'PAIDAMT,RF),PAIDAMT                                          
         SRP   0(L'PAIDAMT,RF),64-2,5                                           
         SP    UNPDAMT,0(L'PAIDAMT,RF)      (RF) = ROUNDED PAID AMOUNT          
*                                                                               
PRT3     CLI   BPRD2,0            DON'T SHOW BILLED OR UNBL FOR                 
         BNE   PRT4               PIGGY BACKS                                   
         OC    INOREP,INOREP      FOR SPECIAL REP                               
         BNZ   PRT4                                                             
         TM    INOIND,INOIWK      AND FOR WEEKLY DISPLAY                        
         BNZ   PRT4                                                             
         TM    INOIND2,INOIBIL    OR IF NOT ASKING FOR BILL AMTS                
         BNO   PRT4                                                             
         CLI   INOSPL,C'N'        OR FOR SPL=NO                                 
         BNE   PRT6                                                             
PRT4     ZAP   UNBLAMT,=P'0'                                                    
         ZAP   BILLAMT,=P'0'                                                    
         B     PRT8                                                             
*                                 UNBL=RND(ORD)-RND(BILLED)                     
PRT6     LA    RF,APWORK                                                        
         ZAP   0(L'ORDAMT,RF),ORDAMT                                            
         SRP   0(L'ORDAMT,RF),64-2,5                                            
         ZAP   UNBLAMT,0(L'ORDAMT,RF)  ROUNDED ORDERED AMT                      
         CP    BILLAMT,=P'0'                                                    
         BE    PRT8                                                             
         ZAP   0(L'BILLAMT,RF),BILLAMT                                          
         SRP   0(L'BILLAMT,RF),64-2,5                                           
         SP    UNBLAMT,0(L'BILLAMT,RF)   (RF) = ROUNDED BILLED AMOUNT           
*                                                                               
*                                 %PAID= ORD/PAID                               
PRT8     ZAP   PERPDAMT,ORDAMT    ZERO IT OUT                                   
         ZAP   ORDAMT,ORDAMT                                                    
         BE    PRT10              CAN'T DIVIDE ZERO                             
*                                                                               
         ZAP   APWORK(12),PAIDAMT                                               
         MP    APWORK(12),=P'10000'                                             
         DP    APWORK(12),ORDAMT                                                
         ZAP   PERPDAMT,APWORK(6)                                               
         SRP   PERPDAMT,64-1,5                                                  
*                                                                               
PRT10    EDIT  PERPDAMT,(5,DPERPD),1,DUB=APDUB,WRK=APWORK                       
         SRP   ORDAMT,64-2,5      DIVIDE BY 100                                 
         EDIT  ORDAMT,(12,DORD),DUB=APDUB,WRK=APWORK,COMMAS=YES,FLOAT=-         
         SRP   PAIDAMT,64-2,5     DIVIDE BY 100                                 
         EDIT  PAIDAMT,(12,DPAID),DUB=APDUB,WRK=APWORK,COMMAS=YES,FLOATX        
               =-                                                               
         EDIT  UNPDAMT,(12,DUNPD),DUB=APDUB,WRK=APWORK,COMMAS=YES,FLOATX        
               =-                                                               
*                                                                               
PRT20    SRP   BILLAMT,64-2,5     DIVIDE BY 100                                 
         EDIT  BILLAMT,(12,DBILL),DUB=APDUB,WRK=APWORK,COMMAS=YES,FLOATX        
               =-                                                               
         EDIT  UNBLAMT,(12,DBILBL),DUB=APDUB,WRK=APWORK,COMMAS=YES,FLOAX        
               T=-                                                              
PRTX     B     EXIT                                                             
         EJECT                                                                  
*=======================================================*                       
* VCLEARF - CLEAR AND FOUT FIELDS                       *                       
*                                                       *                       
* ON ENTRY                                              *                       
*        P1    BYTE 0    = 0 UNPROTECTED FIELDS         *                       
*                        = 1 PROTECTED FIELDS           *                       
*              BYTES 1-3 = A(START FIELD HEADER)        *                       
*        P2    BYTES 1-3 = A(END FIELD HEADER)          *                       
*                                                       *                       
*=======================================================*                       
*                                                                               
VCLEARF  NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         SR    RE,RE                                                            
         LA    R4,X'10'            BRANCH CONDITION                             
         LA    RF,MOVESPA          CLEAR FIELD INSTRUCTION                      
         CLI   0(R1),0             TEST FOR UNPROTECTED FIELDS                  
         BE    *+12                YES                                          
         LA    R4,X'80'            SET BRANCH CONDITION AND CLEAR               
         LA    RF,ZEROFLD          INSTRUCTION FOR PROTECTED FIELDS             
         SPACE 1                                                                
VCLEARF2 IC    RE,0(R2)            LENGTH OF FIELD PLUS HEADER                  
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
VCLEARF4 LA    R2,0(RE,R2)                                                      
         CR    R2,R3               TEST IF END FIELD REACHED                    
         BNH   VCLEARF2            NO-CONTINUE                                  
         XIT1                       YES-ALL DONE                                
         SPACE 1                                                                
MOVESPA  MVC   8(0,R2),SPACES                                                   
ZEROFLD  XC    8(0,R2),8(R2)                                                    
TSTBRAN  BC    0,VCLEARF4                                                       
         EJECT                                                                  
CLRWBLK  NTR1                                                                   
         LA    R1,BLK                                                           
         LA    RE,3                                                             
CLRW5    LA    R0,56                                                            
CLRW10   ZAP   0(L'AMOUNTS,R1),=P'0'                                            
         LA    R1,L'AMOUNTS(R1)                                                 
         BCT   R0,CLRW10                                                        
         BCT   RE,CLRW5                                                         
         XIT1                                                                   
         SPACE 2                                                                
*=================*                                                             
* SPOTIO'S HOOK   *                                                             
*=================*                                                             
*                                                                               
IOHOOK   NTR1                                                                   
         GOTO1 ASPHOOK              ROUTINES IN THE 00                          
         B     EXIT                                                             
         EJECT                                                                  
*=============*                                                                 
* LITERAL POOL*                                                                 
*=============*                                                                 
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
SPACES   DC    CL80' '                                                          
DMREAD   DC    CL7'DMREAD'                                                      
DMWRITE  DC    CL7'DMWRT '                                                      
TEMPSTR  DC    CL7'TEMPSTR'                                                     
*                                                                               
* SPFINWRK                                                                      
       ++INCLUDE SPFINWRK                                                       
*                                                                               
LOCALD   DSECT                                                                  
SWLIST   DS    CL1                                                              
PREVDATA DS    XL1                                                              
*                                                                               
* DISD - DSECT TO COVER DISPLAY SCREEN                                          
*                                                                               
DISD     DSECT                                                                  
DMONTH   DS    CL6                MONTH                                         
         DS    CL1                                                              
DPERPD   DS    CL5                % PAID                                        
         DS    CL1                                                              
DORD     DS    CL12               ORDERED                                       
         DS    CL1                                                              
DPAID    DS    CL12               PAID                                          
         DS    CL1                                                              
DUNPD    DS    CL12               UNPAID                                        
         DS    CL1                                                              
DBILL    DS    CL12               BILLED                                        
         DS    CL1                                                              
DBILBL   DS    CL12               BILLABLE                                      
         DS    CL1                                                              
DISDX    EQU   *                                                                
*                                                                               
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   FINTABH                                                          
       ++INCLUDE SPFINFED                                                       
         SPACE 2                                                                
         ORG                                                                    
         EJECT                                                                  
       ++INCLUDE DDGLVXCTLD                                                     
         EJECT                                                                  
       ++INCLUDE DDGLOBEQUS                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052SPFIN02   11/14/06'                                      
         END                                                                    
